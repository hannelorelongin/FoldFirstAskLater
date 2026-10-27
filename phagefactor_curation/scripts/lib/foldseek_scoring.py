#!/usr/bin/env python3
"""
foldseek_scoring.py
===================
Scoring, filtering, description-upgrade and confidence-tier functions for the
local FoldSeek 3Di search step (02d).

These functions live in this dedicated module so the local-FoldSeek path does not
depend on an earlier 02b_foldseek_pipeline.py (which carried ESMFold + Web
API logic not used here).

Functions exported:
  _is_informative_fs        : description filter (drops 'hypothetical', DUFs, ...)
  _extract_pdb_description  : clean up "Crystal structure of X" PDB titles
  _phage_boost_factor       : composite-score multiplier for ranking
  _is_same_host_hit         : taxname-based same-host flag
  _is_generic_name          : detect orf/Y-gene/UPF-style generic labels
  _top3_agrees              : 2+ of top-3 share the same function
  _upgrade_description      : pick the most informative variant
  _compute_fs_confidence    : CONFIDENT / GOOD / BORDERLINE / WEAK / NO_HIT
  build_best_and_top3       : per-gene best-hit + top-3 frame builders
  _DEFENSE_ANY              : regex (lexicon.DEFENSE_ANY, exported for re-use)
"""

import re
from collections import Counter

try:
    import pandas as pd
except ImportError as e:  # pragma: no cover -- pipeline-level dependency
    raise SystemExit(f"Missing dependency: {e}\n  pip install pandas")

from config import (UNINFORMATIVE_STRINGS, HOST_GENUS,
                    FS_CONFIDENT_EVALUE, FS_CONFIDENT_SCORE, FS_CONFIDENT_TOP3_SCORE,
                    FS_GOOD_EVALUE, FS_GOOD_SCORE, FS_GOOD_TOP3_EVALUE,
                    FS_BORDERLINE_EVALUE, FS_BORDERLINE_SCORE,
                    FS_BORDERLINE_SCOREONLY_EVALUE)

# Single source of truth for description vocabulary (see lexicon.py).
from lexicon import (
    is_informative_desc,
    classify,
    TIER_LOW,
    GENERIC_DESCRIPTORS as _GENERIC_DESCRIPTORS,
    GENE_LABEL as _GENE_LABEL,
    UNCHAR_PATTERN as _UNCHAR_PATTERN,
    GENERIC_DOMAIN as _GENERIC_DOMAIN,
    GENERIC_NAMES as _GENERIC_NAMES,
    PHAGE_SPECIFIC as _PHAGE_SPECIFIC,
    PHAGE_CONTEXT as _PHAGE_CONTEXT,
    GP_WRAPPER as _GP_WRAPPER_MALUS,
    PDB_CRYSTAL_PREFIX as _PDB_CRYSTAL_PREFIX,
    PDB_TRAILING_QUAL as _PDB_TRAILING_QUAL,
    PDB_UNINFORMATIVE as _PDB_UNINFORMATIVE,
    PDB_TITLE as _PDB_TITLE,
    PROMISCUOUS_FOLDS as _PROMISCUOUS_FOLD_PATTERNS,
    DEFENSE_ANY as _DEFENSE_ANY,
    EUKARYOTIC_DESC as _EUKARYOTIC_DESCRIPTION_KEYWORDS,
)

from config import LOW_INFO_MALUS, HOST_BOOST
from rules import clean_name, name_alternatives, same_function, UPGRADE_RULES, _PHAGE_FUNCTION

_GP_ORF_LABEL = re.compile(r"\b(?:gp|orf)\d+\b", re.I)


def _names_known_function(desc: str) -> bool:
    """A domain label that names a phage function ('Integrase catalytic
    domain-containing protein'): not a low-information label in the vote."""
    d = _FRAGMENT_TAG.sub("", str(desc or "")).strip()
    return bool(d) and bool(_PHAGE_SPECIFIC.search(_GP_ORF_LABEL.sub("", d)))


def _renamed_by_rule(desc: str) -> bool:
    """A naming rule (rules.UPGRADE_RULES) renames this label to a function name."""
    return any(top3_pat is None and tmpl and not re.search(r"\\\d", tmpl) and pat.search(desc)
               for pat, top3_pat, tmpl in UPGRADE_RULES)


# =============================================================================
# PROMISCUOUS FOLD DETECTION
# =============================================================================
# Certain structural folds are so widespread across functionally unrelated
# proteins that a FoldSeek hit to them carries little or no functional
# information, especially when (a) the hit is to a eukaryotic entry with no
# phage/prokaryotic support in the top 3, or (b) the top-3 hits span multiple
# unrelated functional families (fold-level promiscuity).
#
# Scientific basis:
#   TLD / Metallo-beta-lactamase (MBL) fold (SCOP 3.60.15.10):
#     Aravind (1999) In Silico Biology 1:21-31 (PMID 11471255); Daiyasu et al.
#     (2001) FEBS Lett 503:1-6 (PMID 11513844).  The αβ/βα MBL scaffold is
#     shared by class-A/B/C/D beta-lactamases, glyoxalase II, CPSF-73/100 RNA
#     cleavage factors, SNM1/Artemis DNA-repair nucleases, tRNase Z, and many
#     uncharacterised DUFs.  FoldSeek returns "beta-lactamase" for ANY protein
#     bearing this fold, regardless of catalytic function.
#
#   Eukaryotic motor/cytoskeletal proteins (kinesin, myosin, tubulin, actin):
#     Their folds (P-loop NTPase, TED, WD40) are abundant in bacteria and
#     archaea but the eukaryote-specific proteins themselves are never genuine
#     phage hits; they appear because pdb100 / afdb-swissprot contain many
#     high-resolution eukaryotic motor crystal structures.
#
#   Note: Rossmann fold, TIM barrel, OB-fold, jelly-roll etc. are also
#   highly promiscuous but are intentionally NOT listed here because they can
#   yield genuine and important phage functional annotations (e.g. jelly-roll
#   capsid proteins, Rossmann-fold terminase ATPase domains).  Only folds whose
#   phage hits are nearly always false positives are included.
#
# Action: hits matching these patterns are flagged (promiscuous_fold_flag) and a
# best hit's confidence is capped at BORDERLINE, so it never names a gene on its
# own; it stays in the table as evidence. Not a rejection: true positives exist
# (e.g. a real MBL-superfamily RNA-repair enzyme in a moron module).


# Eukaryotic description keywords (lexicon.EUKARYOTIC_DESC): unambiguous
# eukaryote-specific terms in the hit description, a signal independent of the
# hit's taxonomy.


def _is_promiscuous_fold_hit(description: str) -> bool:
    """
    Return True iff the description matches a known structurally promiscuous
    fold whose FoldSeek hits are frequently false positives in phage context.
    See PROMISCUOUS_FOLD_PATTERNS above for references and rationale.
    """
    if not description or not isinstance(description, str):
        return False
    return bool(_PROMISCUOUS_FOLD_PATTERNS.search(description))


def _has_eukaryotic_description(description: str) -> bool:
    """
    Return True iff the description text contains unambiguous evidence of a
    eukaryote-specific protein, used when taxonomy is not embedded in the DB
    (afdb-swissprot, pdb100).  Conservative: only fires on explicit organism
    names or unambiguously eukaryotic protein classes.
    """
    if not description or not isinstance(description, str):
        return False
    return bool(_EUKARYOTIC_DESCRIPTION_KEYWORDS.search(description))


# -----------------------------------------------------------------------------
# PDB DESCRIPTION EXTRACTION
# -----------------------------------------------------------------------------
# PDB titles follow "Crystal structure of X [from/in/at/with ...]" where
# "X" is often the biologically relevant protein name.  We try to extract X
# and return a cleaner label.  If the result is still uninformative (ORF-style,
# engineered/synthetic construct, etc.) we return None so the caller can treat
# the hit as uninformative.


# Trailing qualifiers to strip once we have the protein name

# PDB titles that are uninformative even after extraction


def _extract_pdb_description(description: str) -> str:
    """
    Given a raw PDB hit description (often a crystal structure title), return a
    cleaned protein name, or the original string if no cleaning is needed.

    Rules:
      1. Strip "Crystal structure of [a/an/the]" prefix.
      2. Strip trailing qualifiers ("from X", "in complex with Y", "at 2.2 A", ...).
      3. Strip mutation/condition notes in parentheses at the end.
      4. If the result is an ORF-style label, DARPin, SeMet apo, etc. → return ""
         so the caller treats it as uninformative.
      5. If no "Crystal structure" prefix was present, return the original unchanged.
    """
    if not description or not isinstance(description, str):
        return description or ""

    desc = description.strip()

    # Only process if it looks like a crystal structure title
    if not _PDB_CRYSTAL_PREFIX.match(desc):
        return desc

    # Step 1: strip prefix
    core = _PDB_CRYSTAL_PREFIX.sub("", desc).strip()

    # Step 2: strip trailing qualifiers (from / in complex with / at N.N A / with ...)
    core = _PDB_TRAILING_QUAL.sub("", core).strip()

    # Step 3: strip trailing parenthetical mutation/condition notes, e.g. "(Y211A)"
    core = re.sub(r'\s*\([A-Z]\d+[A-Z]\)\s*$', '', core).strip()
    core = re.sub(r'\s*\(P\d+\)\s*$', '', core).strip()   # e.g. "(P41)"

    # Step 4: uninformative after extraction?
    if not core or _PDB_UNINFORMATIVE.match(core):
        return ""

    return core


# -----------------------------------------------------------------------------
# DESCRIPTION INFORMATIVENESS
# -----------------------------------------------------------------------------



def _is_informative_fs(description: str) -> bool:
    """Return True iff a FoldSeek description carries real functional info.

    Delegates to lexicon.is_informative_desc so that the informativeness RULES
    have exactly one home. Proven equivalent to the previous inline
    implementation across 14,652 real descriptions before the swap; keeping a
    second copy here is how the two drifted apart last time.
    """
    return is_informative_desc(description)


# -----------------------------------------------------------------------------
# PHAGE-CONTEXT COMPOSITE SCORING
# -----------------------------------------------------------------------------




# _DEFENSE_ANY is lexicon.DEFENSE_ANY (defense systems + toxin-antitoxin), imported above.


# bare gp / phage-protein / Mu-like wrapper hits -> demoted in the top-3 ranking


def _phage_boost_factor(desc: str) -> float:
    """Composite-score multiplier (named phage protein x2, viral context x1.5,
    generic 'family/domain-containing' suffix x LOW_INFO_MALUS (0.60) penalty, gp/wrapper x0.40 malus).

    The low-information penalty also covers GENE-LABEL style names (UPF1234 protein,
    YabC protein, orfNN, 'Prophage ..., Orf12'): these carry as little functional
    information as a bare 'X family protein', so they should not out-rank a named
    function at equal bitscore."""
    if not desc or not isinstance(desc, str):
        return 1.0
    boost = 1.0
    if _PHAGE_SPECIFIC.search(desc):
        boost *= 2.00
    elif _PHAGE_CONTEXT.search(desc):
        boost *= 1.50
    desc_for_generic = re.sub(r'\s*\(fragment\)\s*$', '', desc.strip(), flags=re.IGNORECASE)
    # a defense domain label that a naming rule turns into a named protein
    # ('SMODS-associating 2TM ... domain-containing protein' -> SLATT effector)
    # names a function: no low-information malus
    if ((_GENERIC_DOMAIN.search(desc_for_generic) or _GENERIC_NAMES.search(desc_for_generic))
            and not (_DEFENSE_ANY.search(desc_for_generic) and _renamed_by_rule(desc_for_generic))):
        boost *= LOW_INFO_MALUS     # config foldseek.low_info_malus (default 0.60)
    # gp/wrapper MALUS -- a bare "gpNN" / "phage protein" / "Mu-like
    # prophage protein" carries no function; demote it so genuinely informative
    # hits surface in the per-gene top-3 (gp hits crowd out useful ones).
    if _GP_WRAPPER_MALUS.search(desc.strip()):
        boost *= 0.40
    return boost


def _host_boost_factor(desc: str, same_host) -> float:
    """Host-aware RANKING tie-breaker (default x1.20, config: foldseek.host_boost).

    Deliberately small — much smaller than the phage-specific x2.0 — because the
    host signal is noisier: the structure DBs are dominated by a few model hosts,
    so a large multiplier would mostly amplify DATABASE COMPOSITION BIAS rather
    than real host-derived morons/AMGs.

    Two guards keep it honest:
      1. only INFORMATIVE, NON-PROMISCUOUS descriptions are boosted, so it can
         never float host junk ('hypothetical protein' from the host) to the top;
      2. it feeds composite_score ONLY. _compute_fs_confidence stays on raw
         evalue/score, so the boost can reorder hits WITHIN a confidence tier but
         can never promote one across a tier boundary.

    Inert when: host_boost == 1.0, no HOST_GENUS configured, or the hit table has
    no taxonomy (same_host is then None/False).
    """
    if not same_host or HOST_BOOST == 1.0:
        return 1.0
    if not _is_informative_fs(desc):
        return 1.0
    if _is_promiscuous_fold_hit(desc):
        return 1.0
    return HOST_BOOST


def _is_same_host_hit(taxname: str):
    """Same-host-genus flag. Returns None (-> NA) when no host genus is
    configured, so the column is NA rather than a misleading False and the
    host-moron gate does not fire. Otherwise True/False vs HOST_GENUS."""
    if not HOST_GENUS:
        return None
    if not taxname or not isinstance(taxname, str):
        return False
    return HOST_GENUS.lower() in taxname.lower()


# -----------------------------------------------------------------------------
# GENERIC-NAME DETECTION & DESCRIPTION UPGRADE
# -----------------------------------------------------------------------------




def _is_generic_name(desc: str) -> bool:
    if not desc or not isinstance(desc, str):
        return True
    return bool(_GENERIC_NAMES.search(desc.strip()))


def _top3_agrees(descs: list, anchor=None) -> bool:
    """Return True iff 2+ descriptions refer to the same function (Fragment-aware).

    With `anchor` (the best hit's name and/or the upgraded name the gene would
    get; a string or a list), one of the agreeing pair must be an anchor: two
    hits that agree only with each other do not corroborate the best hit."""
    def _n(d):
        return re.sub(r'\s*\(fragment\)\s*$', '', d.strip().lower())
    def _same(a, b):
        return a == b or a.startswith(b + " ") or b.startswith(a + " ")
    normed = [_n(d) for d in descs if d and isinstance(d, str)]
    if len(normed) < 2:
        return False
    anchors = [anchor] if isinstance(anchor, str) else list(anchor or [])
    anchors = [_n(a) for a in anchors if a and isinstance(a, str)]
    if anchors:
        # an anchor is normally one of the top-3 itself: it needs one MORE match
        # anchor matches also count rules.same_function ('DNA-binding protein'
        # backs 'DNA binding domain protein, excisionase family')
        return any(sum(_same(a, b) or same_function(a, b) for b in normed)
                   >= (2 if a in normed else 1) for a in anchors)
    for i in range(len(normed)):
        for j in range(i + 1, len(normed)):
            if _same(normed[i], normed[j]):
                return True
    return False


_TITLE_TAIL = re.compile(r"\b(variant|mutant)\b.*\b(at|in|with)\b|"
                         r"\bat\s+(cryogenic|room)\s+temperature\b|"
                         r"\bin\s+complex\s+with\b", re.I)


def _reads_like_title(desc: str, strict: bool = False) -> bool:
    """A structure/paper TITLE rather than a protein name: a tell-tale phrase
    (lexicon.PDB_TITLE), a variant/condition tail ('... variant Delta+PHS L36R
    at cryogenic temperature'), or simply >= 8 words. strict=True ignores the
    word count (phrase evidence only). Shared with step 04."""
    if not desc or not isinstance(desc, str):
        return False
    d = desc.strip()
    phrase = bool(_PDB_TITLE.search(d)) or bool(_TITLE_TAIL.search(d))
    return phrase if strict else (phrase or len(d.split()) >= 8)


def _is_low_information(desc: str) -> bool:
    """Lexicon tier 1: a domain/family/repeat label, gene label, ORF or wrapper."""
    return classify(desc)[0] == TIER_LOW


def _low_info(desc: str) -> bool:
    """Low-information label that does not name a known function."""
    return _is_low_information(desc) and not _names_known_function(desc)


_TIER_RANK = {"NO_HIT": 0, "WEAK": 1, "BORDERLINE": 2, "GOOD": 3, "CONFIDENT": 4}
_FRAGMENT_TAG = re.compile(r"\s*\(fragment\)\s*$", re.I)


def _preferred_name(best_desc: str, cand: str) -> str:
    """Two names for the SAME function (rules.same_function): the cleaned,
    unambiguous ('A/B' is ambiguous), non-bare-family, most specific one.
    A tie goes to the majority (cand)."""
    def key(d):
        alts = name_alternatives(d)
        bare_family = bool(re.fullmatch(r"\S+\s+family", clean_name(d), re.I))
        return (len(alts) == 1, not bare_family, max((len(a) for a in alts), default=0))
    return clean_name(best_desc) if key(best_desc) > key(cand) else clean_name(cand)


def _vote_candidate_ok(best_desc: str, cand: str) -> bool:
    """Gates every majority candidate must pass, whatever happens next: never a
    title, a eukaryote-specific name, a promiscuous fold, a generic descriptor,
    or a low-information label replacing an informative name."""
    n = _FRAGMENT_TAG.sub("", str(cand)).strip().lower()
    if (_reads_like_title(cand) or _has_eukaryotic_description(cand)
            or _is_promiscuous_fold_hit(cand) or n in _GENERIC_DESCRIPTORS):
        return False
    return not _is_low_information(cand) or _low_info(best_desc)


def _majority_may_replace(best_desc: str, cand: str,
                          best_score=None, cand_score=None,
                          best_tier=None, cand_tier=None) -> bool:
    """May the top-3 majority description `cand` replace the best hit's own
    description?

    Only the DESCRIPTION is swapped (accession, taxon, kingdom, e-value and
    score stay those of hit #1), so the vote may confirm or sharpen hit #1 but
    must not override stronger evidence:
      * an uninformative best hit or a generic gene label (lexicon.GENERIC_NAMES:
        orf12, UPF0123 protein, y-genes) can always be replaced;
      * a title, a eukaryote-specific name, a promiscuous fold, a generic
        descriptor (lexicon.GENERIC_DESCRIPTORS: 'Transcriptional regulator',
        'DNA-binding protein') or a
        low-information label (domain/family suffix, gene label, wrapper) never
        replaces an informative, named best hit;
      * a eukaryote-specific or low-information best hit is replaced by a
        majority that names a function;
      * a generic-descriptor best hit ('DNA-binding protein', 'Transcriptional
        regulator') is replaced by a majority that names a more specific
        function, provided the majority's own FoldSeek confidence tier is at
        least that of hit #1 (its score may be lower: 'DNA-binding protein'
        576 CONFIDENT vs 'Phage tail tube protein' 351 x2 CONFIDENT -> tail tube).
        Without tiers, the more informative name wins;
      * otherwise the majority must have the stronger alignment: its best raw
        FoldSeek score >= that of hit #1. The phage boost decides the ranking
        among comparable hits, not whether two hits outvote a stronger one
        ('Phage tail sheath' 2326 x2 vs 'tape measure' 1898 -> sheath;
        'Actin filament-associated' 150 x2 vs 'Capsid protein' 175 -> capsid).
        Without scores, falls back to the boost (cand must not rank lower).
    """
    if not best_desc or not _is_informative_fs(best_desc) or _is_generic_name(best_desc):
        return True
    norm = lambda d: re.sub(r'\s*\(fragment\)\s*$', '', str(d).lower().strip())
    if norm(best_desc) == norm(cand):
        return True
    if (_reads_like_title(cand) or _has_eukaryotic_description(cand)
            or _is_promiscuous_fold_hit(cand)
            or norm(cand) in _GENERIC_DESCRIPTORS):
        return False
    if _is_low_information(cand) != _low_info(best_desc):
        return _low_info(best_desc)
    if _has_eukaryotic_description(best_desc):
        return True
    if norm(best_desc) in _GENERIC_DESCRIPTORS:
        # cand is informative, named, not low-information and not itself a
        # generic descriptor (checked above): more specific than hit #1.
        if best_tier is not None and cand_tier is not None:
            return _TIER_RANK.get(cand_tier, 0) >= _TIER_RANK.get(best_tier, 0)
        return True
    if best_score is not None and cand_score is not None:
        # A majority without any phage context does not outvote a phage-boosted
        # hit #1 on raw score: it must win on the composite (score x boost) that
        # ranked hit #1 first ('Terminase' 250 x2 vs 'DNA primase' 295 x2).
        # The boost must come from a phage FUNCTION, not from a bare gpNN/orfNN
        # label ('IS66 family element, orf1' is not protected against 'Transposase' x2).
        b_boost, c_boost = _phage_boost_factor(best_desc), _phage_boost_factor(cand)
        # 'Conserved phage C-terminus' says 'phage' but names no function
        names_phage_function = bool(_PHAGE_FUNCTION.search(_GP_ORF_LABEL.sub("", best_desc)))
        if c_boost <= 1.0 < b_boost and names_phage_function:
            return cand_score * c_boost >= best_score * b_boost
        return cand_score >= best_score
    return _phage_boost_factor(cand) >= _phage_boost_factor(best_desc)


def _upgrade_description(best_desc: str, top3_descs: list,
                         best_score=None, top3_scores=None,
                         best_evalue=None, top3_evalues=None) -> str:
    """Pick the most informative annotation from best_desc + top3.

    PDB crystal-structure titles are cleaned up before ranking:
    "Crystal structure of the bacteriophage Mu transpososome" becomes
    "bacteriophage Mu transpososome" which then gets phage-boost scoring.
    Uninformative PDB entries (ORF-style, DARPin, SeMet apo, etc.) are dropped.
    """
    def _clean(d):
        if not d or not isinstance(d, str):
            return d
        return _extract_pdb_description(d) if _PDB_CRYSTAL_PREFIX.match(d) else d

    best_desc = _clean(best_desc) or ""
    top3_descs = [_clean(d) for d in (top3_descs or [])]
    if top3_scores is None or len(top3_scores) != len(top3_descs):
        top3_scores = [None] * len(top3_descs)
    if top3_evalues is None or len(top3_evalues) != len(top3_descs):
        top3_evalues = [None] * len(top3_descs)

    all_descs = ([best_desc] if best_desc else []) + top3_descs
    informative = [d for d in all_descs if isinstance(d, str) and _is_informative_fs(d)]
    if not informative:
        return best_desc or ""

    non_generic = [d for d in informative if not _is_generic_name(d)]

    # Rule 1 - majority vote over top-3
    top3_non_generic = [(d, s, e) for d, s, e in zip(top3_descs, top3_scores, top3_evalues)
                        if isinstance(d, str) and _is_informative_fs(d) and not _is_generic_name(d)]
    if top3_non_generic:
        _n = lambda d: re.sub(r'\s*\(fragment\)\s*$', '', str(d).lower().strip())
        norm_map: dict = {}
        for d, s, e in top3_non_generic:
            norm_map.setdefault(_n(d), []).append((d, s, e))
        counts = Counter({k: len(v) for k, v in norm_map.items()})
        top_norm, top_count = counts.most_common(1)[0]
        if top_count >= 2:
            group = norm_map[top_norm]
            # never a '(Fragment)' variant as the name; then the longest
            cand = max((d for d, _, _ in group), key=lambda d: (not _FRAGMENT_TAG.search(d), len(d)))
            scores = [s for _, s, _ in group if s is not None]
            cand_score = max(scores) if scores else None
            # Confidence tiers on the same scale as foldseek_confidence. The
            # majority gets top-3 agreement credit (it IS a repeated name);
            # hit #1 only if its own name recurs in the top-3.
            best_tier = cand_tier = None
            if best_score is not None or best_evalue is not None:
                best_rep = [d for d in top3_descs if _n(d) == _n(best_desc)] or [best_desc]
                best_tier = _compute_fs_confidence(best_evalue, best_score, best_rep, True)
                cand_tier = max((_compute_fs_confidence(e, s, [cand, cand], True)
                                 for _, s, e in group),
                                key=lambda t: _TIER_RANK.get(t, 0))
            # Same function, different wording ('Phage portal protein, HK97 family'
            # vs 'Phage portal protein' x2): a naming choice, not competing
            # evidence -> the cleanest, most specific name, whatever the scores.
            if (best_desc and _is_informative_fs(best_desc) and not _is_generic_name(best_desc)
                    and _vote_candidate_ok(best_desc, cand) and same_function(best_desc, cand)):
                return _preferred_name(best_desc, cand)
            # Different functions: the vote may replace hit #1 only on equal or
            # stronger evidence (see _majority_may_replace).
            if _majority_may_replace(best_desc, cand, best_score, cand_score,
                                     best_tier, cand_tier):
                return clean_name(cand)

    # Rule 2 - generic best_desc -> swap with best non-generic top-3
    if _is_generic_name(best_desc) and non_generic:
        # a named phage protein first, else the strongest hit (list order); a bare
        # viral context word does not count ('viral Zalpha domain' from a PDB title
        # is not preferred over 'HTH-type transcriptional regulator GolR')
        best_non_generic = max(enumerate(non_generic), key=lambda x: (
            bool(_PHAGE_SPECIFIC.search(x[1])), -x[0]))[1]
        if best_desc and _GENE_LABEL.match(best_desc.strip()):
            return f"{best_desc.strip()} ({best_non_generic})"
        return best_non_generic

    # Rule 3 - best_desc is a prefix of a more specific top-3 entry
    if best_desc and not _is_generic_name(best_desc):
        bl = best_desc.lower().strip()
        for d in informative[1:]:
            dl = d.lower().strip()
            if dl != bl and dl.startswith(bl + " "):
                return clean_name(d)

    # hit #1 kept: same clean-up as a vote result ('Putative ...', phage-lineage
    # family suffix, organism prefix), so its wording is not treated differently
    return clean_name(best_desc)


def _compute_fs_confidence(evalue, score, top3_inf_descs: list,
                           has_any_hit: bool, anchor: str = None) -> str:
    """Rate the FoldSeek structural-match quality (CONFIDENT/GOOD/BORDERLINE/WEAK/NO_HIT).

    CONFIDENT requires EITHER a strong evalue (<=1e-3 + score>=200) OR top3
    agreement.  A high score alone (sc >= 300) without top3 agreement is
    downgraded to BORDERLINE: this prevents promiscuous folds (e.g. TLD/MBL,
    motor proteins) from reaching CONFIDENT solely because their fold is common
    enough to yield a high structural-alignment score against many proteins.
    """
    import math

    def _safe(v):
        try:
            f = float(v)
            return None if (math.isnan(f) or math.isinf(f)) else f
        except (TypeError, ValueError):
            return None

    if not has_any_hit:
        return "NO_HIT"

    ev = _safe(evalue)
    sc = _safe(score) or 0.0

    inf_descs = [d for d in (top3_inf_descs or [])
                 if _is_informative_fs(str(d) if d else "")]
    agrees = _top3_agrees(inf_descs, anchor)

    # CONFIDENT: requires strong evalue+score, OR top3 agreement at threshold.
    # A high score alone is not CONFIDENT: high structural-alignment scores are
    # common for promiscuous folds (see _PROMISCUOUS_FOLD_PATTERNS).
    if ev is not None and ev <= FS_CONFIDENT_EVALUE and sc >= FS_CONFIDENT_SCORE:
        return "CONFIDENT"
    if agrees and ((ev is not None and ev <= FS_CONFIDENT_EVALUE) or sc >= FS_CONFIDENT_TOP3_SCORE):
        return "CONFIDENT"

    if (ev is not None and ev <= FS_GOOD_EVALUE) or sc >= FS_GOOD_SCORE:
        return "GOOD"
    if agrees and ev is not None and ev <= FS_GOOD_TOP3_EVALUE:
        return "GOOD"

    if agrees and ((ev is not None and ev <= FS_BORDERLINE_EVALUE) or sc >= FS_BORDERLINE_SCORE):
        return "BORDERLINE"
    if ev is not None and ev <= FS_BORDERLINE_EVALUE:
        return "BORDERLINE"
    if sc >= FS_BORDERLINE_SCORE and (ev is None or ev <= FS_BORDERLINE_SCOREONLY_EVALUE):
        return "BORDERLINE"

    return "WEAK"


_TIER_RANK = {"CONFIDENT": 3, "GOOD": 2, "BORDERLINE": 1, "WEAK": 0, "NO_HIT": 0}


def _phage_part(desc: str) -> float:
    """The phage-boost part (x2 / x1.5) of _phage_boost_factor, without the malus."""
    if not desc or not isinstance(desc, str):
        return 1.0
    if _PHAGE_SPECIFIC.search(desc):
        return 2.00
    if _PHAGE_CONTEXT.search(desc):
        return 1.50
    return 1.0


def gate_phage_boost(hits: pd.DataFrame, top_n: int = 20) -> pd.DataFrame:
    """The phage boost may not lift a BORDERLINE/WEAK hit above a GOOD/CONFIDENT one
    ('Phage portal protein' 159, e 0.1 over 'AntA/AntB antirepressor' 229, e 1e-3).

    Each hit gets its own tier from its e-value and score; a hit backed by another
    hit of the same function among the gene's top_n counts as top-3 agreement, so a
    small phage protein seen twice ('Phage holin' x2, e 0.02) keeps its boost. A
    bitscore cut-off would not do: holins and tail tubes are small and score low.

    Eukaryotic hits (column `kingdom`, lib/taxonomy.py) are not phage proteins:
    they get no phage boost, and they never rank above a GOOD+ bacterial/archaeal/
    viral hit of the same gene (their composite_score is capped just below it).

    Expects composite_score/phage_boost/informative columns; recomputes
    composite_score and returns the frame re-sorted like the callers."""
    hits = hits.copy()
    king = hits["kingdom"] if "kingdom" in hits.columns else pd.Series("", index=hits.index)
    # 1. no phage boost for a eukaryotic hit
    for idx in hits.index[king == "Eukaryote"]:
        part = _phage_part(hits.at[idx, "description"])
        if part > 1.0:
            hits.at[idx, "phage_boost"] = hits.at[idx, "phage_boost"] / part
            hits.at[idx, "composite_score"] = hits.at[idx, "composite_score"] / part
    inf = hits[hits["informative"]].sort_values(["gene", "composite_score"],
                                                ascending=[True, False])
    inf = inf.groupby("gene", sort=False).head(top_n)
    gated, capped = {}, []
    for _, grp in inf.groupby("gene", sort=False):
        descs = list(grp["description"])
        tiers = []
        for i, (ev, sc, d) in enumerate(zip(grp["evalue"], grp["score"], descs)):
            agrees = any(j != i and same_function(o, d) for j, o in enumerate(descs))
            # [d, d] = 'another hit names this function' for _compute_fs_confidence
            tiers.append(_TIER_RANK[_compute_fs_confidence(
                ev, sc, [d, d] if agrees else [d], True)])
        if max(tiers) >= _TIER_RANK["GOOD"]:
            # 2. the boost does not lift a BORDERLINE/WEAK hit over a GOOD+ one
            for idx, t, d in zip(grp.index, tiers, descs):
                if t < _TIER_RANK["GOOD"] and _phage_part(d) > 1.0 \
                        and king.get(idx, "") != "Eukaryote":   # already unboosted
                    gated[idx] = _phage_part(d)
        # 3. a eukaryotic hit ranks below the best GOOD+ prokaryotic/viral hit
        good_prok = [hits.at[idx, "composite_score"] / gated.get(idx, 1.0)
                     for idx, t in zip(grp.index, tiers)
                     if t >= _TIER_RANK["GOOD"] and king.get(idx, "") in ("Bacteria", "Archaea", "Virus")]
        if good_prok:
            floor = max(good_prok)
            for idx in grp.index:
                if king.get(idx, "") == "Eukaryote" and hits.at[idx, "composite_score"] >= floor:
                    capped.append((idx, floor * 0.999))
    for idx, part in gated.items():
        hits.at[idx, "phage_boost"] = hits.at[idx, "phage_boost"] / part
        hits.at[idx, "composite_score"] = hits.at[idx, "composite_score"] / part
    for idx, val in capped:
        hits.at[idx, "composite_score"] = val
    return hits.sort_values(["gene", "composite_score", "evalue"],
                            ascending=[True, False, True])


# =============================================================================
# BEST-HIT + TOP-3 BUILDERS
# =============================================================================

def build_best_and_top3(hits: pd.DataFrame, all_genes: list) -> tuple:
    """
    From sorted hits DataFrame, build:
      best_df : one row per gene -- best informative hit (fall back to best overall)
                upgraded description + foldseek_confidence column added
      top3_df : top-3 informative hits per gene
    """
    def _best_hit(group):
        inf = group[group["informative"]]
        return inf.iloc[0] if len(inf) > 0 else group.iloc[0]

    best = (
        hits
        .groupby("gene", sort=False)
        .apply(_best_hit, include_groups=False)
        .reset_index(level="gene")
        .reset_index(drop=True)
    )
    # Keep aa_length when present so qcov_frac can be computed downstream.
    # Also keep foldseek_subdb: which DB the winning hit came from. Without it
    # the fs_db column is empty all the way to final_annotations_table.csv, so
    # there is no way to tell whether a call came from pdb100 / afdb50 /
    # afdb-swissprot — i.e. no way to judge what each database contributes.
    _extra_cols = [c for c in ["aa_length", "foldseek_subdb", "kingdom", "kingdom_source"]
                   if c in best.columns]
    best = best[["gene", "accession", "description", "taxname",
                 "pident", "evalue", "score", "composite_score",
                 "qcov_aa", "same_host", "defense_flag", "informative"] + _extra_cols]
    best = best.rename(columns={"informative": "informative_hit_found"})

    # When a gene's best hit is not eukaryotic, its eukaryotic hits leave the
    # top-3 (they stay in all_hits.csv), so they cannot rename it by majority
    # vote or lend it top-3 agreement. Genes whose best hit is eukaryotic keep
    # the mixed top-3 (step 03 demotes them).
    if "kingdom" in hits.columns:
        _euk_free = set(best.loc[best["kingdom"] != "Eukaryote", "gene"])
        hits = hits[~(hits["gene"].isin(_euk_free) & (hits["kingdom"] == "Eukaryote"))]

    # Per-gene top-3 informative descriptions
    # Per-gene top-3 informative (description, raw score, e-value)
    top3_map: dict = {}
    for gene, grp in hits[hits["informative"]].groupby("gene", sort=False):
        g3 = grp.head(3)
        top3_map[gene] = list(zip(g3["description"].fillna(""), g3["score"], g3["evalue"]))

    # Phage rescue: inject specific-phage hit into top-3 if missing
    rescue_pool = hits[(hits["score"] > 500) & (hits["evalue"] < 1e-7)
                       & hits["description"].fillna("").str.contains(_PHAGE_SPECIFIC)]
    for gene, grp in rescue_pool.groupby("gene", sort=False):
        top3 = top3_map.get(gene, [])
        if any(_PHAGE_SPECIFIC.search(str(d)) for d, _, _ in top3 if d):
            continue
        rescue = (grp.iloc[0]["description"], grp.iloc[0]["score"], grp.iloc[0]["evalue"])
        if rescue[0] and rescue[0] not in [d for d, _, _ in top3]:
            if len(top3) < 3:
                top3.append(rescue)
            else:
                top3[2] = rescue
            top3_map[gene] = top3

    upg_descs, confidences = [], []
    for gene, raw_desc, evalue, score in zip(best["gene"], best["description"],
                                             best["evalue"], best["score"]):
        raw_desc = str(raw_desc or "")
        top3 = top3_map.get(gene, [])
        top3_descs = [d for d, _, _ in top3]
        upg = _upgrade_description(raw_desc, top3_descs,
                                   best_score=score, top3_scores=[s for _, s, _ in top3],
                                   best_evalue=evalue, top3_evalues=[e for _, _, e in top3])
        upg_descs.append(upg)
        cleaned_top3 = [
            (_extract_pdb_description(d) if _PDB_CRYSTAL_PREFIX.match(str(d)) else d)
            for d in top3_descs
        ]
        confidences.append(_compute_fs_confidence(
            evalue, score,
            [d for d in cleaned_top3 if d and _is_informative_fs(str(d))],
            pd.notna(evalue),
            anchor=[(_extract_pdb_description(d) if _PDB_CRYSTAL_PREFIX.match(d) else d)
                    for d in (upg, raw_desc)],
        ))
    best["description"]         = upg_descs
    best["foldseek_confidence"] = confidences

    # Coverage fraction + partial-match flag
    if "aa_length" in best.columns:
        best["foldseek_qcov_frac"]    = (best["qcov_aa"] / best["aa_length"]).round(3)
        best["foldseek_partial_match"] = best["foldseek_qcov_frac"].apply(
            lambda f: (f < 0.5) if pd.notna(f) else False
        )
    else:
        best["foldseek_qcov_frac"]    = float("nan")
        best["foldseek_partial_match"] = False

    # Drop same_host / defense_flag for non-useful descriptions
    not_useful = (
        (best["informative_hit_found"] == False) |
        best["description"].apply(
            lambda d: not _is_informative_fs(str(d) if d else "")
                      or _is_generic_name(str(d) if d else "")
        )
    )
    best.loc[not_useful, "same_host"]    = False
    best.loc[not_useful, "defense_flag"] = False

    # Promiscuous fold + eukaryotic description flags
    # Step 04 routes flagged hits to manual review instead of auto-annotating.
    best["promiscuous_fold_flag"] = best["description"].apply(
        lambda d: _is_promiscuous_fold_hit(str(d) if d else "")
    )
    best["eukaryotic_desc_flag"] = best["description"].apply(
        lambda d: _has_eukaryotic_description(str(d) if d else "")
    )

    # A promiscuous fold (β-lactamase / MBL-TLD, motor proteins, …) is a
    # FOLD, not a function — it carries little functional information even at a
    # high structural score. CAP its confidence at BORDERLINE so it can never enter
    # GOOD/CONFIDENT auto-annotation; the flag + cap together keep it as a noted-
    # but-uninformative hit (step 04 still routes flagged hits to review).
    _demote = best["promiscuous_fold_flag"] & best["foldseek_confidence"].isin(["CONFIDENT", "GOOD"])
    best.loc[_demote, "foldseek_confidence"] = "BORDERLINE"

    _t3_extra = [c for c in ["foldseek_subdb", "kingdom", "kingdom_source"] if c in hits.columns]
    top3 = (
        hits[hits["informative"]]
        .groupby("gene", sort=False)
        .head(3)
        .reset_index(drop=True)
    )[["gene", "accession", "description", "taxname",
       "pident", "evalue", "score", "composite_score", "qcov_aa",
       "same_host", "defense_flag"] + _t3_extra]

    # Add genes with zero hits
    no_hit = set(all_genes) - set(best["gene"])
    if no_hit:
        no_hit_df = pd.DataFrame({
            "gene":                 sorted(no_hit),
            "informative_hit_found": False,
            "foldseek_confidence":  "NO_HIT",
            "promiscuous_fold_flag": False,
            "eukaryotic_desc_flag":  False,
        })
        best = pd.concat([best, no_hit_df], ignore_index=True)

    best = best.sort_values("gene").reset_index(drop=True)
    top3 = top3.sort_values(["gene", "score"], ascending=[True, False]).reset_index(drop=True)
    return best, top3
