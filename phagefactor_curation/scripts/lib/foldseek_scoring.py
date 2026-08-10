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
  _DEFENSE_PATTERN          : regex (exported for re-use)
"""

import re
from collections import Counter

try:
    import pandas as pd
except ImportError as e:  # pragma: no cover -- pipeline-level dependency
    raise SystemExit(f"Missing dependency: {e}\n  pip install pandas")

from config import UNINFORMATIVE_STRINGS, HOST_GENUS

# Single source of truth for description vocabulary (see lexicon.py).
from lexicon import (
    is_informative_desc,
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
    PROMISCUOUS_FOLDS as _PROMISCUOUS_FOLD_PATTERNS,
    DEFENSE_PATTERN as _DEFENSE_PATTERN,
    EUKARYOTIC_DESC as _EUKARYOTIC_DESCRIPTION_KEYWORDS,
)

try:
    from config import HOST_BOOST
except ImportError:      # older config.py without the setting
    HOST_BOOST = 1.20


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
# Action: hits matching these patterns are flagged with promiscuous_fold_flag=True
# in best_hit.csv.  Step 04 then routes them to needs_review rather than
# auto-annotating.  The flag is NOT an automatic rejection: many true positives
# exist (e.g. a real MBL-superfamily RNA-repair enzyme in a moron module).

# _PROMISCUOUS_FOLD_PATTERNS is now defined ONCE in lexicon.py as PROMISCUOUS_FOLDS,
# imported at the top of this file. The rationale above still
# applies -- only the definition moved.

# Eukaryotic fold keywords inferred from description text alone (no taxonomy).
# Used for afdb-swissprot and pdb100 hits where taxname="" because the DB
# was built without embedded taxonomy.  Pattern is intentionally conservative
# (only unambiguous eukaryote-specific terms) to avoid false demotion.
# _EUKARYOTIC_DESCRIPTION_KEYWORDS is now defined ONCE in lexicon.py as EUKARYOTIC_DESC,
# imported at the top of this file. The rationale above still
# applies -- only the definition moved.


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

# _PDB_CRYSTAL_PREFIX is now defined ONCE in lexicon.py as PDB_CRYSTAL_PREFIX,
# imported at the top of this file. The rationale above still
# applies -- only the definition moved.

# Trailing qualifiers to strip once we have the protein name
# _PDB_TRAILING_QUAL is now defined ONCE in lexicon.py as PDB_TRAILING_QUAL,
# imported at the top of this file. The rationale above still
# applies -- only the definition moved.

# PDB titles that are uninformative even after extraction
# _PDB_UNINFORMATIVE is now defined ONCE in lexicon.py as PDB_UNINFORMATIVE,
# imported at the top of this file. The rationale above still
# applies -- only the definition moved.


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

# _UNCHAR_PATTERN is now defined ONCE in lexicon.py as UNCHAR_PATTERN,
# imported at the top of this file. The rationale above still
# applies -- only the definition moved.


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

# _PHAGE_SPECIFIC is now defined ONCE in lexicon.py as PHAGE_SPECIFIC,
# imported at the top of this file. The rationale above still
# applies -- only the definition moved.

# _PHAGE_CONTEXT is now defined ONCE in lexicon.py as PHAGE_CONTEXT,
# imported at the top of this file. The rationale above still
# applies -- only the definition moved.

# _GENERIC_DOMAIN is now defined ONCE in lexicon.py as GENERIC_DOMAIN,
# imported at the top of this file. The rationale above still
# applies -- only the definition moved.

# _DEFENSE_PATTERN is now defined ONCE in lexicon.py as DEFENSE_PATTERN,
# imported at the top of this file. The rationale above still
# applies -- only the definition moved.


# bare gp / phage-protein / Mu-like wrapper hits -> demoted in the top-3 ranking
# _GP_WRAPPER_MALUS is now defined ONCE in lexicon.py as GP_WRAPPER,
# imported at the top of this file. The rationale above still
# applies -- only the definition moved.


def _phage_boost_factor(desc: str) -> float:
    """Composite-score multiplier (named phage protein x2, viral context x1.5,
    generic 'family/domain-containing' suffix x0.75 penalty, gp/wrapper x0.40 malus).

    The x0.75 generic penalty also covers GENE-LABEL style names (UPF1234 protein,
    YabC protein, orfNN, 'Prophage ..., Orf12'): these carry as little functional
    information as a bare 'X family protein', so they should not out-rank a named
    function at equal bitscore. (Added 2026-07; previously only the domain/family
    SUFFIX was penalised.)"""
    if not desc or not isinstance(desc, str):
        return 1.0
    boost = 1.0
    if _PHAGE_SPECIFIC.search(desc):
        boost *= 2.00
    elif _PHAGE_CONTEXT.search(desc):
        boost *= 1.50
    desc_for_generic = re.sub(r'\s*\(fragment\)\s*$', '', desc.strip(), flags=re.IGNORECASE)
    if _GENERIC_DOMAIN.search(desc_for_generic) or _GENERIC_NAMES.search(desc_for_generic):
        boost *= 0.75
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

# _GENERIC_NAMES is now defined ONCE in lexicon.py as GENERIC_NAMES,
# imported at the top of this file. The rationale above still
# applies -- only the definition moved.

# _GENE_LABEL is now defined ONCE in lexicon.py as GENE_LABEL,
# imported at the top of this file.


def _is_generic_name(desc: str) -> bool:
    if not desc or not isinstance(desc, str):
        return True
    return bool(_GENERIC_NAMES.search(desc.strip()))


def _top3_agrees(descs: list) -> bool:
    """Return True iff 2+ descriptions refer to the same function (Fragment-aware)."""
    normed = []
    for d in descs:
        if not d or not isinstance(d, str):
            continue
        n = re.sub(r'\s*\(fragment\)\s*$', '', d.strip().lower())
        normed.append(n)
    if len(normed) < 2:
        return False
    for i in range(len(normed)):
        for j in range(i + 1, len(normed)):
            a, b = normed[i], normed[j]
            if a == b or a.startswith(b + " ") or b.startswith(a + " "):
                return True
    return False


def _upgrade_description(best_desc: str, top3_descs: list) -> str:
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

    all_descs = ([best_desc] if best_desc else []) + top3_descs
    informative = [d for d in all_descs if isinstance(d, str) and _is_informative_fs(d)]
    if not informative:
        return best_desc or ""

    non_generic = [d for d in informative if not _is_generic_name(d)]

    # Rule 1 - majority vote over top-3
    top3_non_generic = [d for d in (top3_descs or [])
                        if isinstance(d, str) and _is_informative_fs(d) and not _is_generic_name(d)]
    if top3_non_generic:
        norm_map: dict = {}
        for d in top3_non_generic:
            n = re.sub(r'\s*\(fragment\)\s*$', '', d.lower().strip())
            norm_map.setdefault(n, []).append(d)
        counts = Counter({k: len(v) for k, v in norm_map.items()})
        top_norm, top_count = counts.most_common(1)[0]
        if top_count >= 2:
            return max(norm_map[top_norm], key=len)

    # Rule 2 - generic best_desc -> swap with best non-generic top-3
    if _is_generic_name(best_desc) and non_generic:
        best_non_generic = max(non_generic, key=lambda d: (
            2 if _PHAGE_SPECIFIC.search(d) else (1 if _PHAGE_CONTEXT.search(d) else 0),
            len(d),
        ))
        if best_desc and _GENE_LABEL.match(best_desc.strip()):
            return f"{best_desc.strip()} ({best_non_generic})"
        return best_non_generic

    # Rule 3 - best_desc is a prefix of a more specific top-3 entry
    if best_desc and not _is_generic_name(best_desc):
        bl = best_desc.lower().strip()
        for d in informative[1:]:
            dl = d.lower().strip()
            if dl != bl and dl.startswith(bl + " "):
                return d

    return best_desc


def _compute_fs_confidence(evalue, score, top3_inf_descs: list,
                           has_any_hit: bool) -> str:
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
    agrees = _top3_agrees(inf_descs)

    # CONFIDENT: requires strong evalue+score, OR top3 agreement at threshold.
    # Score-only CONFIDENT was removed: high structural-alignment scores are
    # common for promiscuous folds (see _PROMISCUOUS_FOLD_PATTERNS) and do not
    # alone constitute confident functional annotation.
    if ev is not None and ev <= 1e-3 and sc >= 200:
        return "CONFIDENT"
    if agrees and ((ev is not None and ev <= 1e-3) or sc >= 300):
        return "CONFIDENT"

    if (ev is not None and ev <= 0.01) or sc >= 200:
        return "GOOD"
    if agrees and ev is not None and ev <= 0.05:
        return "GOOD"

    if agrees and ((ev is not None and ev <= 0.1) or sc >= 90):
        return "BORDERLINE"
    if ev is not None and ev <= 0.1:
        return "BORDERLINE"
    if sc >= 90 and (ev is None or ev <= 0.5):
        return "BORDERLINE"

    return "WEAK"


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
    _extra_cols = [c for c in ["aa_length", "foldseek_subdb"] if c in best.columns]
    best = best[["gene", "accession", "description", "taxname",
                 "pident", "evalue", "score", "composite_score",
                 "qcov_aa", "same_host", "defense_flag", "informative"] + _extra_cols]
    best = best.rename(columns={"informative": "informative_hit_found"})

    # Per-gene top-3 informative descriptions
    top3_descs_map: dict = {}
    for gene, grp in hits[hits["informative"]].groupby("gene", sort=False):
        top3_descs_map[gene] = list(grp.head(3)["description"].fillna(""))

    # Phage rescue: inject specific-phage hit into top-3 if missing
    for gene, grp in hits.groupby("gene", sort=False):
        top3 = top3_descs_map.get(gene, [])
        has_phage = any(_PHAGE_SPECIFIC.search(str(d)) for d in top3 if d)
        if not has_phage:
            phage_rescue = grp[
                (grp["score"] > 500) &
                (grp["evalue"] < 1e-7) &
                (grp["description"].apply(lambda d: bool(_PHAGE_SPECIFIC.search(str(d) if d else ""))))
            ]
            if not phage_rescue.empty:
                rescue_desc = phage_rescue.iloc[0]["description"]
                if rescue_desc and rescue_desc not in top3:
                    if len(top3) < 3:
                        top3.append(rescue_desc)
                    else:
                        top3[2] = rescue_desc
                    top3_descs_map[gene] = top3

    upg_descs, confidences = [], []
    for _, row in best.iterrows():
        gene = row["gene"]
        raw_desc = str(row.get("description") or "")
        top3 = top3_descs_map.get(gene, [])
        has_any_hit = pd.notna(row.get("evalue"))
        upg = _upgrade_description(raw_desc, top3)
        upg_descs.append(upg)
        cleaned_top3 = [
            (_extract_pdb_description(d) if _PDB_CRYSTAL_PREFIX.match(str(d)) else d)
            for d in top3
        ]
        confidences.append(_compute_fs_confidence(
            row.get("evalue"), row.get("score"),
            [d for d in cleaned_top3 if d and _is_informative_fs(str(d))],
            has_any_hit,
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

    _t3_extra = [c for c in ["foldseek_subdb"] if c in hits.columns]
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
