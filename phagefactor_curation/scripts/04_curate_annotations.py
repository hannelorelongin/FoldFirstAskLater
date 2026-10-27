#!/usr/bin/env python3
"""
04_curate_annotations.py
=========================
Automated curation and flagging of the PHold vs FoldSeek comparison.

Reads:
  03_comparison/comparison_per_gene.csv

Decision tree per hypothetical gene:
  ---------------------------------------------------------------------
  Case                                     Auto-action       Flag?
  ---------------------------------------------------------------------
  strong/partial match, both inf           Auto-merge        No
  complementary (cat matches FS desc)      Auto-merge        No
  phold_only (PHold inf, FS uninf)         Use PHold         No
  foldseek_only (FS inf, PHold uninf)      Use FoldSeek (GOOD+) No
  different (both inf, diverge)            Flag              Yes
  both_uninformative                       "hypothetical"    No
  ---------------------------------------------------------------------

For flagged genes, a curation_suggestion column is pre-filled with the
rule engine's proposed merged annotation, and a curation_explanation column
states which rule fired and why. You review these in Excel, adjust if
needed, then fill final_annotation.

Writes:
  04_output/curation/curated_annotations.csv -> all genes, with needs_review,
      curation_suggestion, curation_explanation and final_annotation.
  Step 05 then writes review_suggested.csv (flagged genes) and the final table.

After manual review: edit final_annotation in review_suggested.csv, then rerun
scripts/05_build_output.py.

Usage:
  cd phagefactor/
  python scripts/04_curate_annotations.py
"""

import sys
import re
from pathlib import Path

_SCRIPTS_DIR = Path(__file__).parent
_PROJECT_DIR = _SCRIPTS_DIR.parent
sys.path.insert(0, str(_PROJECT_DIR))
# scripts/lib/ holds the shared modules (config, utils, lexicon, ...).
# Harmless if they still sit next to this file.
_LIB_DIR = Path(__file__).resolve().parent / "lib"
if _LIB_DIR.is_dir():
    sys.path.insert(0, str(_LIB_DIR))

# Single source of truth for description vocabulary (see lexicon.py).
from lexicon import (
    GENERIC_STRUCTURAL_PRODUCTS as _GENERIC_STRUCTURAL_TERMS,
    DEFENSE_ANY as _DEFENSE_ANY,
    PDB_TITLE as _FS_PDB_TITLE_RE,
    EUKARYOTIC_DESC as _EUKARYOTIC_DESC_RE,
    TA_FAMILY as _TA_FAMILY,
    DOMAIN_PARTNERS as _DOMAIN_PARTNERS,
)

from config import (
    COMPARISON_DIR, CURATION_DIR,
    FUZZY_STRONG_THRESHOLD, FUZZY_PARTIAL_THRESHOLD,
    PHOLD_TRUSTED_CONF, PHOLD_WEAK_CONF,
    HOST_GENUS,
)
from config import SUBDB_CLOSE_MIN_FIDENT
from utils import log, section, clean_str, is_informative, tokenize, fuzzy_score
# relevant_domain: lexicon.py section G, the positive counterpart to the
# promiscuous-fold list.
from lexicon import relevant_domain
from rules import clean_name as _rules_clean_name
from lexicon import (is_informative_desc, classify as _classify, PDB_TRAILING_QUAL as _PDB_TRAILING_QUAL,
                     extract_pdb_description as _extract_pdb_description,
                     TAG_DOMAIN_SUFFIX, TAG_GENE_LABEL, TAG_ORF_STYLE, TAG_WRAPPER)
from lexicon import (
    COMPLEMENTARY_CATEGORY_MAP,
    GENERIC_WORDS,
    GENERIC_DESCRIPTORS as _GENERIC_DESCRIPTORS,
    FS_GENERIC as _FS_GENERIC,
    FS_GENERIC_BROAD as _FS_GENERIC_BROAD,
    PHAGE_WRAPPER_ONLY as _PHAGE_WRAPPER_ONLY_RE,
    EUKA_RESCUE as _EUKA_RESCUE_RE,
)
from foldseek_scoring import _phage_boost_factor  # keep-foldseek: confident-fs-relevant gate
# _is_pdb_title plus variant/condition tails ('... variant Delta+PHS L36R at
# cryogenic temperature', 'cA4-bound Cami1 in complex with 70S ribosome').
# Case-5 gates keep calling _is_pdb_title.
from foldseek_scoring import _reads_like_title

try:
    import pandas as pd
except ImportError:
    print("pandas required.")
    sys.exit(1)


# -----------------------------------------------------------------------------
# COMPLEMENTARY DETECTION
# -----------------------------------------------------------------------------

def is_complementary(phold_cat: str, fs_desc: str) -> bool:
    """
    Return True if PHold's functional category and the FoldSeek description
    are complementary (same biology, different vocabulary).

    E.g. PHold category='lysis', FS desc='endolysin' -> complementary
    """
    if not phold_cat or not fs_desc:
        return False
    cat_lower = phold_cat.lower()
    desc_lower = fs_desc.lower()
    for category, keywords in COMPLEMENTARY_CATEGORY_MAP.items():
        if category.lower() in cat_lower:
            for kw in keywords:
                if kw.lower() in desc_lower:
                    return True
    return False


# -----------------------------------------------------------------------------
# DEFENSEFINDER ANNOTATION HELPER
# -----------------------------------------------------------------------------

def _parse_defensefinder_name(tophit: str) -> str:
    """
    Convert a DefenseFinder gene ID to a readable system name.
    E.g. 'Gao_Mza_753'  -> 'Gao Mza defense system protein'
         'Avs_1_001'    -> 'Avs1 defense system protein'
    Returns empty string if the pattern does not match, the tophit is a raw
    number, or the name contains phage-gene-number segments (gp##, orf##) that
    indicate a phage protein rather than a defense-system entry.
    """
    if not tophit or not isinstance(tophit, str):
        return ""
    t = tophit.strip()
    # Literal 'DefenseFinder protein' = the system name was lost upstream.
    # Stays blocklisted, but warn rather than drop silently.
    if t.lower() == "defensefinder protein":
        log("  [WARN] unparsed DefenseFinder name reached curation "
            "('DefenseFinder protein'). The system name was lost upstream — "
            "check the DefenseFinder merge step, not the curation rules.")
        return ""
    # Reject sentinel / missing-value strings (e.g. pandas NaN read as "NA")
    if t.lower() in ("nan", "na", "none", "n/a", ""):
        return ""
    # Skip raw numeric scores like "55.96875"
    if re.match(r'^[\d.]+$', t):
        return ""
    # Reject bare phage-gene patterns (gp29, orf12 alone) but allow genuine
    # DefenseFinder multi-component system IDs like MMB_gp29_gp30_1094.
    # Only reject when the ENTIRE name starts with a phage-gene prefix.
    if re.match(r'^(?:gp|orf|phi)\d+', t, re.IGNORECASE):
        return ""
    # Strip trailing numeric serial (e.g. _1094, _753, _001)
    base = re.sub(r'_\d+$', '', t)
    if not base:
        return ""
    # Split on underscores: all-lowercase/digit tokens with gp/orf prefix = component IDs
    parts = base.split('_')
    # Separate the system acronym(s) from the component protein IDs
    system_parts, comp_parts = [], []
    for p in parts:
        if re.match(r'^(?:gp|orf)\d+', p, re.IGNORECASE):
            comp_parts.append(p)
        else:
            system_parts.append(p)
    if not system_parts:
        return ""
    system_name = ' '.join(system_parts)
    if comp_parts:
        return f"{system_name} ({'/'.join(comp_parts)}) defense system protein"
    else:
        return f"{system_name} defense system protein"


# -----------------------------------------------------------------------------
# POST-SELECTION ANNOTATION HELPERS
# -----------------------------------------------------------------------------

# _DEFENSE_ANY is lexicon.DEFENSE_ANY (defense systems + toxin-antitoxin), imported above.


def _desc_is_defense(desc: str) -> bool:
    """True if the description itself names a defense/TA function. Used to decide
    whether a defense flag (which 03_compare may set from ANY top-3 hit) should
    force review: only when the CHOSEN phold/FS call is itself defense -- an
    incidental TA entry in a non-chosen top-3 slot must not flag an otherwise
    agreeing gene (e.g. CEDLNMDB_00945, a transcriptional regulator)."""
    return bool(desc) and isinstance(desc, str) and bool(_DEFENSE_ANY.search(desc))


def _strip_fragment(desc: str) -> str:
    """
    Remove trailing '(Fragment)' from an annotation string.
    The fragment suffix refers to the PDB structure being incomplete, not the
    gene -- it is irrelevant for functional annotation.
    """
    if not desc or not isinstance(desc, str):
        return desc
    return re.sub(r'\s*\(fragment\)\s*$', '', desc.strip(), flags=re.IGNORECASE).strip()


# Rules: (pattern_on_desc, pattern_in_top3, replacement)
#   pattern_in_top3 None -> unconditional rename
#   replacement     None -> use the matched top3 entry verbatim
#                   str  -> literal, or "{match}" template, or a \1 backreference
# UPGRADE_RULES live in lib/rules.py (ordered rules: FIRST MATCH WINS).
from rules import UPGRADE_RULES as _UPGRADE_RULES


def _apply_final_upgrades(desc: str, top3_str: str) -> str:
    """
    Apply post-selection annotation upgrades:
      1. Strip (Fragment) suffix.
      2. Apply _UPGRADE_RULES: prefer more informative top-3 description when
         the primary description matches a less-specific pattern.
    """
    if not desc or not isinstance(desc, str):
        return desc

    # Strip (Fragment) first
    desc = _strip_fragment(desc)

    # Parse top3 entries
    top3 = [t.strip() for t in top3_str.split("|") if t.strip() and t.strip() != "NA"] \
           if top3_str and top3_str not in ("NA", "") else []

    for desc_pat, top3_pat, template in _UPGRADE_RULES:
        if not desc_pat.search(desc):
            continue
        if top3_pat is None:
            # Unconditional rename. A \1 backreference is applied as a
            # substitution so the rule can keep part of the original.
            if template:
                desc = (desc_pat.sub(template, desc)
                        if re.search(r"\\\d", template) else template)
            break
        # Search top3 for a matching entry
        for t3 in top3:
            if top3_pat.search(t3):
                matched = _strip_fragment(t3)
                if template:
                    desc = template.format(match=matched)
                else:
                    desc = matched
                break
    # cGAS is the eukaryotic name; note the bacterial homolog (CD-NTase) in
    # brackets when not already present.
    if re.search(r'\bcGAS\b', desc, re.I) and not re.search(r'cd-?ntase', desc, re.I):
        desc = f"{desc} (CD-NTase)"
    return desc


# Map description text -> function category for proteins that have no PHold call
from rules import category_from_text as _category_from_text

# FoldSeek descriptions carrying no real function beyond "some protein". Used to
# decide when a specialized sub-DB hit (T6SS/T4SS/VFDB/CARD) should win over a
# CONFIDENT-but-generic FoldSeek call even at LOW phold confidence (TagO, IcmN).
# Deliberately narrow so SPECIFIC FS calls (e.g. vanT -> "Alanine racemase") do
# NOT match and keep winning.


def _infer_function_cat(desc: str, current_cat: str) -> str:
    """Category from the description when the current category is unknown.
    A known PHold category is kept."""
    if current_cat and current_cat.lower() not in ("unknown function", "na", "nan"):
        return current_cat
    cat = _category_from_text(desc)
    return cat if cat != "unknown function" else (current_cat or "unknown function")


# -----------------------------------------------------------------------------
# CURATION GATES -- grouped by function, in true top-to-bottom execution order.
#
# Sub-DB substitution lives upstream, in 03_compare_annotations.py (the sub-DB
# name is substituted into phold_product before agreement is even classified).
# It is a standalone upstream step, not part of this tree.
#
# Everything below is a PRE-CHECK in the "different" branch of
# merge_annotations(): tried in code order, first match wins and returns before
# the relatedness fallback.
#
# Gates are tagged "[group: slug]" in the code and in curation_explanation, so
# grep finds a whole group wherever its members sit. Groups are functional
# categories, not execution order -- only keep-phold has a real internal
# priority (most confident first).
#
# keep-phold      -- phold's call wins outright, no merge, no review
#   defense-priority        confident defense-system phold kept over any FS
#   atpase-family           helicase/ABC/SMC + FS ATPase = same superfamily
#   weak-fs-guard           borderline/generic FS can't override informative phold
#   transaldolase-keep      keep transaldolase over FS's panB fold-alike
#   suppress-fs-wrapper     bare "(pro)phage protein" FS hit carries no signal
#   top3-majority           >=2 FS top-3 hits name phold's function, as confidently as the best hit
#   top3-corroboration      an FS top-3 entry (not just top-1) corroborates phold
#   structural-call         phold's generic structural call vs an FS name of another category
#
# keep-foldseek   -- FoldSeek's call wins outright, no merge, no review
#   confident-fs-relevant   confident + phage-relevant FS beats a weak phold guess
#
# merge           -- combine both calls into one annotation
#   ta-functionalize        FS names the toxin/antitoxin phold only flagged by role
#   transposition-motor     FS ATPase IS the transposase's motor -- merge, don't flag
#   morphogenesis           two structural/assembly calls, same virion module
#   subdb-over-generic-fs   specialised sub-DB hit beats a generic FS call
#   fs-specificity-upgrade  FS top-3 converges on a specific name for a generic
#                           phold structural term
#
# specificity     -- prefer whichever side is genuinely specific (symmetric)
#   generic-vs-specific     one side is a pure generic descriptor, keep the other
#
# fallback        -- nothing above matched
#   relatedness-fallback    same function -> merge silently; else needs_review_divergent
# -----------------------------------------------------------------------------

# ---- [merge: fs-specificity-upgrade] helpers -------------------------------
# Several "different" pairs are cases where phold gives a generic
# PHROG-category-level structural term (e.g. "tail protein",
# "baseplate wedge subunit protein", "endolysin") and FoldSeek's top-3 hits
# independently CONVERGE on the same specific, named phage gene product
# (e.g. "Phage protein D", "Phage protein GP46", "...protein gp45"). These
# aren't disagreements -- FS is just naming the same thing more precisely.
# Detect: phold call matches a curated generic-structural-term pattern AND
# >=2 FS top-3 entries mutually share a specific (non-generic) token. Merge as
# "<phold generic term> (<FS specific name>)", attribute phold+foldseek,
# classify concordant (no review flag).

# Words to additionally ignore when looking for FS top-3 internal concordance:
# top-3 "specific names" are frequently short bare IDs ("D", "GP46", "gp45")
# that tokenize()'s len>2 filter would drop as noise -- so we build our own
# lighter token set here rather than reusing tokenize() directly.
_SPEC_DROP_WORDS = GENERIC_WORDS | {"phage", "prophage", "homolog", "to", "of"}


def _top3_id_tokens(desc: str) -> set:
    """Tokenise an FS top-3 entry for fs-specificity-upgrade concordance
    checking, keeping short specific identifiers (e.g. 'D', 'GP46') that
    tokenize() would drop."""
    if not desc or not isinstance(desc, str):
        return set()
    raw = set(re.findall(r"[a-zA-Z0-9]+", desc.lower()))
    return {t for t in raw if t not in _SPEC_DROP_WORDS and len(t) > 1}


def _fs_top3_concordant_specific(top3_str: str):
    """[merge: fs-specificity-upgrade] helper: return the most specific FS top-3 description that is
    corroborated by >=1 OTHER top-3 entry sharing a specific identifier token
    (e.g. both mention 'gp46' / 'd' / 'gp45'), or None if no such internal
    concordance exists. This is the signal that FS's top-3 calls are mutually
    consistent on a precise name (not just noise)."""
    entries = [_strip_fragment(t.strip()) for t in (top3_str or "").split("|")
               if t.strip() and t.strip() != "NA"]
    if len(entries) < 2:
        return None
    tok = [_top3_id_tokens(e) for e in entries]
    for i in range(len(entries)):
        if not tok[i]:
            continue
        for j in range(len(entries)):
            if j != i and tok[j] and (tok[i] & tok[j]):
                return entries[i]
    return None


# ---- [review: head-tail-conflict] helpers -----------------------------------
# Which part of the virion a name places the protein in. Neck proteins (connector,
# adaptor, head closure, tail terminator/completion) sit between head and tail, so
# they never conflict with either. The portal counts as head: it is a capsid vertex,
# so 'baseplate protein (portal protein)' is a conflict.
_NECK_PART = re.compile(r"connector|adapt[oe]r|head[- ]?(closure|completion)|\bneck\b|collar|"
                        r"terminator|tail[- ]completion|stopper|head[- ]tail|joining", re.I)
_TAIL_PART = re.compile(r"\btail|fib(er|re)|baseplate|tape[- ]measure|sheath|\btube\b|spike|\bhub\b|wedge", re.I)
_HEAD_PART = re.compile(r"capsid|\bhead\b|scaffold|prohead|decoration|portal", re.I)


def _virion_part(desc) -> str:
    """'head', 'tail', 'neck' or '' for a product name."""
    d = str(desc or "")
    if _NECK_PART.search(d):
        return "neck"
    if _TAIL_PART.search(d):
        return "tail"
    if _HEAD_PART.search(d):
        return "head"
    return ""


def _head_tail_conflict(p_desc, final_desc):
    """The FoldSeek-side name when the final call puts a PHold head protein on
    the tail (or the reverse), else None. Covers a merge 'PHold (FS)' /
    'PHold / FS' and a plain FS replacement."""
    p = clean_str(p_desc)
    if not is_informative(p) or _virion_part(p) not in ("head", "tail"):
        return None
    f = clean_str(final_desc)
    if f.lower() == p.lower():
        return None
    fs = f[len(p):].strip(" ()/") if f.lower().startswith(p.lower()) else f
    if fs and _virion_part(fs) in ("head", "tail") and _virion_part(fs) != _virion_part(p):
        return fs
    return None


def _flag(row, col) -> bool:
    """A True/False column read NaN-safely: an empty cell (e.g. foldseek_same_host
    when no host genus is configured) is False, not bool(NaN) == True."""
    v = row.get(col, False)
    return bool(v) if v is not None and not (isinstance(v, float) and v != v) else False


# ---- [merge: euka-substitute] helpers --------------------------------------
# A eukaryote-kingdom FS best hit is generally a convergent-fold false positive
# in a prophage and should not be prioritised. Prefer an informative bacterial/
# archaeal/viral FS top-3 hit instead. RESCUE a curated set of eukaryote-
# HOMOLOGOUS but biologically-real defence/immunity proteins (Sir2, TIR, STING,
# cGAS, viperin, Argonaute, gasdermin, SamHD1, Thoeris) -- those are kept when no
# prokaryotic top-3 hit is available, rather than discarded.


def _is_rescued_euka(desc: str) -> bool:
    """True if a eukaryote-homologous description is a curated meaningful
    defence/immunity family that must NOT be discarded as a euka false positive."""
    return bool(desc and isinstance(desc, str) and _EUKA_RESCUE_RE.search(desc))


def _first_prokaryotic_top3(top3_str: str, top3_kingdoms_str: str):
    """Return the first FS top-3 description whose per-hit kingdom is
    prokaryotic/viral (Bacteria/Archaea/Virus), Fragment-stripped, or None.
    Used by euka-substitute to prefer a bacterial hit over a eukaryotic best hit."""
    descs = [_strip_fragment(t.strip()) for t in (top3_str or "").split("|")]
    kings = [k.strip().lower() for k in (top3_kingdoms_str or "").split("|")]
    for i, d in enumerate(descs):
        if not d or d == "NA":
            continue
        k = kings[i] if i < len(kings) else ""
        if k in ("bacteria", "archaea", "virus", "viruses"):
            return d
    return None


# ---- [keep-phold: suppress-fs-wrapper] helpers -----------------------------
# "Prophage protein" / "Phage protein" / "Putative (pro)phage protein" carry
# no functional content beyond "this is a phage gene" -- when phold has a
# specific call, don't flag divergent, just keep phold's call. Suppression
# applies ONLY when the FS description is JUST the wrapper phrase. If a
# qualifier survives in front of it (e.g. "baseplate phage protein" --
# "baseplate" IS informative), the rule must NOT fire; the qualifier has to
# remain visible, so it falls through to normal divergent handling instead.


# Broader "FS hit carries no usable function" detector (superset of the wrapper):
# gp/HK97/Mu-like prophage proteins, generic "phage <descriptor> protein",
# bare ATP-binding/ATPase, "Prophage X protein NN", uncharacterised. Used to keep
# phold (no review) when FoldSeek only returns one of these. Carefully excludes
# specific phage names (tail/portal/baseplate/holin/terminase/capsid/connector/fiber).


# PDB/structure-title detector: pdb100 descriptions are sometimes the full paper/
# structure title (a sentence) rather than a gene name -- carries no protein
# identity ("Biophysical and cellular characterisation of a junctional epitope
# antibody that locks IL-6 and gp80...").


def _is_pdb_title(desc: str) -> bool:
    """True if the FS description reads like a structure/paper title, not a gene
    name (>=8 words, or a tell-tale structural-biology phrase)."""
    if not desc or not isinstance(desc, str):
        return False
    return len(desc.split()) >= 8 or bool(_FS_PDB_TITLE_RE.search(desc))


def _fs_uninformative(desc: str) -> bool:
    """FoldSeek hit carries no usable function (wrapper, broad-generic, or a
    structure/paper title that is not a gene name)."""
    if not desc or not isinstance(desc, str):
        return False
    return bool(_FS_GENERIC_BROAD.match(desc.strip())) or _is_pdb_title(desc)


def _is_phage_wrapper_only(desc: str) -> bool:
    """True only if `desc` is JUST a generic phage-protein wrapper phrase with
    no qualifier in front (anchored match -- "baseplate phage protein" does
    NOT match; the wrapper must appear alone, with no informative qualifier)."""
    if not desc or not isinstance(desc, str):
        return False
    return bool(_PHAGE_WRAPPER_ONLY_RE.match(desc.strip()))


# ---- [keep-phold: top3-corroboration] helpers ------------------------------
# Scans FS top-3 (not just top-1) for corroboration.
# Example: "Anti-termination protein Q-like" (phold) was flagged divergent
# against FS's generic top-1 "Antitermination protein", but FS's THIRD-ranked
# hit ("Antiterminator Q protein of prophage CP-933K") directly corroborates
# phold's "Q-like" call -- merge_annotations() only ever compares against
# fs_desc (top-1). Two refinements make this robust: (1) hyphens are
# normalised before matching -- "anti-term" vs "antiterm" / "anti-termination"
# vs "antitermination" would otherwise silently fail token overlap; (2) gated
# to FS confidence == CONFIDENT only, since comparing phold against a
# low-confidence FS hit compares across non-equivalent evidence tiers.
def _normalize_hyphens(text: str) -> str:
    """Strip hyphens so 'anti-termination'/'antitermination' and
    'anti-term'/'antiterm' tokenise identically (top3-corroboration hyphen fix)."""
    if not text or not isinstance(text, str):
        return text
    return text.replace("-", "")


def _top3_corroborates(p_desc: str, top3_str: str):
    """[keep-phold: top3-corroboration] helper: scan ALL FS top-3 entries (hyphen-normalised) for
    keyword/fuzzy overlap with PHold's description. Returns the corroborating
    top-3 description, or None."""
    p_tok = tokenize(_normalize_hyphens(p_desc))
    if not p_tok:
        return None
    for entry in (t.strip() for t in (top3_str or "").split("|")):
        if not entry or entry == "NA":
            continue
        e_clean = _strip_fragment(entry)
        e_norm = _normalize_hyphens(e_clean)
        e_tok = tokenize(e_norm)
        if not e_tok:
            continue
        if (p_tok & e_tok) and fuzzy_score(_normalize_hyphens(p_desc), e_norm) >= FUZZY_PARTIAL_THRESHOLD:
            return e_clean
    return None


# ---- [specificity: generic-vs-specific] helpers ----------------------------
# Example: "CII-like transcriptional activator" (phold, a real named phage
# regulator family) vs. "DNA-binding protein (Fragment)" (FS, generic) -- the
# existing strong/partial substring-preference logic never reaches these
# because they land in "different". If one side IS a pure generic descriptor
# and the other names a specific family, auto-resolve to the specific one.
# Must not collide with the EXISTING specific-over-generic merges that already
# live in _UPGRADE_RULES (e.g. "DNA helicase / AAA family ATPase" -> "DNA
# helicase", "minor tail protein / TIGR04255" -> "minor tail protein") --
# those are COMBINED single-source strings, resolved by pattern upgrade, not
# cross-source disagreements. This gate therefore only fires on an EXACT
# membership match against a small, pure generic-descriptor set -- it can
# never match a combined "X / generic-Y" string (which contains "/" and extra
# text), so it cannot double-fire with _UPGRADE_RULES.


def _generic_vs_specific(p_desc: str, fs_desc: str):
    """[specificity: generic-vs-specific] helper: if exactly one of (p_desc, fs_desc) is a PURE member of
    _GENERIC_DESCRIPTORS (after stripping a trailing "(Fragment)") and the
    other is informative/specific, return (specific_desc, specific_source).
    Else None. Exact-membership keeps this from ever matching combined
    "X / generic-Y" strings already handled by _UPGRADE_RULES (see docstring)."""
    p_key  = _strip_fragment(clean_str(p_desc)).strip().lower()
    fs_key = _strip_fragment(clean_str(fs_desc)).strip().lower()
    p_generic  = p_key in _GENERIC_DESCRIPTORS
    fs_generic = fs_key in _GENERIC_DESCRIPTORS

    def _specific(desc, key):
        # winner must be genuinely specific: informative, NOT itself in the
        # generic set, and NOT a bare (pro)phage-protein wrapper. This stops a
        # generic FS wrapper ("Putative phage protein") from beating a specific
        # phold call when phold happens to be in the generic set.
        return (is_informative(desc) and key not in _GENERIC_DESCRIPTORS
                and not _is_phage_wrapper_only(desc))

    if p_generic and not fs_generic and _specific(fs_desc, fs_key):
        return (fs_desc, "foldseek")
    if fs_generic and not p_generic and _specific(p_desc, p_key):
        return (p_desc, "phold")
    return None


# -----------------------------------------------------------------------------
# ANNOTATION MERGING
# -----------------------------------------------------------------------------

# ---- NetFlaX helpers -------------------------------------------------------
from rules import TA_CATEGORY as _TA_CATEGORY   # one definition (rules.py)
from rules import same_function as _same_function
from config import FOLDSEEK_TOP3
from foldseek_scoring import _compute_fs_confidence, _TIER_RANK

# FoldSeek top-3 hits per gene with their own e-value and score (loaded in main)
_TOP3_HITS: dict = {}


def _load_top3_hits():
    if not Path(FOLDSEEK_TOP3).is_file():
        return
    t = pd.read_csv(str(FOLDSEEK_TOP3), dtype=str).fillna("")
    for gene, g in t.groupby("gene", sort=False):
        _TOP3_HITS[gene] = [(r["description"], r.get("evalue", ""), r.get("score", ""))
                            for _, r in g.iterrows()]


def _top3_majority_for_phold(locus_tag: str, p_desc: str, fs_conf: str):
    """[keep-phold: top3-majority] >=2 FoldSeek top-3 hits name PHold's function
    and are as confident as the best hit: return the best of those names."""
    hits = _TOP3_HITS.get(locus_tag, [])
    descs = [d for d, _, _ in hits]
    corro = [(d, e, sc) for d, e, sc in hits
             if d and (_same_function(_strip_fragment(d), p_desc) or _top3_corroborates(p_desc, d))]
    if len(corro) < 2:
        return None
    best_rank = _TIER_RANK.get(str(fs_conf).upper(), 0)
    tiers = [_TIER_RANK.get(_compute_fs_confidence(e, sc, descs, True), 0) for _, e, sc in corro]
    if max(tiers) < best_rank:
        return None
    return p_desc


def _netflax_role(name: str) -> str:
    """'toxin' / 'antitoxin' / '' from a NetFlaX sub-DB name (any format)."""
    m = re.search(r'\b(antitoxin|toxin)\b', str(name), re.I)
    return m.group(1).lower() if m else ""


def _netflax_name(name: str) -> str:
    """Canonical NetFlaX product name: role only, no partner accession."""
    role = _netflax_role(re.sub(r'\(partner:[^)]*\)', '', str(name)))
    return (f"NetFlaX-predicted {role}" if role
            else "NetFlaX-predicted toxin-antitoxin system protein")


_LOW_INFO_TAGS = {TAG_DOMAIN_SUFFIX, TAG_GENE_LABEL, TAG_ORF_STYLE, TAG_WRAPPER}


def _is_low_information_name(desc: str) -> bool:
    """A name that says WHICH BOX the protein falls in, not WHAT IT DOES:
    lexicon tier 1 (domain/family/repeat/motif suffix, gene label, ORF style,
    wrapper) or a broad FoldSeek wrapper -- AND no characterised domain (PIN, HNH, ...)
    Generic-but-real descriptors ('Lipoprotein', 'DNA-binding protein',
    'Secreted protein') are deliberately NOT low-information here.
    AND no function category can be inferred from the name.
    The last two keep 'MarR family transcriptional regulator' or 'PIN
    domain-containing protein' (same shape, real function) informative."""
    if not desc or not isinstance(desc, str):
        return False
    d = clean_str(desc)
    shaped = bool(_classify(d)[1] & _LOW_INFO_TAGS) or _fs_uninformative(d)
    if not shaped or relevant_domain(d) is not None:
        return False
    # 'Terminase large subunit gp17-like C-terminal domain-containing protein'
    # or 'Arc-like DNA binding domain-containing protein' keep their name.
    return _category_from_text(d) == "unknown function"


_VIRION_MODULES = {
    "head": re.compile(r'\b(head|capsid|portal|decoration|scaffold\w*)\b', re.I),
    "tail": re.compile(r'\b(tail|baseplate|fib(?:er|re)|sheath|tape.measure)\b', re.I),
}


def _virion_module_majority(fs_desc: str, top3) -> str:
    """When the FoldSeek best hit names one virion module (head) but >= 2 top-3
    hits name the other (tail), return the first top-3 hit of the majority module
    ('Head decoration protein' | 'Phage tail protein' | 'Tail fiber protein' ->
    'Phage tail protein'); otherwise fs_desc unchanged. Only used when phold's
    call is a generic structural term."""
    hits = [t.strip() for t in str(top3).split("|") if t.strip() and t.strip() != "NA"]
    own = [m for m, rx in _VIRION_MODULES.items() if rx.search(str(fs_desc))]
    if len(own) != 1:
        return fs_desc
    other = "tail" if own[0] == "head" else "head"
    side = [h for h in hits
            if _VIRION_MODULES[other].search(h) and not _VIRION_MODULES[own[0]].search(h)]
    return side[0] if len(side) >= 2 else fs_desc


def _fs_names_a_function(desc: str) -> bool:
    """A FoldSeek description that names WHAT THE PROTEIN IS: informative, not
    a title/wrapper, not a narrow-generic FS phrase, not low-information."""
    if not is_informative(desc) or _fs_uninformative(desc):
        return False
    d = clean_str(desc)
    if _FS_GENERIC.search(d) or _reads_like_title(d):
        return False
    # generic-but-real ('Lipoprotein', 'Secreted protein'): fine as a last-resort
    # final name, but never enough to beat a specific call
    if d.lower() in _GENERIC_DESCRIPTORS:
        return False
    return not _is_low_information_name(d)


def _title_core(desc: str) -> str:
    """Protein name buried in a structure title: drop a 'Crystal structure of'
    prefix, a ligand prefix ('cA4-bound ...') and a trailing qualifier ('... in
    complex with 70S ribosome'). 'cA4-bound Cami1 in complex with 70S ribosome'
    -> 'Cami1'. Returns '' when nothing usable is left."""
    d = clean_str(desc)
    d = _extract_pdb_description(d) or d
    d = _PDB_TRAILING_QUAL.sub("", d)
    d = re.sub(r"^(?:[\w+-]+[-\s](?:bound|free|liganded|apo)\s+)+", "", d, flags=re.I).strip(" ,;:")
    return d if (d and is_informative_desc(d) and not _reads_like_title(d)) else ""


_CELL_WALL_BINDING = re.compile(r"\b(?:cell[ -]?wall|peptidoglycan|carbohydrate|choline|"
                                r"lysm|sh3b?)\b[^,;]{0,30}binding", re.I)


def merge_annotations(row: pd.Series) -> tuple:
    """
    Apply the curation decision tree for one gene.

    Returns:
      (final_desc: str,
       final_function_cat: str,
       source: str,
       curation_action: str,
       needs_review: bool,
       curation_suggestion: str,
       curation_explanation: str)
    """
    agreement    = str(row.get("agreement", "both_uninformative")).lower()
    p_desc       = clean_str(row.get("phold_product", "NA"))
    p_cat        = clean_str(row.get("phold_function_cat", "NA"))
    p_conf       = str(row.get("phold_confidence", "none")).lower()
    p_evalue     = row.get("phold_evalue")
    p_inf        = _flag(row, "phold_inf")
    p_tophit     = clean_str(row.get("phold_tophit", "NA"))
    p_method     = clean_str(row.get("phold_method", "NA"))
    sub_source   = clean_str(row.get("subdb_source", "NA")).lower()
    sub_name     = clean_str(row.get("subdb_name", "NA"))
    p_phrog_l    = str(row.get("phold_phrog", "")).lower()

    # Sub-DB hit (ACR/VFDB/CARD/NetFlaX/DefenseFinder): phold's global top1 search
    # often only records a GENERIC placeholder in `product` when one of these wins
    # ("VFDB virulence factor protein", "CARD resistance protein", or leaves it
    # blank for DefenseFinder) -- the real structured identity was joined in
    # 03_compare from sub_db_tophits/*.tsv (see _load_subdb_hits there) and is
    # surfaced here as subdb_source/subdb_name. We trust phold_phrog (not the
    # placeholder string or a regex on the raw tophit ID) to tell us which sub-DB
    # phold actually flagged this gene against -- it survives the NaN-cleaning
    # that can blank out phold_product for DefenseFinder hits.
    subdb_hit = is_informative(sub_name) and sub_source == p_phrog_l
    netflax_hit = subdb_hit and sub_source == "netflax"
    if subdb_hit:
        p_desc = sub_name
        p_inf  = True   # it IS informative: a named, structurally-identified protein
        if sub_source in ("defensefinder", "acr"):
            # PHold maps these to the generic "moron, auxiliary metabolic gene and
            # host takeover" PHROG category; override to "defense", the
            # biologically correct category for these immune-system families.
            p_cat = "defense"
        elif netflax_hit:
            # NetFlaX = PREDICTED toxin-antitoxin pairs. Not an HMM-validated
            # defense system: it keeps PHold's TA category and is resolved by
            # its own gate below (never by defense-priority / subdb-priority).
            p_desc = _netflax_name(sub_name)
            p_cat  = _TA_CATEGORY
    elif p_phrog_l == "defensefinder" and (
        p_desc.lower() in ("defensefinder protein", "defense protein")
        or "defensefinder" in p_method.lower()
    ):
        # No subdb_source/subdb_name for this gene (sub_db_tophits missing or
        # empty): parse the DefenseFinder system name from the tophit ID.
        df_name = _parse_defensefinder_name(p_tophit)
        if df_name:
            p_desc = df_name
            p_inf  = True
            p_cat  = "defense"

    fs_desc      = clean_str(row.get("foldseek_description", "NA"))
    fs_conf      = str(row.get("foldseek_confidence", "NO_HIT") or "NO_HIT")
    # foldseek_inf from 03_compare already has the WEAK filter applied (03_compare
    # sets fs_inf=False for WEAK/NO_HIT confidence hits before writing the CSV).
    # We keep the raw flag for re-routing logic below.
    _fs_inf_raw  = _flag(row, "foldseek_inf")
    # Safety net: WEAK/NO_HIT confidence hits must not drive annotation decisions
    # even if 03_compare classified them as informative, and the description is
    # re-checked against the current lexicon so a step-04-only rerun picks up
    # lexicon edits.
    fs_inf       = (_fs_inf_raw and (fs_conf not in ("WEAK", "NO_HIT"))
                    and is_informative_desc(fs_desc))
    fs_evalue    = row.get("foldseek_evalue")
    fs_score     = row.get("foldseek_score")
    fs_pident    = row.get("foldseek_pident")
    fz_score     = row.get("fuzzy_score")
    top3         = clean_str(row.get("foldseek_top3", "NA"))
    defense_flag   = _flag(row, "foldseek_defense")
    fs_taxname     = clean_str(row.get("foldseek_taxname", "NA"))
    fs_db          = clean_str(row.get("foldseek_db", "NA"))
    fs_promiscuous = _flag(row, "foldseek_promiscuous")
    # Flag from step 02, OR the current lexicon (so a keyword added since the
    # FoldSeek post-processing ran takes effect on a 04-only rerun).
    fs_euka_desc   = (_flag(row, "foldseek_euka_desc")
                      or (bool(_EUKARYOTIC_DESC_RE.search(fs_desc))
                          and not _EUKA_RESCUE_RE.search(fs_desc)))
    # Taxonomy-grounded eukaryote-only signal (best_hit_kingdom=="Eukaryote" with
    # no Bacteria/Archaea/Virus support in the top-3 -- the same condition
    # 03_compare_annotations.py uses to demote the confidence tier). This is
    # STRONGER evidence than fs_euka_desc (a keyword fallback for generic-
    # sounding descriptions like gene symbols that carry no organism/anatomical
    # keyword) and must feed the same "eukaryotic fold-level false positive"
    # gates below -- otherwise a hit demoted from GOOD/CONFIDENT down to
    # BORDERLINE purely because every top-3 hit is human/eukaryotic can still
    # sail through as auto_foldseek_only untouched.
    fs_euka_kingdom = _flag(row, "foldseek_euka_kingdom")

    # A defense call is never itself a reason to send a gene to review (Tan): the
    # defense annotation + flag are kept in the final table, but review_suggested
    # holds only genuinely doubtful genes. The verbose _defense_note still appears
    # in the explanation. (Genuine divergence still flags, via `not related`.)

    # ----- Agreement re-routing gates (applied in order) ----------------------

    # Gate 14: DF override made p_inf=True but 03_compare still saw
    # "defensefinder protein"/NaN as uninformative -> reclassify as phold_only.
    if p_inf and agreement == "both_uninformative" and not fs_inf:
        agreement = "phold_only"

    # Gate 15: WEAK/NO_HIT confidence means 03_compare may have set an agreement
    # that relied on an unreliable FS hit.  Re-route to prevent Case 4 from
    # auto-accepting poor structural evidence.
    # (03_compare filters these too.)
    if _fs_inf_raw and not fs_inf:
        # FS was informative at parse time but confidence filter demoted it
        if agreement == "foldseek_only":
            agreement = "both_uninformative"
        elif agreement == "different" and p_inf:
            agreement = "phold_only"

    # Gate 16: DF upgrade (Fix 1) set p_inf=True AFTER 03_compare classified this
    # as "foldseek_only" (PHold appeared uninformative then).  Now both sources
    # are informative -> the gene has two competing annotations and needs review.
    if p_inf and agreement == "foldseek_only" and fs_inf:
        agreement = "different"

    # Build suffix notes that appear in any explanation
    _defense_note = (
        " [DEFENSE SYSTEM: check DefenseFinder/PADLOC for exact protein name]"
        if defense_flag else ""
    )

    # -- Specialised sub-DB gate: NetFlaX / VFDB / CARD (any agreement class) --
    # These sub-DBs are reference collections (predicted TA pairs; virulence
    # factors; resistance genes), not HMM-validated families. A PHold hit to
    # one of them is only as good as its identity to the entry (e.g. a
    # 30%-identity "NetFlaX antitoxin" vs a CONFIDENT site-specific integrase).
    #
    # "close"  = phold_fident >= curate.subdb_close_min_fident (default 0.40).
    #            phold_fident is the fraction of identical residues in PHold's
    #            alignment of this protein to the sub-DB entry (0-1).
    # "remote" = phold_fident below that (or missing).
    # FoldSeek "names a function" = _fs_names_a_function (not generic, not a
    # title, not a low-information domain/family label, not a location).
    #
    #   FS CONFIDENT + names a function, sub-DB remote
    #       -> FoldSeek name                  [keep-foldseek: subdb-yield]
    #   NetFlaX, FS GOOD/CONFIDENT + names a function (close, or FS only GOOD)
    #       -> "<FS name> (NetFlaX-predicted <role>)"   [merge: ta-functionalize]
    #   otherwise
    #       -> sub-DB name (VFDB/CARD: subdb-priority below; NetFlaX: here)
    #                                          [keep-phold: subdb-priority]
    if subdb_hit and sub_source in ("netflax", "vfdb", "card"):
        _sd_fid   = row.get("phold_fident")
        _sd_fid   = float(_sd_fid) if _is_valid_float(_sd_fid) else float("nan")
        _sd_close = _sd_fid == _sd_fid and _sd_fid >= SUBDB_CLOSE_MIN_FIDENT
        _sd_ev    = (f"{sub_source} call '{p_desc}' (identity={_sd_fid:.2f}"
                     + (f", evalue={p_evalue:.1e}" if _is_valid_float(p_evalue) else "")
                     + ")")
        _fs_ev    = (f"FoldSeek '{fs_desc}' [{fs_conf}"
                     + (f", evalue={fs_evalue:.1e}" if _is_valid_float(fs_evalue) else "")
                     + (f", score={fs_score:.0f}" if _is_valid_float(fs_score) else "")
                     + "]")
        # a eukaryotic best hit never replaces a sub-DB call, even with bacterial
        # top-3 support ('Late embryogenesis abundant protein Lea14-A', Gossypium)
        _sd_euka_best = str(row.get("best_hit_kingdom", "")).strip().lower() == "eukaryote"
        _fs_usable = (fs_inf and fs_conf in ("CONFIDENT", "GOOD")
                      and not (fs_promiscuous or fs_euka_desc or fs_euka_kingdom or _sd_euka_best)
                      and _fs_names_a_function(fs_desc))
        if _fs_usable:
            fs_name = _apply_final_upgrades(fs_desc, top3)
            # Only when the two calls actually conflict: an agreeing FoldSeek hit
            # (strong / partial / complementary) is handled by Cases 1-3.
            if fs_conf == "CONFIDENT" and not _sd_close and agreement == "different":
                _cat = _infer_function_cat(fs_name, "unknown function")
                if _cat == "unknown function" and is_informative(p_cat):
                    _cat = p_cat          # e.g. a VFDB transporter stays a moron
                return (
                    fs_name, _cat,
                    "foldseek", "auto_fs_over_weak_phold", False, fs_name,
                    f"Confident, specific {_fs_ev} kept over a remote {_sd_ev} "
                    f"(< {SUBDB_CLOSE_MIN_FIDENT:.2f} identity to the reference entry). "
                    f"[keep-foldseek: subdb-yield]{_defense_note}",
                )
            if netflax_hit:
                _role = _netflax_role(p_desc) or "toxin-antitoxin system protein"
                merged = (fs_name if re.search(r'\b(anti)?toxin\b', fs_name, re.I)
                          else f"{fs_name} (NetFlaX-predicted {_role})")
                _cat = _infer_function_cat(fs_name, "unknown function")
                return (
                    merged, _cat if _cat != "unknown function" else _TA_CATEGORY,
                    "merged", "auto_merge_TA_functionalised", False, merged,
                    f"{_sd_ev} gives the role, {_fs_ev} names the protein; both kept. "
                    f"[merge: ta-functionalize]{_defense_note}",
                )
        if netflax_hit:
            _why = ("no informative FoldSeek hit" if not (fs_inf and is_informative(fs_desc))
                    else f"{_fs_ev} is generic, weak or a fold-level match")
            return (
                p_desc, _TA_CATEGORY, "phold", "auto_phold_over_weak_fs", False, p_desc,
                f"{_sd_ev} kept: {_why}. [keep-phold: subdb-priority]{_defense_note}",
            )
        # VFDB / CARD close, or FoldSeek not decisive -> unchanged downstream
        # logic (Case 1-5; subdb-priority in Case 5).

    # -- Case 1: strong or partial match -------------------------------------
    if agreement in ("strong", "partial"):
        # Prefer PHold description: PHold searches phage-specific PHROG/PHAGE-DB
        # databases and is generally more phage-appropriate than generic PDB hits.
        # Exception: if FS description contains p_desc as a substring (i.e. FS is
        # a more qualified version of the same annotation), use the FS description.
        # E.g. PHold "tail protein" + FS "GpE family phage tail protein" -> use FS.
        if is_informative(p_desc) and is_informative(fs_desc):
            if p_desc.lower() in fs_desc.lower():
                best_desc = fs_desc   # FS is more specific
            else:
                best_desc = p_desc    # default: trust PHold
        else:
            best_desc = p_desc if is_informative(p_desc) else fs_desc
        best_desc = _apply_final_upgrades(best_desc, top3)
        best_cat  = _infer_function_cat(best_desc, p_cat if is_informative(p_cat) else "unknown function")
        ev_str = f"phold_evalue={p_evalue:.2e}" if _is_valid_float(p_evalue) else ""
        fs_ev  = f"foldseek_evalue={fs_evalue:.2e}" if _is_valid_float(fs_evalue) else ""
        ev_full = "; ".join(x for x in [ev_str, fs_ev] if x)
        flag_review = False   # defense annotation is kept, but a defense call is
                              # not itself a doubt -> don't send it to review
        return (
            best_desc, best_cat,
            ("both agree" if agreement == "strong" else "merged"),
            f"auto_merge_{agreement}",
            flag_review,
            best_desc,
            f"{agreement.capitalize()} match (fuzzy={fz_score:.2f}). "
            f"PHold='{p_desc}', FoldSeek='{fs_desc}' (cat='{p_cat}'). "
            f"{ev_full}{_defense_note}",
        )

    # -- Case 2: complementary -----------------------------------------------
    # Triggers if 03_compare classified this as complementary (top3-aware), OR
    # if this script's own complementary check agrees.
    # Use PHold product (most phage-specific) unless FS best-hit is the direct
    # functional match.
    if agreement == "complementary" or (p_inf and fs_inf and is_complementary(p_cat, fs_desc)):
        # Prefer the specific FoldSeek call ONLY when PHold is a pure generic
        # descriptor ([specificity: generic-vs-specific], e.g. PHold
        # "transcriptional regulator" -> FS "RinA"/"Excisionase"); otherwise
        # keep the specific PHold name even when
        # the categories are complementary (e.g. PHold "ParA-like partition
        # protein" vs FS "Sporulation initiation inhibitor Soj" -> keep ParA).
        _ge = _generic_vs_specific(p_desc, fs_desc)
        # a BORDERLINE FoldSeek name does not replace even a generic phold call
        if _ge and _ge[1] == "foldseek" and (fs_conf in ("CONFIDENT", "GOOD") or not is_informative(p_desc)):
            _fs_pick = _virion_module_majority(_ge[0], top3)
            best_desc = _apply_final_upgrades(_fs_pick, top3)
            note_src  = (f"PHold product='{p_desc}' is generic; preferring the "
                         f"specific FoldSeek call '{_fs_pick}' (cat='{p_cat}').")
            if _fs_pick != _ge[0]:
                note_src += (f" Best hit '{_ge[0]}' names another virion module than "
                             f"most of the top-3. [specificity: module-majority]")
        else:
            best_desc = p_desc if is_informative(p_desc) else fs_desc
            best_desc = _apply_final_upgrades(best_desc, top3)
            note_src  = (f"PHold product='{p_desc}' (cat='{p_cat}') is consistent with "
                         f"FoldSeek context '{fs_desc}'. Using PHold name.")
        best_cat    = _infer_function_cat(best_desc, p_cat if is_informative(p_cat) else "unknown function")
        flag_review = False   # defense annotation is kept, but a defense call is
                              # not itself a doubt -> don't send it to review
        conf_note   = f" [FS confidence: {fs_conf}]" if fs_conf not in ("CONFIDENT","GOOD") else ""
        return (
            best_desc, best_cat,
            ("both agree" if agreement == "strong" else "merged"),
            "auto_merge_complementary",
            flag_review,
            best_desc,
            f"Complementary annotations. {note_src}{conf_note}"
            f"{_defense_note}",
        )

    # -- Case 3: phold_only ---------------------------------------------------
    if agreement == "phold_only":
        # PHold "low" confidence can still have good evalue/bitscore -- don't
        # demote it; just label the confidence level in the explanation.
        conf_label = p_conf if p_conf in PHOLD_TRUSTED_CONF | PHOLD_WEAK_CONF else "unvalidated"
        ev_str = f"; evalue={p_evalue:.2e}" if _is_valid_float(p_evalue) else ""
        best_desc = _apply_final_upgrades(p_desc, top3)
        best_cat  = _infer_function_cat(best_desc, p_cat if is_informative(p_cat) else "unknown function")
        return (
            best_desc, best_cat,
            "phold",
            "auto_phold_only",
            False,
            best_desc,
            f"PHold annotation only (confidence={conf_label}{ev_str}). "
            f"No informative FoldSeek hit. PHROG={row.get('phold_phrog', 'NA')}.",
        )

    # -- Case 4: foldseek_only ------------------------------------------------
    if agreement == "foldseek_only":
        ev_str  = f"; evalue={fs_evalue:.2e}" if _is_valid_float(fs_evalue) else ""
        sc_str  = f"; score={fs_score:.0f}" if _is_valid_float(fs_score) else ""
        # FoldSeek fident is a 0-1 fraction; print it as a percentage
        pid_str = (f"; pident={(float(fs_pident) * 100 if float(fs_pident) <= 1 else float(fs_pident)):.1f}%"
                   if _is_valid_float(fs_pident) else "")
        best_desc = _apply_final_upgrades(fs_desc, top3)
        best_cat  = _infer_function_cat(best_desc, "unknown function")

        # [fs-only: title-fallback] the winning description is a structure/paper
        # TITLE ("... variant Delta+PHS L36R at cryogenic temperature",
        # "cA4-bound Cami1 in complex with 70S ribosome"), not a protein name.
        # Case 5 tested titles (_is_pdb_title), Case 4 never did. In order:
        #   1. the protein name inside the title ('Cami1'), if there is one;
        #   2. the first top-3 entry that names a function;
        #   3. nothing usable -> no final annotation (hit stays in foldseek_*).
        # A name that is merely LONG (>= 8 words) is only swapped when a better
        # name exists; it is never demoted on length alone.
        _title_note = ""
        _strict_title = _reads_like_title(best_desc, strict=True)
        if _reads_like_title(best_desc):      # naming only: applies to defense hits too
            _alts = [t.strip() for t in str(top3).split("|")
                     if t.strip() and t.strip() != "NA"]
            _core = _title_core(best_desc) if _strict_title else ""
            _alt = _core or next((a for a in _alts
                                  if not _reads_like_title(a) and _fs_names_a_function(a)), None)
            if not _alt and _strict_title:
                return (
                    "hypothetical protein", "unknown function",
                    "no_informative_hit", "both_uninformative", False,
                    fs_desc,
                    f"FoldSeek-only hit '{fs_desc}' [{fs_conf}{ev_str}{sc_str}] is a "
                    f"structure title / wrapper, not a protein name, and no top-3 hit "
                    f"names a function; treated as no informative hit. "
                    f"[fs-only: title-fallback]",
                )
            if _alt:
                _title_note = (f" Top hit description '{fs_desc}' reads like a "
                               f"structure title; "
                               + (f"protein name '{_alt}' extracted from it."
                                  if _alt == _core else f"top-3 name '{_alt}' used instead.")
                               + " [fs-only: title-fallback]")
                best_desc = _apply_final_upgrades(_alt, top3)
                best_cat  = _infer_function_cat(best_desc, "unknown function")

        # [fs-only: weak-evidence] a FoldSeek-only name needs a GOOD/CONFIDENT
        # hit to become the final annotation. A BORDERLINE hit is too weak on its
        # own (eIF5A, NuA4, PetP, a 65 aa 'major capsid'); the hit is kept in
        # foldseek_description / foldseek_confidence, the name in curation_suggestion.
        if fs_conf not in ("CONFIDENT", "GOOD"):
            return (
                "hypothetical protein", "unknown function",
                "no_informative_hit", "both_uninformative", False, best_desc,
                f"FoldSeek-only hit '{best_desc}'{ev_str}{sc_str}{pid_str} "
                f"[FS confidence: {fs_conf}] is too weak to name the protein; "
                f"kept as structural evidence, not as the annotation."
                f"{_title_note} [fs-only: weak-evidence]",
            )

        # GOOD+ FoldSeek-only hit that is a promiscuous fold (TLD/MBL, motor
        # proteins, ubiquitination machinery; Aravind 1999 PMID 11471255; Daiyasu
        # 2001 PMID 11513844), has a eukaryote-specific description, or is a
        # eukaryotic best hit without a same-function prokaryotic/viral hit:
        # the fold is real but the function is not inferred -> review.
        if fs_promiscuous or fs_euka_desc or fs_euka_kingdom:
            prom_note = ""
            if fs_promiscuous:
                prom_note += (
                    " [PROMISCUOUS FOLD: this description matches a known "
                    "structurally promiscuous fold (e.g. TLD/beta-lactamase, "
                    "motor protein). Structural similarity does not imply "
                    "functional identity — verify top-3 agreement before "
                    "accepting this annotation. Ref: Aravind 1999 PMID 11471255.]"
                )
            if fs_euka_desc:
                prom_note += (
                    " [EUKARYOTIC DESCRIPTION: description suggests a eukaryote-"
                    "specific protein despite phage context — likely a fold-level "
                    "false positive.]"
                )
            if fs_euka_kingdom:
                prom_note += (
                    f" [EUKARYOTIC TAXONOMY: eukaryotic best hit ({fs_taxname}) and no "
                    f"Bacteria/Archaea/Virus top-3 hit of the same function — likely a "
                    f"fold-level false positive.]"
                )
            return (
                best_desc, best_cat,
                "foldseek",
                "needs_review_promiscuous_fold",
                True,   # flag for review
                best_desc,
                f"FoldSeek annotation only (PHold no hit){ev_str}{sc_str}{pid_str}. "
                f"Organism: {fs_taxname}. DB: {fs_db}. [FS confidence: {fs_conf}]"
                f"{_defense_note}{prom_note}",
            )

        # [fs-only: low-information] a FoldSeek-only name that is only a label
        # ('SHOCT domain-containing protein', 'XkdX family protein', 'YodL-like';
        # see _is_low_information_name) is not used as the final annotation, at
        # any confidence. The hit is kept: foldseek_description /
        # foldseek_confidence unchanged, name in curation_suggestion.
        if not defense_flag and _is_low_information_name(best_desc):
            return (
                "hypothetical protein", "unknown function",
                "no_informative_hit", "both_uninformative", False, best_desc,
                f"FoldSeek-only hit '{best_desc}'{ev_str}{sc_str}{pid_str} "
                f"[FS confidence: {fs_conf}] names a domain/family/label, not a "
                f"function; kept as structural evidence, not as the annotation."
                f"{_title_note} [fs-only: low-information]",
            )

        # A FoldSeek-only defense hit (e.g. RloG) is a confident call, not a
        # doubt -> keep the annotation but do not send it to review.
        flag_review = False
        action = "auto_foldseek_defense" if defense_flag else "auto_foldseek_only"
        return (
            best_desc, best_cat,
            "foldseek",
            action,
            flag_review,
            best_desc,
            f"FoldSeek annotation only (PHold no hit){ev_str}{sc_str}{pid_str}. "
            f"Organism: {fs_taxname}. DB: {fs_db}. [FS confidence: {fs_conf}]"
            f"{_defense_note}{_title_note}",
        )

    # -- Case 5: different (both informative, diverge) -------------------------
    if agreement == "different":
        # [keep-phold: defense-priority] a confident DEFENSE-system phold call
        # (DefenseFinder / PHROG defense) is HMM-validated and trusted -- keep it,
        # no review, even when FoldSeek names a different fold (transposase,
        # RloG, ...). Defense modules routinely reuse mobile-element folds, so a
        # divergent FS is expected here.
        if p_cat == "defense" and is_informative(p_desc) and p_conf in ("high", "medium"):
            best_desc = _apply_final_upgrades(p_desc, top3)
            return (
                best_desc, "defense", "phold", "auto_phold_defense", False, best_desc,
                f"Confident defense-system call '{p_desc}' (DefenseFinder) kept over "
                f"FoldSeek '{fs_desc}'. [keep-phold: defense-priority]{_defense_note}",
            )

        # [keep-phold: subdb-priority] a phold specialised sub-DB hit (VFDB / CARD /
        # ACR -- curated, structurally-identified virulence / resistance / anti-CRISPR
        # protein) is authoritative; keep it over a divergent general-DB FoldSeek
        # call, no flag. Defense-family sub-DBs are handled by defense-priority above;
        # NetFlaX never reaches this point (specialised sub-DB gate, before Case 1; VFDB/CARD reach it only when close or when FoldSeek is not decisive).
        if subdb_hit and is_informative(p_desc):
            best_desc = _apply_final_upgrades(p_desc, top3)
            best_cat  = _infer_function_cat(best_desc, p_cat if is_informative(p_cat) else "unknown function")
            return (
                best_desc, best_cat, "phold", "auto_phold_subdb_priority", False, best_desc,
                f"phold specialised sub-DB call '{p_desc}' kept as authoritative over "
                f"FoldSeek '{fs_desc}'. [keep-phold: subdb-priority]{_defense_note}",
            )

        # [keep-phold: atpase-family] phold helicase / ATP-dependent enzyme +
        # FoldSeek AAA(+)-ATPase are the SAME superfamily (helicases ARE AAA+
        # ATPases) -> not divergent, keep phold, no flag. Belt-and-suspenders:
        # the complementary map already covers this, but this gate guarantees
        # it even if the complementary path isn't reached. ABC transporters and
        # SMC/condensin proteins are also AAA(+)-family ATPases, so phold "ABC
        # transporter" vs FS "Chromosome segregation protein SMC" / "AAA family
        # ATPase" is the same nucleotide-binding superfamily, not a disagreement.
        if (re.search(r'helicase|atp-?dependent|\babc\b|transporter', str(p_desc), re.I)
                and re.search(r'\baaa\b|atpase|atp-?binding|\bsmc\b|chromosome segregation',
                              str(fs_desc), re.I)
                and not re.search(r'transpos', str(p_desc), re.I)):
            best_desc = _apply_final_upgrades(p_desc, top3)
            best_cat  = _infer_function_cat(
                best_desc, p_cat if is_informative(p_cat) else "DNA, RNA and nucleotide metabolism")
            return (
                best_desc, best_cat, "phold", "auto_phold_atpase_family", False, best_desc,
                f"phold '{p_desc}' and FoldSeek '{fs_desc}' are the same ATPase superfamily "
                f"(helicases are AAA+ ATPases); kept phold, no review. "
                f"[keep-phold: atpase-family]{_defense_note}",
            )

        # [keep-phold: domain-partner] FoldSeek names a domain of the protein
        # phold calls (lexicon.DOMAIN_PARTNERS: AP2 on an HNH endonuclease)
        for _p_re, _fs_re, _why in _DOMAIN_PARTNERS:
            if _p_re.search(str(p_desc)) and _fs_re.search(str(fs_desc)):
                best_desc = _apply_final_upgrades(p_desc, top3)
                best_cat  = _infer_function_cat(best_desc, p_cat if is_informative(p_cat) else "unknown function")
                return (
                    best_desc, best_cat, "phold", "auto_phold_domain_partner", False, best_desc,
                    f"FoldSeek '{fs_desc}' is a domain of phold's '{p_desc}' ({_why}); "
                    f"kept phold, no review. [keep-phold: domain-partner]{_defense_note}",
                )

        # [keep-phold: top3-majority] two of FoldSeek's top-3 name PHold's
        # function, as confidently as the best hit: their name wins over a
        # different best hit ('Minor capsid protein | Tail terminator | Tail
        # terminator' on a PHold 'tail terminator' -> 'Tail terminator').
        if is_informative(p_desc):
            _maj = _top3_majority_for_phold(str(row.get("locus_tag", "")), p_desc, fs_conf)
            if _maj:
                best_desc = _apply_final_upgrades(_maj, top3)
                best_cat  = _infer_function_cat(best_desc, p_cat if is_informative(p_cat) else "unknown function")
                return (
                    best_desc, best_cat, "both agree", "auto_keep_top3_majority", False, best_desc,
                    f"Two FoldSeek top-3 hits name PHold's function '{p_desc}' as confidently as "
                    f"the best hit '{fs_desc}'. Top-3 FS: {top3}. [keep-phold: top3-majority]{_defense_note}",
                )

        # [merge: euka-substitute] the FS best hit is eukaryotic (by taxonomy
        # kingdom OR a curated euka description like Retinoblastoma) -> do NOT
        # prioritise it. Prefer an informative bacterial/archaeal/viral FS top-3
        # hit and auto-merge it with phold via a slash (no obvious combined name),
        # no review flag: e.g. "single strand DNA binding protein / Periplasmic
        # protein". If no prokaryotic top-3 hit exists, a RESCUE term (Sir2, TIR,
        # STING, ... immunity homologs) falls through to normal handling (kept);
        # anything else keeps phold and drops the euka name.
        # Only for a GOOD+ FS call: a BORDERLINE one (incl. a eukaryotic best hit
        # demoted in step 03) goes to weak-fs-guard / fs-only weak-evidence
        # below, so no other top-3 hit is promoted through it.
        _best_king = str(row.get("best_hit_kingdom", "")).strip().lower()
        _euka_best = (_best_king == "eukaryote") or fs_euka_desc
        if _euka_best and not _is_rescued_euka(fs_desc) and fs_conf in ("CONFIDENT", "GOOD"):
            _prok = _first_prokaryotic_top3(
                top3, str(row.get("foldseek_top3_kingdoms", "")))
            # a confident PHold call is not merged with a bacterial hit of another
            # function category ('tail length tape measure protein' + 'ParA')
            _p_cat_txt = _category_from_text(p_desc) if is_informative(p_desc) else ""
            if (_prok and p_conf in PHOLD_TRUSTED_CONF
                    and _p_cat_txt in ("tail", "head and packaging", "connector")
                    and _category_from_text(_prok) != _p_cat_txt):
                _prok = None
            if _prok and is_informative(_prok) and not _fs_uninformative(_prok):
                merged = f"{p_desc} / {_prok}" if is_informative(p_desc) else _prok
                best_cat = _infer_function_cat(
                    p_desc if is_informative(p_desc) else _prok,
                    p_cat if is_informative(p_cat) else "unknown function")
                return (
                    merged, best_cat, "merged", "auto_merge_euka_substituted", False, merged,
                    f"Eukaryotic FS best-hit '{fs_desc}' not prioritised; auto-merged phold "
                    f"'{p_desc}' with bacterial top-3 hit '{_prok}'. [merge: euka-substitute]",
                )
            elif is_informative(p_desc):
                best_desc = _apply_final_upgrades(p_desc, top3)
                best_cat  = _infer_function_cat(best_desc, p_cat if is_informative(p_cat) else "unknown function")
                return (
                    best_desc, best_cat, "phold", "auto_phold_over_euka_fs", False, best_desc,
                    f"Eukaryotic FS best-hit '{fs_desc}' not prioritised and no bacterial "
                    f"top-3 alternative; kept phold '{p_desc}'. [merge: euka-substitute]",
                )

        # [keep-phold: weak-fs-guard] a BORDERLINE/WEAK FoldSeek hit must NOT
        # override or be flagged-divergent against an informative phold call.
        # phold (even low conf) is the more phage-appropriate evidence; only
        # CONFIDENT/GOOD FS competes. Big reducer of false "needs_review" entries.
        # (subdb-over-generic-fs, below, runs after this one, but its p_desc is
        # already the sub-DB name, so keeping phold here is consistent.)
        _fs_weak = fs_conf not in ("CONFIDENT", "GOOD")
        _fs_generic = _fs_uninformative(fs_desc)
        if (_fs_weak or _fs_generic) and is_informative(p_desc) \
                and not _is_phage_wrapper_only(p_desc) and not subdb_hit:
            best_desc = _apply_final_upgrades(p_desc, top3)
            best_cat  = _infer_function_cat(best_desc, p_cat if is_informative(p_cat) else "unknown function")
            _why = "below GOOD confidence" if _fs_weak else "a generic/gp/Mu-like wrapper"
            return (
                best_desc, best_cat, "phold", "auto_phold_over_weak_fs", False, best_desc,
                f"FoldSeek hit '{fs_desc}' is {_why}; kept phold '{p_desc}' without review. "
                f"[keep-phold: weak-fs-guard]{_defense_note}",
            )

        # [keep-foldseek: confident-fs-relevant] (symmetric to weak-fs-guard
        # above) a CONFIDENT/GOOD, non-promiscuous FoldSeek call should be TAKEN
        # over a WEAK (low-confidence) phold call rather than flagged divergent.
        # A phold low-conf guess is not authoritative; a confident structural
        # hit is the better evidence, provided it is phage-relevant: a confident
        # hit on a conserved fold unrelated to phage biology goes to review
        # instead. Phage-relevant means: either a named
        # phage-specific/viral-context description (`_phage_boost_factor >= 1.5`,
        # the same boost tier used for ranking), a same-host-genus hit (plausible
        # host-derived moron/AMG -- these DO occur in real prophages), or a
        # defense-system hit. E.g. phold(low) "outer membrane protein" vs
        # FS(CONFIDENT) "Phage tail protein" -> take tail, no review.
        _phold_weak = (p_conf in PHOLD_WEAK_CONF) or (p_conf not in PHOLD_TRUSTED_CONF)
        _fs_relevant = (_phage_boost_factor(fs_desc) >= 1.5
                        or _flag(row, "foldseek_same_host")
                        or defense_flag)
        if (fs_conf in ("CONFIDENT", "GOOD") and not fs_promiscuous
                and is_informative(fs_desc) and not _fs_uninformative(fs_desc)
                and _fs_names_a_function(fs_desc)   # a bare domain label is not
                                                    # "better evidence"
                and _phold_weak and not subdb_hit and _fs_relevant):
            best_desc = _apply_final_upgrades(fs_desc, top3)
            best_cat  = _infer_function_cat(best_desc, "unknown function")
            return (
                best_desc, best_cat, "foldseek", "auto_fs_over_weak_phold", False, best_desc,
                f"Confident, phage-relevant FoldSeek '{fs_desc}' kept over weak phold "
                f"'{p_desc}' (phold conf={p_conf}); no review. "
                f"[keep-foldseek: confident-fs-relevant]{_defense_note}",
            )

        # [merge: ta-functionalize] a phold NetFlaX / toxin-antitoxin hit gives
        # the ROLE (toxin/antitoxin) but FoldSeek often names the actual protein
        # -- merge them ("AbrB family transcriptional regulator (antitoxin
        # protein)") and don't flag. Recurring pattern: FS functionalises phold
        # TA/NetFlaX calls.
        _is_ta_phold = (p_phrog_l in ("netflax",) or
                        re.search(r'\b(anti)?toxin\b|netflax|abrb', str(p_desc), re.I))
        if _is_ta_phold and is_informative(fs_desc) and not _fs_uninformative(fs_desc):
            _role = re.search(r'\b(antitoxin|toxin)\b', str(p_desc), re.I)
            role_str = (_role.group(1).lower() + " protein") if _role else clean_str(p_desc)
            # a top-3 hit naming a TA family beats a best hit that doesn't
            # ('YgiT family' antitoxin over 'Phage Cro/CI transcriptional regulator')
            _ta_fs = fs_desc
            if not _TA_FAMILY.search(str(fs_desc)):
                _ta_fs = next((_rules_clean_name(t.strip()) for t in str(top3).split("|")
                               if _TA_FAMILY.search(t) and is_informative(t.strip())), fs_desc)
            merged = f"{_apply_final_upgrades(_ta_fs, top3)} ({role_str})"
            return (
                merged, _TA_CATEGORY,
                "merged", "auto_merge_TA_functionalised", False, merged,
                f"phold TA/NetFlaX role '{p_desc}' functionalised by FoldSeek "
                f"'{_ta_fs}'. [merge: ta-functionalize]{_defense_note}",
            )

        # [merge: transposition-motor] phold "DNA transposition"/transposase +
        # FoldSeek AAA-ATPase / NTPase-KAP / P-loop NTPase -> the FS hit IS the
        # transposition motor ATPase; merge "DNA transposition (<motor>)"
        # instead of flagging divergent.
        _is_transpos = bool(re.search(r'transpos', str(p_desc), re.I))
        _fs_ntpase = bool(re.search(r'\baaa\b.*atpase|aaa\s+family\s+atpase|ntpase\s+kap'
                                    r'|kap\s+family|p-?loop', str(fs_desc), re.I))
        if _is_transpos and _fs_ntpase:
            motor = ("AAA ATPase" if re.search(r'aaa', str(fs_desc), re.I)
                     else "NTPase KAP" if re.search(r'kap', str(fs_desc), re.I)
                     else "P-loop NTPase")
            merged = f"DNA transposition ({motor})"
            return (
                merged, "integration and excision",
                "merged", "auto_merge_transposition_motor", False, merged,
                f"phold transposition call '{p_desc}' + FoldSeek motor '{fs_desc}' "
                f"merged as a transposition ATPase. [merge: transposition-motor]{_defense_note}",
            )

        # [merge: morphogenesis] two virion structural/assembly calls (e.g.
        # tail assembly chaperone + portal) are parts of the same morphogenesis
        # module -- merge "phold (FS)" rather than flag divergent.
        _MORPHO = re.compile(
            r'\b(tail|head|portal|capsid|baseplate|tape|measure|major|minor|'
            r'fib(er|re)|sheath|collar|neck|scaffold|chaperone|prohead|assembly)\b', re.I)
        if (is_informative(p_desc) and is_informative(fs_desc)
                and _MORPHO.search(str(p_desc)) and _MORPHO.search(str(fs_desc))):
            fs_short = re.sub(r'^phage\s+', '', clean_str(fs_desc), flags=re.I)
            merged = f"{clean_str(p_desc)} ({fs_short})"
            best_cat = _infer_function_cat(p_desc, p_cat if is_informative(p_cat) else "tail")
            return (
                merged, best_cat, "merged", "auto_merge_morphogenesis", False, merged,
                f"Structural morphogenesis calls merged: phold '{p_desc}' + "
                f"FoldSeek '{fs_desc}'. [merge: morphogenesis]{_defense_note}",
            )

        # [keep-phold: transaldolase-keep] PHold 'transaldolase' vs FS panB /
        # 3-methyl-2-oxobutanoate hydroxymethyltransferase -> keep transaldolase,
        # no flag (same fold, phold's enzyme call is the correct one).
        if re.search(r'transaldolase', str(p_desc), re.I) and \
           re.search(r'3-methyl-2-oxobutanoate hydroxymethyltransferase|panB', str(fs_desc), re.I):
            return (
                "transaldolase",
                _infer_function_cat("transaldolase", p_cat if is_informative(p_cat) else "other"),
                "phold", "auto_phold_transaldolase", False, "transaldolase",
                f"Kept PHold 'transaldolase' over FoldSeek fold-level '{fs_desc}'. "
                f"[keep-phold: transaldolase-keep]{_defense_note}",
            )

        # ---- Remaining pre-checks ---------------------------------------
        # If any of these fire, the gene is auto-resolved here and never
        # reaches the fallback group below. True execution order:
        # merge:subdb-over-generic-fs -> keep-phold:suppress-fs-wrapper ->
        # merge:fs-specificity-upgrade (top-3 concordance) ->
        # keep-phold:top3-corroboration (CONFIDENT-gated) ->
        # specificity:generic-vs-specific (narrowest/most conservative). See
        # the docstrings above merge_annotations() for the reasoning behind
        # each gate.

        # [merge: subdb-over-generic-fs] a specialized sub-DB hit (T6SS/T4SS/
        # VFDB/CARD/...) was adopted into p_desc, but phold confidence was low
        # so 03 classified this "different" and a generic FoldSeek call
        # ("Putative ... protein", "lipoprotein") would otherwise win. When the
        # FS call is GENERIC, trust the structured sub-DB name even at low
        # phold confidence (TagO, IcmN). The _FS_GENERIC guard keeps SPECIFIC
        # FS calls (vanT -> "Alanine racemase") winning, so this never over-fires.
        if subdb_hit and (_fs_uninformative(fs_desc) or _FS_GENERIC.search(fs_desc or "")):
            best_desc = _apply_final_upgrades(p_desc, top3)
            best_cat  = _infer_function_cat(best_desc, p_cat if is_informative(p_cat) else "unknown function")
            return (
                best_desc, best_cat,
                "phold",
                "auto_merge_subdb_over_generic_fs",
                False,
                best_desc,
                f"Specialized sub-DB hit '{p_desc}' ({sub_source}) preferred over "
                f"generic FoldSeek call '{fs_desc}' despite low phold confidence. "
                f"[merge: subdb-over-generic-fs]{_defense_note}",
            )

        # [keep-phold: suppress-fs-wrapper] FS hit is JUST a "(pro)phage
        # protein" wrapper -- no function beyond "this is a phage gene" -- and
        # PHold has a specific call. Keep PHold's call; do NOT flag divergent.
        # Fires only when the wrapper phrase stands alone: an informative
        # qualifier in front, e.g. "baseplate phage protein", fails the
        # anchored regex and falls through untouched.
        if _is_phage_wrapper_only(fs_desc) and is_informative(p_desc):
            best_desc = _apply_final_upgrades(p_desc, top3)
            best_cat  = _infer_function_cat(best_desc, p_cat if is_informative(p_cat) else "unknown function")
            return (
                best_desc, best_cat,
                "phold",
                "auto_merge_suppress_fs_wrapper",
                False,
                best_desc,
                f"FoldSeek hit '{fs_desc}' is a near-uninformative phage-protein "
                f"wrapper (no function beyond 'this is a phage gene'); kept "
                f"PHold's specific call '{p_desc}' (cat='{p_cat}'). "
                f"[keep-phold: suppress-fs-wrapper]{_defense_note}",
            )

        # [merge: fs-specificity-upgrade] PHold gives a generic PHROG-category-
        # level structural term (e.g. "tail protein", "endolysin") and >=2 of
        # FS's top-3 hits mutually converge on the same specific named gene
        # product (e.g. both top-3 entries mention "gp46"). Not a disagreement
        # -- FS is just more precise. Merge as "<phold generic> (<FS specific>)".
        if is_informative(p_desc) and _GENERIC_STRUCTURAL_TERMS.match(p_desc):
            fs_specific = _fs_top3_concordant_specific(top3)
            # A cell-wall binding domain ('Cell wall-binding protein', 'Peptidoglycan
            # binding domain ...') is part of PHold's endolysin call, not a more
            # specific name for it: keep PHold's name. A receptor-binding protein
            # on a tail call IS more specific and is still merged.
            if fs_specific and _CELL_WALL_BINDING.search(fs_specific):
                best_desc = _apply_final_upgrades(p_desc, top3)
                best_cat  = _infer_function_cat(best_desc, p_cat if is_informative(p_cat) else "unknown function")
                return (
                    best_desc, best_cat,
                    "phold",
                    "auto_keep_phold_fs_binding_domain",
                    False,
                    best_desc,
                    f"FoldSeek's top-3 converge on '{fs_specific}', a binding domain of "
                    f"PHold's call '{p_desc}' rather than a more specific name for it; "
                    f"kept PHold. Top-3 FS: {top3}. [keep-phold: fs-binding-domain]{_defense_note}",
                )
            # A concordant name of another function category ('Cro/CI family
            # transcriptional regulator' on a 'tail protein') does not refine
            # PHold's structural call: keep PHold's name.
            fs_cat = _category_from_text(fs_specific) if fs_specific else ""
            if fs_specific and fs_cat not in ("unknown function", "other",
                                              _category_from_text(p_desc)):
                best_desc = _apply_final_upgrades(p_desc, top3)
                best_cat  = _infer_function_cat(best_desc, p_cat if is_informative(p_cat) else "unknown function")
                return (
                    best_desc, best_cat,
                    "phold",
                    "auto_keep_phold_structural",
                    False,
                    best_desc,
                    f"PHold's structural call '{p_desc}' kept: FoldSeek's top-3 converge on "
                    f"'{fs_specific}' ({fs_cat}), another function category. "
                    f"Top-3 FS: {top3}. [keep-phold: structural-call]{_defense_note}",
                )
            if fs_specific:
                merged = f"{p_desc} ({fs_specific})"
                best_cat = _infer_function_cat(p_desc, p_cat if is_informative(p_cat) else "unknown function")
                return (
                    merged, best_cat,
                    ("both agree" if agreement == "strong" else "merged"),
                    "auto_merge_fs_specificity_upgrade",
                    False,
                    merged,
                    f"PHold's generic structural call '{p_desc}' and FoldSeek's "
                    f"internally-concordant top-3 (independently converging on "
                    f"'{fs_specific}') describe the same protein at different "
                    f"levels of specificity -- not a disagreement. "
                    f"Top-3 FS: {top3}. [merge: fs-specificity-upgrade]{_defense_note}",
                )

        # [keep-phold: top3-corroboration] scan ALL of FS's top-3
        # (hyphen-normalised), not just top-1, for corroboration of PHold's call
        # (PHold "Anti-termination protein Q-like" is confirmed by top-3 #3
        # "Antiterminator Q protein of prophage CP-933K"). Low-confidence phold
        # IS included -- cross-method
        # agreement (phold + any FS top-3 hit naming the same thing) is treated
        # as concordance regardless of phold's own confidence. AraC case: phold
        # low "kinase" + FS top-3 "histidine kinase" -> keep "kinase" (two
        # independent methods agree on kinase). Gated to fs_conf==CONFIDENT so we
        # never compare against an overall-unreliable FS hit. (Per-hit FS
        # confidence isn't in the top-3 string, so we can't require per-entry
        # confidence parity.)
        if fs_conf == "CONFIDENT" and is_informative(p_desc):
            corroborating = _top3_corroborates(p_desc, top3)
            if corroborating:
                best_desc = _apply_final_upgrades(p_desc, top3)
                best_cat  = _infer_function_cat(best_desc, p_cat if is_informative(p_cat) else "unknown function")
                return (
                    best_desc, best_cat,
                    ("both agree" if agreement == "strong" else "merged"),
                    "auto_merge_top3_corroboration",
                    False,
                    best_desc,
                    f"PHold='{p_desc}' (cat='{p_cat}') is corroborated by "
                    f"FoldSeek's top-3 hit '{corroborating}' once hyphen "
                    f"variants are normalised (FS confidence=CONFIDENT). "
                    f"Treating as concordant rather than divergent. "
                    f"Top-3 FS: {top3}. [keep-phold: top3-corroboration]{_defense_note}",
                )

        # [specificity: generic-vs-specific] one side is a PURE generic
        # descriptor ("DNA-binding protein", "membrane protein", ...) and the
        # other names a specific protein/family (e.g. PHold "CII-like
        # transcriptional activator" vs FS "DNA-binding protein (Fragment)") --
        # prefer the specific call, with correct source attribution.
        # Exact-membership check only, so this can never collide with
        # _UPGRADE_RULES' combined-string resolutions (e.g. "DNA helicase / AAA
        # family ATPase" -> "DNA helicase" -- that string contains "/" + extra
        # text and will never match _GENERIC_DESCRIPTORS by exact membership).
        specificity_hit = _generic_vs_specific(p_desc, fs_desc)
        if specificity_hit:
            specific_desc, specific_src = specificity_hit
            generic_desc = fs_desc if specific_src == "phold" else p_desc
            best_desc = _apply_final_upgrades(specific_desc, top3)
            best_cat  = _infer_function_cat(best_desc, p_cat if is_informative(p_cat) else "unknown function")
            return (
                best_desc, best_cat,
                specific_src,
                "auto_merge_specific_over_generic",
                False,
                best_desc,
                f"One side is a pure generic descriptor ('{generic_desc}'), the "
                f"other names a specific protein ('{specific_desc}', "
                f"source={specific_src}); preferring the specific call. "
                f"[specificity: generic-vs-specific]{_defense_note}",
            )
        # ---- end remaining pre-checks ------------------------------------

        # ---- fallback: relatedness-fallback ------------------------------
        # Nothing above resolved the gene. Score both candidates' reliability
        # and, at the end, check whether they actually describe the same
        # biology before deciding to flag for review (see _shares_function
        # below) -- genuinely unrelated disagreements are the only ones that
        # should reach needs_review_divergent.
        fs_strong = (_is_valid_float(fs_score) and float(fs_score) >= 300 and
                     _is_valid_float(fs_evalue) and float(fs_evalue) <= 1e-5)
        p_trusted = p_conf in PHOLD_TRUSTED_CONF

        # Apply fragment/upgrade rules to both candidates
        p_final = _apply_final_upgrades(p_desc, top3)
        fs_final = _apply_final_upgrades(fs_desc, top3)

        # Attribute source based on which evidence the suggestion actually uses.
        # This is the key fix: rows in needs_review still carry proper source
        # attribution so the final table's annotation_source is phold / foldseek /
        # phold+foldseek (not just a generic "flagged" placeholder).
        if fs_strong and p_trusted:
            # Dedup: exact equality OR one description contains the other
            # (e.g. "panB (3-methyl-...)" contains "3-methyl-...") -> use the longer
            if p_final == fs_final:
                suggestion = p_final
                divergent_source = ("both agree" if agreement == "strong" else "merged")   # same call from both
            elif fs_final.lower() in p_final.lower():
                suggestion = p_final   # p_final is more specific
                divergent_source = ("both agree" if agreement == "strong" else "merged")
            elif p_final.lower() in fs_final.lower():
                suggestion = fs_final  # fs_final is more specific
                divergent_source = ("both agree" if agreement == "strong" else "merged")
            else:
                suggestion = f"{p_final} / {fs_final}"
                divergent_source = ("both agree" if agreement == "strong" else "merged")   # combined annotation
            explanation = (
                f"DIVERGENT: both PHold (conf={p_conf}) and FoldSeek "
                f"(score={fs_score:.0f}, evalue={fs_evalue:.2e}) are informative but disagree. "
                f"PHold='{p_desc}' (cat={p_cat}), FS='{fs_desc}' ({fs_taxname}). "
                f"Top-3 FS: {top3}. Suggestion: '{suggestion}' "
                f"(review if they describe the same function)."
                f"{_defense_note}"
            )
        elif fs_strong and not p_trusted:
            suggestion = fs_final
            divergent_source = "foldseek"
            explanation = (
                f"DIVERGENT: FoldSeek (score={fs_score:.0f}, evalue={fs_evalue:.2e}) "
                f"more reliable (PHold conf='{p_conf}'). "
                f"PHold='{p_desc}', FS='{fs_desc}'. "
                f"Suggestion: use FoldSeek '{fs_final}'."
                f"{_defense_note}"
            )
        elif p_trusted and not fs_strong:
            suggestion = p_final
            divergent_source = "phold"
            explanation = (
                f"DIVERGENT: PHold conf='{p_conf}' more reliable "
                f"(FS score={fs_score}, evalue={fs_evalue}). "
                f"PHold='{p_desc}' (cat={p_cat}), FS='{fs_desc}' ({fs_taxname}). "
                f"Top-3 FS: {top3}. Suggestion: use PHold '{p_final}'."
                f"{_defense_note}"
            )
        else:
            # Neither source is clearly stronger -- prefer PHold as more
            # phage-specific, but use FS if it is the only informative call.
            if is_informative(fs_desc) and not is_informative(p_desc):
                suggestion = fs_final
                divergent_source = "foldseek"
            else:
                suggestion = p_final
                divergent_source = "phold"
            explanation = (
                f"DIVERGENT -- low confidence both sources. "
                f"PHold='{p_desc}' (conf={p_conf}), "
                f"FS='{fs_desc}' (score={fs_score}, evalue={fs_evalue}, "
                f"taxname={fs_taxname}). "
                f"Top-3 FS: {top3}. Manual review strongly recommended."
                f"{_defense_note}"
            )
        # Apply upgrade rules to the assembled suggestion (catches combined strings
        # like "DNA helicase / AAA family ATPase" and "minor tail / TIGR04255")
        suggestion = _apply_final_upgrades(suggestion, top3)
        final_cat = _infer_function_cat(suggestion, p_cat if is_informative(p_cat) else "unknown function")

        # relatedness-fallback only REVIEWs genuine disagreements. If phold & FS (top-1 or any
        # top-3) share a specific functional token, OR phold's category matches FS,
        # they aren't really divergent -> mark "both agree" and DON'T flag review
        # (big reducer of the review file). Genuine unrelated disagreements stay flagged.
        _STOP = {"protein", "domain", "family", "like", "dna", "rna", "binding",
                 "putative", "phage", "prophage", "type", "system", "containing",
                 "subunit", "terminal", "associated", "related", "homolog"}
        def _split_tokens(s):
            # split on every non-alphanumeric WITHOUT hyphen-merging, so compound
            # enzyme names keep their parts ('metallo','protease') for the
            # substring/prefix tests below.
            return {t for t in re.split(r"[^a-z0-9]+", str(s).lower())
                    if t and t not in _STOP}
        def _common_prefix_len(x, y):
            n = 0
            for cx, cy in zip(x, y):
                if cx != cy:
                    break
                n += 1
            return n
        def _shares_function(a, b):
            # 1) hyphen-normalised token overlap (anti-term ~ antitermination).
            ta = tokenize(_normalize_hyphens(a)) - _STOP
            tb = tokenize(_normalize_hyphens(b)) - _STOP
            if ta & tb:
                return True
            # 2) split-token substring OR shared-prefix>=6 -- catches compound and
            # typo'd enzyme words that hyphen-merge would otherwise hide:
            # metallo-protease ~ metallopeptidase ('metallo' prefix of both),
            # deoxyribosyltransferase ~ "Deoxyribosyltansferase" (typo, 13-char prefix).
            sa, sb = _split_tokens(a), _split_tokens(b)
            for x in sa:
                if len(x) < 5:
                    continue
                for y in sb:
                    if len(y) < 5:
                        continue
                    if x in y or y in x or _common_prefix_len(x, y) >= 6:
                        return True
            # 3) synonym groups sourced from COMPLEMENTARY_CATEGORY_MAP value-lists
            # (reuse the curated map symmetrically; category label ignored here):
            # Fe-S~oxidoreductase, glycosidase~amidase, ParA~Soj, Fur~LexA, etc.
            la, lb = str(a).lower(), str(b).lower()
            for _kws in COMPLEMENTARY_CATEGORY_MAP.values():
                if any(k in la for k in _kws) and any(k in lb for k in _kws):
                    return True
            return False
        _t3_share = any(_shares_function(p_desc, t) for t in str(top3).split("|"))
        related = (is_informative(p_desc) and is_informative(fs_desc) and
                   (_shares_function(p_desc, fs_desc) or _t3_share
                    or is_complementary(p_cat, fs_desc)))
        if related:
            # both methods point at the same biology -> keep the single cleaner
            # phold name instead of a verbose "phold / foldseek" string
            # (e.g. "metallo-protease", not "metallo-protease / Phage metallopeptidase...").
            if is_informative(p_final):
                suggestion = p_final
                final_cat  = _infer_function_cat(
                    suggestion, p_cat if is_informative(p_cat) else final_cat)
            divergent_source = "both agree" if suggestion in (p_final, fs_final) else "merged"
        # A high-confidence phold call is not put in doubt by a FoldSeek hit that is
        # only GOOD and far weaker (Arc repressor e 2e-16 vs 'Phage portal protein'
        # e 0.005 on 108 aa): phold's name is kept without review.
        # phold 'high' can rest on identity or ProstT5 alone, so its e-value must
        # also beat FoldSeek's by >= 5 orders of magnitude.
        if (not related and p_conf == "high" and fs_conf == "GOOD"
                and _is_valid_float(p_evalue) and _is_valid_float(fs_evalue)
                and float(p_evalue) <= float(fs_evalue) * 1e-5):
            return (
                p_final, _infer_function_cat(p_final, p_cat if is_informative(p_cat) else final_cat),
                "phold", "auto_phold_over_weak_fs", False, p_final,
                f"{explanation} High-confidence phold vs a GOOD (not CONFIDENT) "
                f"FoldSeek hit; kept phold '{p_final}' without review. "
                f"[keep-phold: high-phold-guard]",
            )
        # Only genuine divergence goes to review; a defense call by itself is not
        # a doubt (defense annotation is kept regardless).
        flag_review = not related
        action = "auto_merge_related" if related else "needs_review_divergent"
        return (
            suggestion, final_cat,
            divergent_source,
            action,
            flag_review,
            suggestion,
            f"{explanation} [fallback: relatedness-fallback]",
        )

    # -- Case 6: both_uninformative -------------------------------------------
    # Distinguish TRUE dark matter (no FoldSeek hit at all) from a hit that exists
    # but is uninformative (DUF/empty/below-threshold) -> separate annotation_source
    # so diagnostics can tell "nothing found" from "found a fold, no functional name".
    fs_had_hit = (str(fs_conf) not in ("NO_HIT", "", "nan", "none", "None")) or bool(_fs_inf_raw)
    src = "no_informative_hit" if fs_had_hit else "no_hit"
    weak_note = (f" FoldSeek returned a WEAK/unreliable hit ({fs_desc}) -- filtered out."
                 if _fs_inf_raw and fs_conf in ("WEAK",) and is_informative(fs_desc) else "")
    return (
        "hypothetical protein", "unknown function",
        src,
        "both_uninformative",
        False,
        "hypothetical protein",
        f"Neither PHold nor FoldSeek found an informative annotation.{weak_note}",
    )


def _is_valid_float(v) -> bool:
    import math
    try:
        f = float(v)
        return not math.isnan(f) and not math.isinf(f)
    except (TypeError, ValueError):
        return False


# -----------------------------------------------------------------------------
# MAIN
# -----------------------------------------------------------------------------

def main():
    section("STEP 04 -- CURATE ANNOTATIONS")
    CURATION_DIR.mkdir(parents=True, exist_ok=True)

    # -- Load comparison table -------------------------------------------------
    comp_path = COMPARISON_DIR / "comparison_per_gene.csv"
    if not comp_path.exists():
        log(f"ERROR: {comp_path} not found. Run 03_compare_annotations.py first.")
        sys.exit(1)

    comp = pd.read_csv(str(comp_path))
    log(f"Loaded comparison table: {len(comp)} genes")
    _load_top3_hits()
    log(f"Columns: {list(comp.columns)}")

    # -- Apply curation logic --------------------------------------------------
    section("APPLYING CURATION DECISION TREE")

    def _apply_relevant_domain(final_desc, source, action, flag, row):
        """Post-process: if FoldSeek named a characterized domain (SPOR/PIN/...),
        a domain hit can't be more specific than phold. Keep phold's product with
        the domain in brackets when phold is informative, else use the domain's
        characterized activity. Always flag for review. (see lexicon.py section G)
        Gated on the FINAL call being a bare domain, so a confident non-domain
        phold/merge call is never downgraded."""
        hit = relevant_domain(final_desc)
        if not hit:
            return final_desc, source, action, flag
        dom, activity = hit
        p_desc = clean_str(row.get("phold_product", ""))
        if is_informative(p_desc) and not _is_phage_wrapper_only(p_desc):
            new_src = source if source in ("phold", "merged", "both agree") else "merged"
            return f"{p_desc} ({dom} domain)", new_src, "relevant_domain_review", True
        return f"{activity} ({dom} domain)", "foldseek", "relevant_domain_review", True

    results = []
    for _, row in comp.iterrows():
        (final_desc, final_cat, source, action,
         flag, suggestion, explanation) = merge_annotations(row)
        final_desc, source, action, flag = _apply_relevant_domain(
            final_desc, source, action, flag, row)
        if action == "relevant_domain_review":
            suggestion = final_desc   # keep the review suggestion in sync
        # [review: head-tail-conflict] PHold places the protein on the head and
        # FoldSeek on the tail (or the reverse): keep PHold's name (also as the
        # review suggestion, which step 05 uses as the final name) and send to
        # review; the FoldSeek name is in the explanation and in fs_description
        _ht = _head_tail_conflict(row.get("phold_product", ""), final_desc)
        if _ht:
            p_name = clean_str(row.get("phold_product", ""))
            explanation = (f"PHold '{p_name}' ({_virion_part(p_name)}) and FoldSeek '{_ht}' "
                           f"({_virion_part(_ht)}) place the protein in different virion "
                           f"parts; kept PHold's name, FoldSeek's is the alternative. "
                           f"Was: {final_desc}. [review: head-tail-conflict]")
            final_desc, source, action, flag, suggestion = (
                p_name, "phold", "head_tail_conflict_review", True, p_name)
            final_cat = _infer_function_cat(p_name, final_cat)

        results.append({
            **row.to_dict(),   # keep all comparison columns
            "final_product":         final_desc,
            "final_function":        final_cat,
            "best_source":           source,
            "curation_action":       action,
            "needs_review":          flag,
            "curation_suggestion":   suggestion,
            "curation_explanation":  explanation,
            "final_annotation":      "",  # <- reviewer fills this in for needs_review rows
        })

    curated = pd.DataFrame(results)

    # -- Split into auto-curated and needs-review ------------------------------
    auto_curated = curated[curated["needs_review"] == False].copy()
    needs_review = curated[curated["needs_review"] == True].copy()

    # For needs_review, pre-fill final_annotation with curation_suggestion so
    # the reviewer can simply accept or edit inline
    needs_review["final_annotation"] = needs_review["curation_suggestion"]

    # -- Statistics ------------------------------------------------------------
    section("CURATION STATISTICS")
    n_total = len(curated)
    n_auto  = len(auto_curated)
    n_flag  = len(needs_review)

    log(f"Total hypothetical genes  : {n_total}")
    log(f"Auto-curated (no review)  : {n_auto}  ({100*n_auto//n_total}%)")
    log(f"Flagged for review        : {n_flag}  ({100*n_flag//n_total}%)")

    log("\nAuto-curated action breakdown:")
    log(auto_curated["curation_action"].value_counts().to_string())

    if n_flag > 0:
        log(f"\nFlagged genes ({n_flag}):")
        for _, row in needs_review.iterrows():
            log(f"  {row['locus_tag']} ({row['prophage']}): "
                f"phold='{str(row['phold_product'])[:40]}' vs "
                f"fs='{str(row['foldseek_description'])[:40]}'")

    # Informativeness after curation
    n_now_inf = curated["final_product"].apply(is_informative).sum()
    n_still_hypo = (curated["final_product"] == "hypothetical protein").sum()
    log(f"\nAfter curation:")
    log(f"  Genes with informative annotation : {n_now_inf} / {n_total}  "
        f"({100*n_now_inf//n_total}%)")
    log(f"  Still 'hypothetical protein'      : {n_still_hypo} / {n_total}")

    # Per-prophage annotation rate
    log("\nPer-prophage informativeness after curation:")
    pp = curated.groupby("prophage").apply(
        lambda g: pd.Series({
            "n": len(g),
            "annotated": g["final_product"].apply(is_informative).sum(),
        })
    )
    if not pp.empty and "annotated" in pp.columns:
        pp["pct"] = (100 * pp["annotated"] / pp["n"]).round(0).astype(int)
        log(pp.to_string())
    else:
        log("  (no per-prophage data — prophage column may be unset)")

    # -- Save outputs ---------------------------------------------------------
    # Emit ONE combined curated file (all genes + needs_review flag) into the
    # shared general-output folder (04_output). step05 reads this and emits
    # the single review_suggested.csv + final table there -- no duplicate review file.
    CURATION_DIR.mkdir(parents=True, exist_ok=True)
    curated_out = CURATION_DIR / "curated_annotations.csv"

    # final_annotation: review rows keep curation_suggestion (reviewer edits), auto rows = final_product
    curated = curated.copy()
    curated["final_annotation"] = curated.apply(
        lambda r: (r["curation_suggestion"] if r["needs_review"] else r["final_product"]), axis=1)

    # Reorder columns so the curated file reads like the final table (key identity +
    # decision columns first, then phold evidence, then foldseek evidence, then rest).
    _lead = ["prophage", "locus_tag", "aa_length", "final_product", "final_function",
             "final_annotation", "best_source", "needs_review", "curation_action",
             "agreement", "pharokka_function"]
    _phold = [c for c in curated.columns if c.startswith("phold_") or c.startswith("subdb_")]
    _fs    = [c for c in curated.columns if c.startswith("foldseek_") or c.startswith("fuzzy")]
    _ai    = [c for c in curated.columns if c in ("curation_suggestion", "curation_explanation")]
    _seen  = set(_lead + _phold + _fs + _ai)
    _rest  = [c for c in curated.columns if c not in _seen]
    _order = [c for c in (_lead + _phold + _fs + _rest + _ai) if c in curated.columns]
    curated = curated[_order]

    curated.to_csv(str(curated_out), index=False)
    log(f"\ncurated_annotations.csv -> {curated_out}  ({len(curated)} rows, "
        f"{len(needs_review)} flagged for review; review_suggested.csv written by step05)")

    # -- Instructions for manual review ----------------------------------------
    if n_flag > 0:
        section("MANUAL REVIEW INSTRUCTIONS")
        log(f"{n_flag} gene(s) flagged. step05 writes the review subset to:")
        log(f"  {CURATION_DIR / 'review_suggested.csv'}")
        log(f"")
        log(f"To adjust a flagged call: edit the 'final_annotation' column in")
        log(f"  {curated_out}  (needs_review==True rows), then re-run step05.")
        log(f"  curation_suggestion is pre-filled; curation_explanation gives the reasoning.")
        log(f"  Key evidence: phold_product/confidence/evalue, foldseek_description/")
        log(f"  score/evalue/pident, foldseek_top3.")
        log(f"  -> Run: python scripts/05_build_output.py")
    else:
        log("\nNo manual review needed!")
        log("-> Run: python scripts/05_build_output.py")

if __name__ == "__main__":
    main()
