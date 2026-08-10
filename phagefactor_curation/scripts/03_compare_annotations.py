#!/usr/bin/env python3
"""
03_compare_annotations.py
==========================
Compare PHold vs custom FoldSeek annotations for hypothetical proteins.

Inputs:
  01_phold/combined/phold_all.tsv         (all 8 prophages, merged)
  02_foldseek/foldseek_best_hit.csv       (best FS hit per gene)
  02_foldseek/foldseek_top3.csv           (top 3 FS hits per gene)
  input/split/gene_metadata.csv           (to identify which genes are hypothetical)

Outputs:
  03_comparison/phold_hypotheticals.csv   -> PHold results filtered to hypo genes
  03_comparison/foldseek_formatted.csv    -> FS results with standardised columns
  03_comparison/comparison_per_gene.csv   -> full 2-source comparison table

The comparison table has these key columns:
  prophage, locus_tag, phold_product, phold_function_category, phold_confidence,
  phold_evalue, phold_bitscore, phold_phrog, phold_inf,
  subdb_source, subdb_name,   (clean structured ACR/VFDB/CARD/NetFlaX/DefenseFinder
                               identity, joined from sub_db_tophits/*.tsv -- see
                               _load_subdb_hits())
  foldseek_description, foldseek_accession, foldseek_evalue, foldseek_score,
  foldseek_pident, foldseek_qcov, foldseek_taxname, foldseek_inf,
  fuzzy_score, agreement

Usage:
  cd phagefactor/
  python scripts/03_compare_annotations.py
"""

import re
import sys
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
    VIRUS_KEYWORDS as _VIRUS_KEYWORDS,
    ARCHAEA_KEYWORDS as _ARCHAEA_KEYWORDS,
    BACTERIA_KEYWORDS as _BACTERIA_KEYWORDS,
    is_eukaryotic_taxname as _is_eukaryotic_taxname,
    COMPLEMENTARY_CATEGORY_MAP,
)

from config import (
    GENE_METADATA_CSV,
    PHOLD_COMBINED_TSV,
    PHOLD_OUT_DIR,
    FOLDSEEK_BEST_HIT, FOLDSEEK_TOP3,
    COMPARISON_DIR,
    FUZZY_STRONG_THRESHOLD, FUZZY_PARTIAL_THRESHOLD,
    FOLDSEEK_EVALUE_MAX, FOLDSEEK_SCORE_OVERRIDE,
    PHOLD_TRUSTED_CONF, PHOLD_WEAK_CONF,
    HOST_GENUS,
)
from utils import (
    log, section, clean_str, is_informative, fuzzy_score,
    load_phold_tsv,
)

try:
    import pandas as pd
except ImportError:
    print("pandas required.")
    sys.exit(1)


# -----------------------------------------------------------------------------
# HELPERS
# -----------------------------------------------------------------------------

def _scalar(series_or_val, fallback="NA"):
    """Return first element if Series, else the value itself."""
    if hasattr(series_or_val, "iloc"):
        v = series_or_val.iloc[0] if len(series_or_val) > 0 else fallback
    else:
        v = series_or_val
    return v


def _clean_phold_product(val) -> str:
    """Clean a PHold product/function value."""
    s = clean_str(_scalar(val))
    # PHold uses 'hypothetical protein' for no-hit genes and 'unknown function' for function
    return s


# -----------------------------------------------------------------------------
# SUB-DATABASE TOPHIT INTEGRATION  (ACR / VFDB / CARD / NetFlaX / DefenseFinder)
# -----------------------------------------------------------------------------
# phold's combined/global top1 search sometimes picks a hit from one of these
# specialised sub-DBs but only records a GENERIC placeholder string in `product`
# (e.g. "VFDB virulence factor protein", "CARD resistance protein", or leaves
# it blank for DefenseFinder) -- the real structured identity (gene name, ARO
# name, vf_name, system name, ...) lives in the per-sub-DB
# `sub_db_tophits/{db}_cds_predictions.tsv` files and would otherwise be lost.
# It also means genes whose sub-DB hit did NOT win the global top1 race are
# entirely invisible in phold_all -- reading these files directly is the only
# way to surface that evidence at all.
#
# Output layout differs between modes, so we glob recursively under PHOLD_OUT_DIR:
#   genome mode  : 01_phold/<PROPHAGE>/sub_db_tophits/*_cds_predictions.tsv
#   protein mode : 01_phold/proteins/compare/sub_db_tophits/*_cds_predictions.tsv

_SUBDB_FILE_RE = re.compile(r'^(acr|vfdb|card|netflax|defensefinder)_cds_predictions\.tsv$', re.I)


def _strip_prophage_prefix(cds_id: str) -> str:
    """'phiNP:fig|...' -> 'fig|...'; plain genome-mode IDs (no ':') pass through."""
    return cds_id.split(":", 1)[1] if isinstance(cds_id, str) and ":" in cds_id else cds_id


def _format_subdb_name(source: str, row) -> str:
    """Build one clean, structured description string from a sub-DB tophit row."""
    g = lambda col: clean_str(row.get(col, "NA"))

    if source == "defensefinder":
        system  = g("System")
        subtype = g("subtype")
        gene    = g("gene_name")
        label = subtype.replace("_", " ").strip() if is_informative(subtype) else system
        if not is_informative(label):
            label = system
        name = f"{label} defense system protein" if is_informative(label) else "defense system protein"
        if is_informative(gene):
            name += f" ({gene})"
        return name

    if source == "card":
        aro    = g("ARO Name")
        family = g("AMR Gene Family")
        if is_informative(aro) and is_informative(family):
            return f"{aro} ({family})"
        if is_informative(aro):
            return aro
        return family if is_informative(family) else "CARD resistance protein"

    if source == "vfdb":
        vf_name = g("vf_name")
        vf_cat  = g("vf_category")
        desc    = g("description")
        # VFDB's own `description` field is sometimes junk/truncated (e.g. a
        # 2-character fragment) -> treat a too-short description as
        # uninformative so we fall back to vf_name instead of pasting garbage.
        short_nm = g("short_name")
        if is_informative(desc) and len(desc.strip()) <= 3:
            desc = "NA"
        if is_informative(vf_name) and is_informative(desc):
            label = f"{vf_name} ({vf_cat})" if is_informative(vf_cat) else vf_name
            return f"{label}: {desc}"
        if is_informative(vf_name):
            # append the VFDB short gene symbol when present (e.g. "Ibes (ibeB)")
            return f"{vf_name} ({short_nm})" if is_informative(short_nm) else vf_name
        return desc if is_informative(desc) else "VFDB virulence factor protein"

    if source == "netflax":
        typ     = g("type")
        partner = g("partner")
        if is_informative(typ):
            note = f" (partner: {partner})" if is_informative(partner) else ""
            return f"NetFlaX {typ} protein{note}"
        return "NetFlaX toxin-antitoxin system protein"

    if source == "acr":
        for col in ("gene_name", "protein", "acr_name", "name"):
            v = g(col)
            if is_informative(v):
                return f"{v} (anti-CRISPR protein)"
        return "Anti-CRISPR (Acr) protein"

    return ""


def _load_subdb_hits(phold_out_dir: Path) -> "pd.DataFrame":
    """
    Read every sub_db_tophits/{acr,vfdb,card,netflax,defensefinder}_cds_predictions.tsv
    found anywhere under phold_out_dir, strip the prophage prefix from cds_id, and
    build one row per gene with a clean structured name.

    Returns columns: locus_tag, subdb_source, subdb_name, subdb_bitscore, subdb_evalue
    (empty DataFrame with these columns if nothing is found / all sub-DBs are empty).
    """
    rows = []
    for tsv in sorted(phold_out_dir.glob("**/sub_db_tophits/*_cds_predictions.tsv")):
        m = _SUBDB_FILE_RE.match(tsv.name)
        if not m:
            continue
        source = m.group(1).lower()
        try:
            df = pd.read_csv(str(tsv), sep="\t", low_memory=False)
        except Exception as e:
            log(f"  WARNING: could not read {tsv}: {e}")
            continue
        if df.empty or "cds_id" not in df.columns:
            continue
        for _, r in df.iterrows():
            locus_tag = _strip_prophage_prefix(clean_str(r.get("cds_id", "")))
            if not locus_tag or locus_tag == "NA":
                continue
            name = _format_subdb_name(source, r)
            if not is_informative(name):
                continue
            rows.append({
                "locus_tag":      locus_tag,
                "subdb_source":   source,
                "subdb_name":     name,
                "subdb_bitscore": _safe_float(r.get("bitscore")),
                "subdb_evalue":   _safe_float(r.get("evalue")),
            })

    cols = ["locus_tag", "subdb_source", "subdb_name", "subdb_bitscore", "subdb_evalue"]
    if not rows:
        return pd.DataFrame(columns=cols)

    out = pd.DataFrame(rows)
    # A gene could in principle have hits in >1 sub-DB (or appear in multiple
    # per-prophage dirs in genome mode); keep the highest-bitscore hit per gene.
    out = out.sort_values("subdb_bitscore", ascending=False, na_position="last")
    out = out.drop_duplicates(subset="locus_tag", keep="first").reset_index(drop=True)
    return out[cols]


def _safe_float(val) -> float:
    """Convert to float, return None on failure."""
    try:
        v = _scalar(val)
        if v is None or (isinstance(v, float) and pd.isna(v)):
            return None
        return float(v)
    except (TypeError, ValueError):
        return None


# -----------------------------------------------------------------------------
# EUKARYOTIC KINGDOM FILTER
# -----------------------------------------------------------------------------
# Rationale: horizontal gene transfer from eukaryotes to phages is essentially
# non-existent (no biological mechanism). A FoldSeek hit that exists only in
# eukaryotes -- with no bacterial/archaeal/viral representative in the top hits --
# is almost always convergent structural similarity, not true homology.
# The Foldseek databases (PDB, AFDB, AFDB-SwissProt) are heavily biased toward
# human/mouse/yeast structures, so any random helix bundle in a phage protein
# will find some eukaryotic fold match. We demote such hits one step.
# -----------------------------------------------------------------------------

# Keyword-based kingdom assignment (no NCBI taxonomy dependency).
# Bacterial genera below are an inclusive but not exhaustive set; the lookup
# is conservative -- when in doubt it returns "Unknown" rather than guessing.
# Extend as needed if you see common genera being missed in your hits.
# _BACTERIA_KEYWORDS is now defined ONCE in lexicon.py as BACTERIA_KEYWORDS,
# imported at the top of this file. The rationale above still
# applies -- only the definition moved.
# _ARCHAEA_KEYWORDS is now defined ONCE in lexicon.py as ARCHAEA_KEYWORDS,
# imported at the top of this file. The rationale above still
# applies -- only the definition moved.
# _VIRUS_KEYWORDS is now defined ONCE in lexicon.py as VIRUS_KEYWORDS,
# imported at the top of this file. The rationale above still
# applies -- only the definition moved.
# The eukaryote list is defined ONCE in lexicon.py as EUKARYOTE_KEYWORDS and
# matched by lexicon.is_eukaryotic_taxname() (word boundaries, not substrings --
# 'Campylobacter conciSUS' is not a pig). 02d uses the same predicate.


def _kingdom_from_taxname(taxname) -> str:
    """
    Classify a Foldseek taxname into Bacteria / Archaea / Virus / Eukaryote / Unknown.

    Uses inclusive keyword matching on the first word (genus) and the full string.
    Conservative -- unknown taxa return "Unknown" rather than being misclassified.

    This is intentionally a keyword heuristic (no NCBI taxdump dependency).
    The Foldseek output only carries taxname (no taxid), so we make do with names.
    """
    if not taxname or not isinstance(taxname, str):
        return "Unknown"
    t = taxname.strip().lower()
    if not t or t in ("na", "nan", "unknown", "none", ""):
        return "Unknown"

    # Virus check first -- some virus names contain bacterial genus names
    # (e.g. "Campylobacter virus CP21" should be Virus, not Bacteria)
    if any(kw in t for kw in _VIRUS_KEYWORDS):
        return "Virus"

    # Check first word (genus) + full string for the others
    first = t.split()[0] if t.split() else t
    if first in _BACTERIA_KEYWORDS or any(kw in t for kw in ("bacteri", "proteobacter")):
        return "Bacteria"
    if first in _ARCHAEA_KEYWORDS or "archae" in t:
        return "Archaea"
    # Same predicate 02d uses for its parse-time filter -- one list, one matcher.
    if _is_eukaryotic_taxname(t):
        return "Eukaryote"
    return "Unknown"


def _apply_eukaryotic_demote(fs_confidence: str, best_kingdom: str,
                              top_kingdoms) -> tuple:
    """
    Apply the eukaryotic kingdom filter (taxonomy-only).

    If the best Foldseek hit is Eukaryote AND no Bacteria/Archaea/Virus appears
    in the top hits, demote the confidence one step. (Description-euka handling
    lives in 04_curate's euka-substitute gate, which prefers a bacterial top-3
    hit rather than demoting.)

    Demote one step: CONFIDENT/GOOD -> BORDERLINE, BORDERLINE -> WEAK.
    Returns (new_confidence, was_demoted: bool, reason: str).
    """
    if best_kingdom != "Eukaryote":
        return fs_confidence, False, ""
    if {k for k in top_kingdoms if k in ("Bacteria", "Archaea", "Virus")}:
        return fs_confidence, False, ""
    DEMOTE = {
        "CONFIDENT":  "BORDERLINE",
        "GOOD":       "BORDERLINE",
        "BORDERLINE": "WEAK",
        "WEAK":       "WEAK",
        "NO_HIT":     "NO_HIT",
    }
    new_conf = DEMOTE.get(fs_confidence.upper() if isinstance(fs_confidence, str) else "NO_HIT",
                          fs_confidence)
    reason = (f"eukaryotic-only top hits (best={best_kingdom}); "
              f"demoted {fs_confidence} -> {new_conf}")
    return new_conf, True, reason


def _is_complementary(p_function_cat: str, fs_desc: str) -> bool:
    """
    Return True if the PHold function category and the FoldSeek description are
    functionally complementary according to COMPLEMENTARY_CATEGORY_MAP.
    E.g. phold_function_cat="lysis" + fs_desc containing "endolysin" -> True.
    """
    if not p_function_cat or not fs_desc:
        return False
    p_cat = p_function_cat.lower().strip()
    fs_lower = fs_desc.lower()
    for cat, keywords in COMPLEMENTARY_CATEGORY_MAP.items():
        cat_lower = cat.lower()
        # Match if phold category contains the map key or vice-versa
        if cat_lower in p_cat or p_cat in cat_lower:
            for kw in keywords:
                if kw.lower() in fs_lower:
                    return True
    return False


def classify_agreement(p_desc, fs_desc, p_inf, fs_inf,
                        p_conf, p_function_cat="",
                        top3_descs=None) -> tuple:
    """
    Classify the agreement between PHold and FoldSeek annotations.

    Returns (agreement_label: str, fuzzy: float | None)

    Agreement labels:
      strong           : Jaccard >= FUZZY_STRONG_THRESHOLD (default 0.35), both informative
      partial          : Jaccard >= FUZZY_PARTIAL_THRESHOLD (default 0.08), both informative
      complementary    : Jaccard below partial threshold but PHold category matches FS
                         description (best hit OR any top3 entry)
      different        : both informative, Jaccard below partial, no category match
      phold_only       : PHold informative, FS uninformative (or no FS hit)
      foldseek_only    : FS informative, PHold uninformative
      both_uninformative : neither is informative
    """
    if p_inf and fs_inf:
        score = fuzzy_score(p_desc, fs_desc)
        if score >= FUZZY_STRONG_THRESHOLD:
            return "strong", round(score, 3)
        # Complementary checked before partial: function-category agreement
        # is more informative than partial lexical overlap.
        check_descs = [fs_desc] + (list(top3_descs) if top3_descs else [])
        if any(_is_complementary(p_function_cat, d) for d in check_descs if d):
            return "complementary", round(score, 3)
        if score >= FUZZY_PARTIAL_THRESHOLD:
            return "partial", round(score, 3)
        return "different", round(score, 3)
    elif p_inf and not fs_inf:
        return "phold_only", None
    elif not p_inf and fs_inf:
        return "foldseek_only", None
    else:
        return "both_uninformative", None


# -----------------------------------------------------------------------------
# MAIN
# -----------------------------------------------------------------------------

def main():
    section("STEP 03 -- COMPARE PHOLD vs FOLDSEEK ANNOTATIONS")
    COMPARISON_DIR.mkdir(parents=True, exist_ok=True)

    # -- Load gene metadata ----------------------------------------------------
    if not GENE_METADATA_CSV.exists():
        log(f"ERROR: {GENE_METADATA_CSV} not found. Run 00_split_input.py first.")
        sys.exit(1)
    metadata = pd.read_csv(str(GENE_METADATA_CSV))
    hypo_meta = metadata[metadata["is_hypothetical"] == True].copy()
    hypo_locus_tags = set(hypo_meta["locus_tag"])
    log(f"Gene metadata loaded: {len(metadata)} total, {len(hypo_locus_tags)} hypothetical")

    # -- Load PHold combined TSV -----------------------------------------------
    if not PHOLD_COMBINED_TSV.exists():
        log(f"ERROR: {PHOLD_COMBINED_TSV} not found. Run 01_run_phold_local.sh + 01c_merge_phold.py first.")
        sys.exit(1)
    phold_all = pd.read_csv(str(PHOLD_COMBINED_TSV), sep="\t", low_memory=False)
    log(f"PHold TSV loaded: {len(phold_all)} rows")

    # Rename cds_id -> locus_tag for consistent joining
    if "cds_id" in phold_all.columns:
        phold_all = phold_all.rename(columns={"cds_id": "locus_tag"})
    elif "locus_tag" not in phold_all.columns:
        log("ERROR: PHold TSV has neither 'cds_id' nor 'locus_tag' column.")
        sys.exit(1)

    # Strip prophage prefix if present (e.g. "phiNP:fig|67825X47XpegX1887" → "fig|67825X47XpegX1887").
    # In protein mode (01p_phold_proteins.sh) input FASTAs are prefixed with the prophage name
    # so phold inherits that prefix in its cds_id output.  In genome mode phold locus_tags are
    # plain (e.g. "WRIRHRXM_CDS_0009") and contain no ":", so this is a no-op there.
    phold_all["locus_tag"] = phold_all["locus_tag"].apply(
        lambda x: x.split(":", 1)[1] if isinstance(x, str) and ":" in x else x
    )

    # -- Validate: check that PHold locus_tags match gene_metadata ------------
    phold_ids = set(phold_all["locus_tag"])
    meta_ids  = set(metadata["locus_tag"])
    only_in_phold = phold_ids - meta_ids
    only_in_meta  = meta_ids - phold_ids
    if only_in_phold:
        log(f"WARNING: {len(only_in_phold)} locus_tags in PHold output but not in gene_metadata:")
        log(f"  {sorted(only_in_phold)[:5]}...")
    if only_in_meta:
        log(f"WARNING: {len(only_in_meta)} locus_tags in gene_metadata but not in PHold output:")
        log(f"  {sorted(only_in_meta)[:5]}...")
        log(f"  -> These genes had no PHold result (normal for genes with no structure match)")

    # -- Filter PHold to hypothetical genes only -------------------------------
    phold_hypo = phold_all[phold_all["locus_tag"].isin(hypo_locus_tags)].copy()
    log(f"PHold results for hypothetical genes: {len(phold_hypo)}")

    # Write filtered PHold output for inspection
    phold_hypo_out = COMPARISON_DIR / "phold_hypotheticals.csv"
    phold_hypo.to_csv(str(phold_hypo_out), index=False)
    log(f"PHold hypotheticals -> {phold_hypo_out}")

    # -- Load sub-DB tophit evidence (ACR/VFDB/CARD/NetFlaX/DefenseFinder) -----
    section("LOADING SUB-DB TOPHIT EVIDENCE (ACR/VFDB/CARD/NetFlaX/DefenseFinder)")
    subdb_hits = _load_subdb_hits(PHOLD_OUT_DIR)
    if not subdb_hits.empty:
        log(f"Sub-DB tophit evidence loaded: {len(subdb_hits)} genes")
        log(f"  By source: {subdb_hits['subdb_source'].value_counts().to_dict()}")
        subdb_out = COMPARISON_DIR / "subdb_tophits.csv"
        subdb_hits.to_csv(str(subdb_out), index=False)
        log(f"Sub-DB tophits -> {subdb_out}")
    else:
        log("No sub-DB tophit evidence found (acr/vfdb/card/netflax/defensefinder all empty/absent).")
    subdb_idx = subdb_hits.set_index("locus_tag") if not subdb_hits.empty else subdb_hits

    # -- Load FoldSeek best hit ------------------------------------------------
    if not FOLDSEEK_BEST_HIT.exists():
        log(f"ERROR: {FOLDSEEK_BEST_HIT} not found. Run 02b_foldseek_pipeline.py first.")
        sys.exit(1)
    fs_best = pd.read_csv(str(FOLDSEEK_BEST_HIT))
    log(f"FoldSeek best hits loaded: {len(fs_best)} genes")

    # Load top3 for reference
    fs_top3 = pd.read_csv(str(FOLDSEEK_TOP3)) if FOLDSEEK_TOP3.exists() else pd.DataFrame()

    # -- Write FoldSeek formatted CSV ------------------------------------------
    fs_formatted_out = COMPARISON_DIR / "foldseek_formatted.csv"
    fs_best.to_csv(str(fs_formatted_out), index=False)
    log(f"FoldSeek formatted  -> {fs_formatted_out}")

    # -- Index for joining -----------------------------------------------------
    phold_idx = phold_hypo.set_index("locus_tag")
    fs_idx    = fs_best.set_index("gene")

    # -- Build per-gene comparison rows ----------------------------------------
    section("BUILDING COMPARISON TABLE")
    rows = []

    for locus_tag in sorted(hypo_locus_tags):
        # Gene metadata
        meta_row = hypo_meta[hypo_meta["locus_tag"] == locus_tag]
        prophage = meta_row["prophage"].iloc[0] if len(meta_row) > 0 else "unknown"
        meta_function = meta_row["function"].iloc[0] if len(meta_row) > 0 else "unknown function"
        meta_aa_len   = meta_row["aa_length"].iloc[0] if len(meta_row) > 0 else 0

        # PHold data (may be missing if PHold found nothing)
        has_phold = locus_tag in phold_idx.index
        if has_phold:
            pr = phold_idx.loc[locus_tag]
            # Handle duplicate index (take first row if multiple matches)
            def _get(col, fallback=None):
                v = pr[col] if col in pr.index else fallback
                return _scalar(v, fallback)

            p_product  = clean_str(_get("product", "hypothetical protein"))
            p_function = clean_str(_get("function", "unknown function"))
            p_conf     = clean_str(_get("annotation_confidence", "none"))
            p_evalue   = _safe_float(_get("evalue"))
            p_bitscore = _safe_float(_get("bitscore"))
            p_phrog    = clean_str(_get("phrog", "No_PHROG"))
            p_fident   = _safe_float(_get("fident"))
            p_qcov     = _safe_float(_get("qCov"))
            p_method   = clean_str(_get("annotation_method", "none"))
            p_prostt5  = _safe_float(_get("prostt5_confidence"))
            p_tophit   = clean_str(_get("tophit_protein", "NA"))
        else:
            p_product = p_function = p_conf = p_phrog = p_method = p_tophit = "NA"
            p_evalue = p_bitscore = p_fident = p_qcov = p_prostt5 = None

        # Sub-DB tophit evidence (ACR/VFDB/CARD/NetFlaX/DefenseFinder), independent
        # of whether it won phold's global top1 race -- see _load_subdb_hits().
        has_subdb = (not subdb_hits.empty) and (locus_tag in subdb_idx.index)
        if has_subdb:
            sr = subdb_idx.loc[locus_tag]
            # Handle duplicate index defensively (shouldn't occur post drop_duplicates)
            subdb_source = clean_str(_scalar(sr["subdb_source"], "NA"))
            subdb_name   = clean_str(_scalar(sr["subdb_name"], "NA"))
        else:
            subdb_source = "NA"
            subdb_name   = "NA"

        # Sub-DB substitution (standalone, upstream of the curation rule tree
        # in 04_curate_annotations.py -- not part of any group there): copy the
        # sub-DB hit's structured name
        # into phold_product HERE -- during phold handling, before curation/FS
        # comparison -- whenever the sub-DB hit IS phold's own top1 hit (i.e.
        # phold's `phrog` field names the sub-DB source itself --
        # "vfdb"/"card"/"defensefinder"/"netflax"/"acr" -- instead of a real
        # PHROG ID/category; phold only records a GENERIC placeholder in
        # `product` for these wins: "VFDB virulence factor protein", "CARD
        # resistance protein", or blank for DefenseFinder). Substituting here
        # means phold_product / phold_inf / phold_function_cat AND the
        # downstream `agreement` classification (computed below from p_product/
        # p_inf) all see the correct structured identity from the start,
        # rather than only patching a working copy at the curation step
        # (04_curate_annotations.py) after `agreement` has already been
        # computed from the unsubstituted placeholder and routed the gene
        # toward FoldSeek's generic structural-homolog name instead.
        # If the sub-DB hit did NOT win phold's top1 race (2nd place / not the
        # tophit), leave phold_product alone -- subdb_source/subdb_name still
        # surface it in their own columns for info, as before.
        p_phrog_l = p_phrog.lower()
        if has_subdb and is_informative(subdb_name) and subdb_source == p_phrog_l:
            p_product = subdb_name
            if subdb_source in ("defensefinder", "netflax", "acr"):
                # PHold maps these to the generic "moron, auxiliary metabolic
                # gene and host takeover" PHROG category; override to
                # "defense" -- the biologically correct category -- mirroring
                # the equivalent override in 04_curate.merge_annotations.
                p_function = "defense"

        # PHold informative check: product is informative if not "hypothetical protein"
        p_inf = is_informative(p_product) and p_product.lower() != "hypothetical protein"

        # FoldSeek data (may be missing if no structure predicted)
        has_fs = locus_tag in fs_idx.index
        if has_fs:
            fr = fs_idx.loc[locus_tag]
            def _fget(col, fallback=None):
                v = fr[col] if col in fr.index else fallback
                return _scalar(v, fallback)

            fs_desc     = clean_str(_fget("description", "NA"))
            fs_accession= clean_str(_fget("accession", "NA"))
            fs_evalue   = _safe_float(_fget("evalue"))
            fs_score    = _safe_float(_fget("score"))
            fs_pident   = _safe_float(_fget("pident"))
            fs_qcov     = _safe_float(_fget("qcov_aa"))
            fs_taxname  = clean_str(_fget("taxname", "NA"))
            fs_db       = clean_str(_fget("foldseek_subdb", "NA"))  # source DB of best hit
            fs_inf_flag   = _fget("informative_hit_found", False)
            # Use explicit NaN-safe bool conversion (bool(NaN) == True in Python!)
            _sh = _fget("same_host", None)
            # None (no host genus configured) is preserved as None -> NA downstream,
            # not coerced to False (which would look like "not host-derived").
            fs_same_host  = bool(_sh) if (_sh is not None and pd.notna(_sh)) else None
            _df = _fget("defense_flag", None)
            fs_defense    = bool(_df) if (_df is not None and pd.notna(_df)) else False
            fs_confidence = str(_fget("foldseek_confidence", "NO_HIT") or "NO_HIT")
            _pf = _fget("promiscuous_fold_flag", None)
            fs_promiscuous = bool(_pf) if (_pf is not None and pd.notna(_pf)) else False
            _ef = _fget("eukaryotic_desc_flag", None)
            fs_euka_desc  = bool(_ef) if (_ef is not None and pd.notna(_ef)) else False
            # Apply quality filter: evalue < MAX or score >= OVERRIDE
            # (belt-and-suspenders in case foldseek_best_hit.csv was built without filter)
            fs_quality_ok = (
                (fs_evalue is None or fs_evalue < FOLDSEEK_EVALUE_MAX)
                or (fs_score is not None and fs_score >= FOLDSEEK_SCORE_OVERRIDE)
            )
            fs_inf = bool(fs_inf_flag) and is_informative(fs_desc) and fs_quality_ok
            # WEAK/NO_HIT structural hits are unreliable: force uninformative so
            # the downstream agreement is not driven by poor-quality FS evidence.
            if fs_confidence in ("WEAK", "NO_HIT"):
                fs_inf = False
        else:
            fs_desc = fs_accession = fs_taxname = fs_db = "NA"
            fs_evalue = fs_score = fs_pident = fs_qcov = None
            fs_inf = False
            fs_same_host   = False
            fs_defense     = False
            fs_confidence  = "NO_HIT"
            fs_promiscuous = False
            fs_euka_desc   = False
            fs_euka_kingdom = False

        # ---- Eukaryotic kingdom filter (Photosystem-I style false positives) ---
        # Compute best-hit kingdom and the list of kingdoms across the top hits.
        # If the best hit is Eukaryote AND nothing in the top hits is
        # Bacteria/Archaea/Virus, demote foldseek_confidence one step.
        best_hit_kingdom = _kingdom_from_taxname(fs_taxname) if has_fs else "Unknown"
        top_taxnames = []
        if not fs_top3.empty and locus_tag in set(fs_top3["gene"]):
            for _, r3 in fs_top3[fs_top3["gene"] == locus_tag].iterrows():
                tn = clean_str(r3.get("taxname", "NA"))
                if tn and tn != "NA":
                    top_taxnames.append(tn)
        top_kingdoms = [_kingdom_from_taxname(tn) for tn in top_taxnames]
        top3_kingdoms_str = " | ".join(top_kingdoms) if top_kingdoms else "NA"

        if has_fs:
            new_conf, demoted, demote_reason = _apply_eukaryotic_demote(
                fs_confidence, best_hit_kingdom, top_kingdoms,
            )
            # `demoted` is True exactly when best_hit_kingdom=="Eukaryote" AND no
            # Bacteria/Archaea/Virus support anything in the top-3 -- i.e. the
            # taxonomy-grounded eukaryote-only signal, independent of whether the
            # description text itself contains an organism/gene-family keyword
            # (that's the separate, weaker foldseek_euka_desc fallback below).
            # Surfaced as its own flag so downstream curation gates that key on
            # "is this a eukaryotic fold-level false positive" (04_curate_
            # annotations.py Case 4) see this even when the description is a
            # generic-sounding gene symbol with no eukaryote keyword (e.g. a
            # human "Survival of motor neuron-related-splicing factor 30" hit
            # that got demoted GOOD -> BORDERLINE here but previously slipped
            # through 04's promiscuous/euka-desc-only gate untouched).
            fs_euka_kingdom = demoted
            if demoted:
                fs_confidence = new_conf
                # Re-apply WEAK/NO_HIT exclusion after demotion
                if fs_confidence in ("WEAK", "NO_HIT"):
                    fs_inf = False
        else:
            demoted = False
            demote_reason = ""

        # Get top3 FS descriptions for the note column
        top3_descs = []
        if not fs_top3.empty and locus_tag in set(fs_top3["gene"]):
            for _, r3 in fs_top3[fs_top3["gene"] == locus_tag].iterrows():
                d = clean_str(r3.get("description", "NA"))
                if is_informative(d):
                    top3_descs.append(d)
        top3_str = " | ".join(top3_descs[:3]) if top3_descs else "NA"

        # Agreement classification
        agreement, fz_score = classify_agreement(
            p_product, fs_desc, p_inf, fs_inf, p_conf,
            p_function_cat=p_function,
            top3_descs=top3_descs,
        )

        rows.append({
            "prophage":             prophage,
            "locus_tag":            locus_tag,
            "aa_length":            meta_aa_len,
            "pharokka_function":    meta_function,   # original Pharokka /function values

            # PHold columns
            "phold_product":        p_product,
            "phold_function_cat":   p_function,      # PHold /function (category)
            "phold_confidence":     p_conf,
            "phold_evalue":         p_evalue,
            "phold_bitscore":       p_bitscore,
            "phold_fident":         p_fident,
            "phold_qcov":           p_qcov,
            "phold_phrog":          p_phrog,
            "phold_method":         p_method,
            "phold_prostt5":        p_prostt5,
            "phold_tophit":         p_tophit,
            "phold_inf":            p_inf,

            # Sub-DB tophit columns (ACR/VFDB/CARD/NetFlaX/DefenseFinder) -- the
            # clean structured identity behind phold's generic placeholders, and/or
            # evidence for genes where the sub-DB hit did not win phold's top1 race.
            "subdb_source":         subdb_source,
            "subdb_name":           subdb_name,

            # FoldSeek columns
            "foldseek_description":    fs_desc,
            "foldseek_accession":      fs_accession,
            "foldseek_db":             fs_db,           # source DB: afdb-swissprot / pdb100 / afdb50
            "foldseek_evalue":         fs_evalue,
            "foldseek_score":          fs_score,
            "foldseek_pident":         fs_pident,
            "foldseek_qcov":           fs_qcov,
            "foldseek_taxname":        fs_taxname,
            "foldseek_top3":           top3_str,
            "foldseek_inf":            fs_inf,
            "foldseek_same_host":      fs_same_host,    # hit from HOST_GENUS (potential AMG/moron)
            "foldseek_defense":        fs_defense,      # hit suggests defense system protein
            "foldseek_confidence":     fs_confidence,   # CONFIDENT/GOOD/BORDERLINE/WEAK/NO_HIT
            "foldseek_promiscuous":    fs_promiscuous,  # known promiscuous fold → flag for review
            "foldseek_euka_desc":      fs_euka_desc,    # eukaryotic protein inferred from description
            "foldseek_euka_kingdom":   fs_euka_kingdom, # eukaryotic protein inferred from taxonomy (best_hit_kingdom, no prokaryotic/viral top-3 support)
            "best_hit_kingdom":        best_hit_kingdom,
            "foldseek_top3_kingdoms":  top3_kingdoms_str,
            "eukaryotic_demote":       demote_reason,   # empty unless filter fired

            # Agreement
            "fuzzy_score":          fz_score,
            "agreement":            agreement,
        })

    comp_df = pd.DataFrame(rows).sort_values(["prophage", "locus_tag"])
    log(f"Comparison table: {len(comp_df)} genes")

    # -- Agreement statistics --------------------------------------------------
    section("AGREEMENT STATISTICS")
    n_total_hypo = len(comp_df)

    log(f"Overall agreement breakdown ({n_total_hypo} hypothetical genes):")
    log(f"  Thresholds: strong >= {FUZZY_STRONG_THRESHOLD} Jaccard | partial >= {FUZZY_PARTIAL_THRESHOLD} Jaccard")
    log(f"  complementary = PHold category matches FoldSeek keyword (see COMPLEMENTARY_CATEGORY_MAP)")
    log("")
    vc = comp_df["agreement"].value_counts()
    ordered_labels = [
        "strong", "partial", "complementary",
        "phold_only", "foldseek_only",
        "different", "both_uninformative",
    ]
    for label in ordered_labels:
        if label in vc.index:
            count = vc[label]
            pct = 100.0 * count / n_total_hypo
            log(f"  {label:22s}: {count:4d}  ({pct:.1f}%)")
    # Print any unexpected labels
    for label in vc.index:
        if label not in ordered_labels:
            count = vc[label]
            pct = 100.0 * count / n_total_hypo
            log(f"  {label:22s}: {count:4d}  ({pct:.1f}%)")

    both_inf = comp_df[comp_df["phold_inf"] & comp_df["foldseek_inf"]]
    log(f"\nBoth informative: {len(both_inf)} genes")
    if len(both_inf) > 0:
        log("  Agreement within 'both informative' subset:")
        vc2 = both_inf["agreement"].value_counts()
        for label in vc2.index:
            count = vc2[label]
            pct = 100.0 * count / len(both_inf)
            log(f"    {label:22s}: {count:4d}  ({pct:.1f}%)")
        log(f"  Mean Jaccard score (both inf): {both_inf['fuzzy_score'].mean():.3f}")

    log(f"\nPHold informative rate:    {comp_df['phold_inf'].mean():.0%}")
    log(f"FoldSeek informative rate: {comp_df['foldseek_inf'].mean():.0%}")

    # Same-host hits (potential AMG/moron -- biologically significant)
    same_host_df = comp_df[comp_df["foldseek_same_host"] == True]
    if not same_host_df.empty:
        log(f"\nSame-host ({HOST_GENUS}) hits -- potential AMG/moron candidates: "
            f"{len(same_host_df)}")
        for _, r in same_host_df.iterrows():
            log(f"  {r['locus_tag']} ({r['prophage']}): "
                f"FS='{r['foldseek_description'][:55]}' "
                f"(agreement={r['agreement']})")

    # Defense-system hits
    defense_df = comp_df[comp_df["foldseek_defense"] == True]
    if not defense_df.empty:
        log(f"\nDefense-system hits (cross-check with DefenseFinder/PADLOC): "
            f"{len(defense_df)}")
        for _, r in defense_df.iterrows():
            log(f"  {r['locus_tag']}: {r['foldseek_description'][:60]}")

    log("\nPer-prophage summary:")
    pp = comp_df.groupby("prophage").agg(
        n_genes=("locus_tag", "count"),
        phold_inf=("phold_inf", "sum"),
        fs_inf=("foldseek_inf", "sum"),
    )
    pp["phold_%"] = (100 * pp["phold_inf"] / pp["n_genes"]).round(0).astype(int)
    pp["fs_%"]    = (100 * pp["fs_inf"] / pp["n_genes"]).round(0).astype(int)
    log(pp.to_string())

    # -- Save ------------------------------------------------------------------
    comp_out = COMPARISON_DIR / "comparison_per_gene.csv"
    comp_df.to_csv(str(comp_out), index=False)
    log(f"\nComparison table -> {comp_out}")
    log(f"  {len(comp_df)} rows, {len(comp_df.columns)} columns")

    log("\n-> Next step: python scripts/04_curate_annotations.py")

if __name__ == "__main__":
    main()
