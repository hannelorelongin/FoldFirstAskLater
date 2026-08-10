#!/usr/bin/env python3
"""
07_integrate.py — merge Phynteny categories + synteny hints into the final table
================================================================================
Runs AFTER step 06 (phynteny). Adds three things to final_annotations_table.csv:

  phynteny_function_cat  — phynteny's PHROG category (or '' if below threshold)
  phynteny_probability   — phynteny recalibrated confidence
  synteny_hint           — positional/strand hints (C1/Cro, integration, lysis…)
  phynteny_vs_final      — agree | phynteny ADDS | differ | (blank)

"Smart" rescue: when the pipeline left final_function unknown/other AND phynteny
is confident (>= threshold) AND names a real category, we ADOPT phynteny's
category into final_function (recording the original in final_function_pre_phynteny).
phold/FoldSeek calls are never overwritten — only genuine gaps are filled.

Mapping note: phynteny output IDs are POSITIONAL ("phiNP_0" = 1st CDS in record
phiNP), NOT locus_tags — so we map by CDS order within each GenBank record. This
is exact and works in genome mode and protein mode alike.

CLI:
  python 07_integrate.py --final 05_output/final_annotations_table.csv \
      --gbk 05_output/updated_prophages.gb \
      --phynteny 06_phynteny/phynteny.tsv \
      --out 05_output/final_annotations_integrated.csv [--threshold 0.8]
"""

import argparse
from pathlib import Path
import pandas as pd
from Bio import SeqIO

_SCRIPTS = Path(__file__).parent
import sys
sys.path.insert(0, str(_SCRIPTS))
# scripts/lib/ holds the shared modules (config, utils, lexicon, ...).
# Harmless if they still sit next to this file.
_LIB_DIR = Path(__file__).resolve().parent / "lib"
if _LIB_DIR.is_dir():
    sys.path.insert(0, str(_LIB_DIR))
try:
    from synteny_hint import annotate_synteny, coords_from_gbk
    _HAVE_SYNTENY = True
except Exception as e:
    print(f"WARNING: synteny_hint not importable ({e}); synteny_hint column skipped")
    _HAVE_SYNTENY = False
try:
    from rules import make_short_name
except Exception:
    def make_short_name(fp, phrog=None):
        return ""

_GENERIC_FN = {"unknown function", "other", "", "nan", "na"}


def _record_order(gbk: str) -> dict:
    """{record_id: [locus_tag in CDS order]} — for positional phynteny mapping."""
    order = {}
    for rec in SeqIO.parse(gbk, "genbank"):
        order[rec.id] = [(f.qualifiers.get("locus_tag",
                          f.qualifiers.get("protein_id", [None]))[0])
                         for f in rec.features if f.type == "CDS"]
    return order


def _phynteny_to_locus(ph: pd.DataFrame, order: dict) -> pd.DataFrame:
    def id2lt(pid):
        try:
            rec, idx = str(pid).rsplit("_", 1)
            return order.get(rec, [])[int(idx)]
        except Exception:
            return None
    ph = ph.copy()
    ph["locus_tag"] = ph["ID"].apply(id2lt)
    return ph


def _write_gbk_with_hints(gbk_in: str, m: pd.DataFrame, gbk_out: str):
    """Add synteny_hint + integrated final_function as CDS /note qualifiers."""
    by_lt = m.set_index("locus_tag")
    recs = list(SeqIO.parse(gbk_in, "genbank"))
    for rec in recs:
        for feat in rec.features:
            if feat.type != "CDS":
                continue
            lt = (feat.qualifiers.get("locus_tag") or
                  feat.qualifiers.get("protein_id") or [None])[0]
            if lt is None or lt not in by_lt.index:
                continue
            row = by_lt.loc[lt]
            syn = str(row.get("synteny_hint", "") or "")
            fn = str(row.get("final_function", "") or "")
            notes = feat.qualifiers.get("note", [])
            if fn:
                notes.append(f"function (phynteny-integrated): {fn}")
            if syn:
                notes.append(f"synteny_hint: {syn}")
            if notes:
                feat.qualifiers["note"] = notes
    SeqIO.write(recs, gbk_out, "genbank")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--final", required=True)
    ap.add_argument("--gbk", required=True)
    ap.add_argument("--phynteny", default=None,
                    help="06_phynteny/phynteny.tsv. OPTIONAL: omit it to run in "
                         "SYNTENY-ONLY mode (C1/Cro + positional hints from the GBK, "
                         "no phynteny categories). Useful when phynteny is not "
                         "installed, e.g. the FFAL integration.")
    ap.add_argument("--out", required=True)
    ap.add_argument("--threshold", type=float, default=0.8)
    ap.add_argument("--overrides", default=str(_SCRIPTS.parent / "input" / "overrides.tsv"),
                    help="optional TSV of hand-curated exact fixes (applied last)")
    a = ap.parse_args()

    f = pd.read_csv(a.final)
    order = _record_order(a.gbk)

    # ---- manual overrides (applied FIRST, before phynteny compare) ------------
    # Hand-curated exact fixes that don't generalise into _UPGRADE_RULES (which
    # stays the home for regex/top-3 pattern upgrades). TSV columns:
    #   match_type (locus_tag|final_product), match, final_product, final_function,
    #   short_name, note  -- any blank field = leave unchanged.
    if a.overrides and Path(a.overrides).exists():
        ov = pd.read_csv(a.overrides, sep="\t", dtype=str, keep_default_na=False)
        n_ov = 0
        for _, o in ov.iterrows():
            mt, mv = o.get("match_type", "").strip(), o.get("match", "").strip()
            if not mt or not mv:
                continue
            col = "locus_tag" if mt == "locus_tag" else "final_product"
            sel = f[col] == mv
            if not sel.any():
                continue
            for fld in ("final_product", "final_function", "short_name"):
                val = str(o.get(fld, "")).strip()
                if val and fld in f.columns:
                    f.loc[sel, fld] = val
            # recompute short_name if final_product changed but no explicit short given
            if str(o.get("final_product", "")).strip() and not str(o.get("short_name", "")).strip() \
                    and "short_name" in f.columns and make_short_name:
                f.loc[sel, "short_name"] = f.loc[sel, "final_product"].apply(make_short_name)
            n_ov += int(sel.sum())
        print(f"Applied overrides: {n_ov} row(s) from {a.overrides}")

    # ---- Phynteny join (positional) ----
    # SYNTENY-ONLY mode: no --phynteny (or the file is absent) -> skip the
    # phynteny join entirely but still run the synteny_hint block below, so
    # C1/Cro + positional hints are produced without needing phynteny installed.
    _have_ph = bool(a.phynteny) and Path(a.phynteny).exists()
    if not _have_ph:
        if a.phynteny:
            print(f"NOTE: {a.phynteny} not found — running in SYNTENY-ONLY mode.")
        else:
            print("NOTE: no --phynteny given — running in SYNTENY-ONLY mode "
                  "(C1/Cro + positional hints only).")
        m = f.copy()
        m["_cat"] = ""
        m["phynteny_probability"] = float("nan")
    else:
        ph = pd.read_csv(a.phynteny, sep="\t")
        catcol = "phynteny_category" if "phynteny_category" in ph.columns else "predicted_category"
        confcol = "phynteny_confidence" if "phynteny_confidence" in ph.columns else "confidence"
        ph = _phynteny_to_locus(ph, order)
        ph["_cat"] = ph[catcol].astype(str)
        ph["_conf"] = pd.to_numeric(ph[confcol], errors="coerce").fillna(0.0)
        ph.loc[ph["_conf"] < a.threshold, "_cat"] = ""   # below threshold -> no call
        m = f.merge(ph[["locus_tag", "_cat", "_conf"]].dropna(subset=["locus_tag"]),
                    on="locus_tag", how="left")
        m["_cat"] = m["_cat"].fillna("")
        m["phynteny_probability"] = m["_conf"].fillna(0.0)

    # ---- INTEGRATE phynteny into final_function (phynteny takes priority) ------
    # don't multiply function columns. When phynteny is confident
    # (>= threshold) AND names a real category, it OVERWRITES final_function (most
    # agree; 8 fill gaps; the few that differ -> phynteny wins, e.g. Zur ->
    # "transcription regulation", which is correct). Pipeline category is kept only
    # where phynteny is unknown/other or below threshold.
    n_agree = n_add = n_diff = 0
    for i, r in m.iterrows():
        pc = str(r["_cat"]).strip()
        if not pc or pc.lower() in ("unknown function", "other"):
            continue
        ff = str(r["final_function"]).strip()
        if pc.lower() == ff.lower():
            n_agree += 1
        elif ff.lower() in _GENERIC_FN:
            n_add += 1
        else:
            n_diff += 1
        m.at[i, "final_function"] = pc          # phynteny priority
    m = m.drop(columns=["_cat", "_conf"], errors="ignore")

    # ---- FIX 1: normalise final_function to a PHROG category --------------------
    # Genome mode passes pharokka-annotated genes straight through, so their
    # final_function is a PRODUCT string ("portal protein", "integrase", "DNA
    # methyltransferase") not a PHROG category. Map any non-canonical value to a
    # category from the product via the same _FUNC_CAT_RULES used in step 04.
    _CANON = {"head and packaging", "connector", "tail",
              "DNA, RNA and nucleotide metabolism", "integration and excision",
              "transcription regulation", "lysis",
              "moron, auxiliary metabolic gene and host takeover",
              "other", "unknown function"}
    # _FUNC_CAT_RULES lives in 04_curate_annotations; import defensively.
    _FUNC_CAT_RULES = []
    try:
        import importlib.util as _ilu, sys as _sys
        _spec = _ilu.spec_from_file_location("_c4mod", str(_SCRIPTS / "04_curate_annotations.py"))
        _mod = _ilu.module_from_spec(_spec); _spec.loader.exec_module(_mod)
        _FUNC_CAT_RULES = getattr(_mod, "_FUNC_CAT_RULES", [])
    except Exception as e:
        print(f"  (could not load _FUNC_CAT_RULES: {e}; function normalisation skipped)")
    n_normfn = 0
    if _FUNC_CAT_RULES:
        for i, r in m.iterrows():
            ff = str(r["final_function"]).strip()
            if ff in _CANON:
                continue
            cat = "other"
            for pat, c in _FUNC_CAT_RULES:
                if pat.search(str(r["final_product"])):
                    cat = c; break
            m.at[i, "final_function"] = cat
            n_normfn += 1

    # ---- FIX 2: recover confident structural homologs to uncharacterised targets
    # Many genes scored a CONFIDENT/GOOD FoldSeek hit but to an AFDB entry with an
    # empty/DUF header -> they were dumped as plain "hypothetical / no_hit", losing
    # the fact that a conserved fold (and sometimes a DUF id) exists. Relabel these
    # as structural-only: keep a DUF/other name if present, else note the homolog.
    n_struct = 0
    # final-table FoldSeek columns are fs_*; fall back to the old foldseek_*
    # names for backward compat with tables generated before that rename.
    _c_conf = "fs_confidence" if "fs_confidence" in m.columns else "foldseek_confidence"
    _c_desc = "fs_description" if "fs_description" in m.columns else "foldseek_description"
    if _c_conf in m.columns and _c_desc in m.columns:
        for i, r in m.iterrows():
            if str(r.get("annotation_source", "")) not in ("no_hit", "no_informative_hit"):
                continue
            if str(r.get(_c_conf, "")) not in ("CONFIDENT", "GOOD"):
                continue
            desc = str(r.get(_c_desc, "") or "").strip()
            if desc and desc.lower() not in ("nan", "na", "none"):
                m.at[i, "final_product"] = desc          # e.g. "DUF4280 domain-containing protein"
            else:
                m.at[i, "final_product"] = "uncharacterised protein (confident structural homolog)"
            # distinct from "structural-only" (= cust-FoldSeek-with-a-name / source
            # 'foldseek'): this bucket is a confident FOLD match to an UNNAMED target.
            m.at[i, "annotation_source"] = "structural-uncharacterised"
            if "short_name" in m.columns and make_short_name:
                m.at[i, "short_name"] = make_short_name(m.at[i, "final_product"])
            n_struct += 1

    # ---- synteny hint ----
    if _HAVE_SYNTENY:
        c = coords_from_gbk(a.gbk)
        mm = m.merge(c, on="locus_tag", how="left", suffixes=("", "_gbk"))
        if "aa_length" not in mm.columns and "aa_length_gbk" in mm.columns:
            mm["aa_length"] = mm["aa_length_gbk"]
        m["synteny_hint"] = annotate_synteny(mm).values
        m = m.drop(columns=[c for c in m.columns if c.endswith("_gbk")], errors="ignore")

        # C1/Cro: a CONFIDENT ([likely]) divergent-switch call REPLACES short_name
        # (more informative than a generic "transcriptional repressor"). Tentative
        # calls are left as-is.
        if "short_name" in m.columns:
            def _cc(h):
                h = str(h)
                if "[likely]" not in h:
                    return None
                if h.startswith("C1/CI"):
                    return "CI repressor"
                if h.startswith("Cro"):
                    return "Cro"
                return None
            for i, r in m.iterrows():
                cc = _cc(r.get("synteny_hint", ""))
                if cc:
                    m.at[i, "short_name"] = cc

    # phynteny_probability least-relevant -> push to the far right
    cols = [c for c in m.columns if c not in ("phynteny_probability", "synteny_hint")]
    cols += [c for c in ("synteny_hint", "phynteny_probability") if c in m.columns]
    m = m[cols]
    m.to_csv(a.out, index=False)

    # ---- write synteny_hint (+ phynteny category) into a GBK -------------------
    gbk_out = str(Path(a.out).with_name("final_with_synteny.gb"))
    _write_gbk_with_hints(a.gbk, m, gbk_out)

    print(f"Wrote {a.out}: {len(m)} genes")
    print(f"  phynteny integrated into final_function: {n_agree} agree, {n_add} filled gaps, "
          f"{n_diff} overrode pipeline (phynteny priority)")
    if _HAVE_SYNTENY:
        print(f"  synteny_hint set on {(m.synteny_hint!='').sum()} genes; "
              f"C1/Cro -> short_name where [likely]")
    print(f"  GBK with synteny/phynteny notes -> {gbk_out}")


if __name__ == "__main__":
    main()
