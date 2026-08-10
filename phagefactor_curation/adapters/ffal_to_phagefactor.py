#!/usr/bin/env python3
"""
ffal_to_phagefactor.py — bridge Fold First Ask Later (FFAL) output into the
phageFACTor compare + curate layer.
================================================================================
FFAL (github.com/hannelorelongin/FoldFirstAskLater) runs Phold + FoldSeek vs the
PDB and AlphaFold/UniProt50-minimal DBs and writes, per input:

  <prefix>_phold_per_cds_predictions.tsv   Phold per-CDS call
  pdb_database_hits.tsv                     ALL FoldSeek hits vs PDB     (+ pdb_name / uniprot_names / function)
  af50m_database_hits.tsv                   ALL FoldSeek hits vs AFDB    (+ uniprot_name / function)

...but it explicitly does "no processing of hits yet": no single final call per
gene, no reconciliation of the structural hits against the Phold call. That is
exactly phageFACTor 03_compare + 04_curate.

This adapter maps FFAL output into the four files phageFACTor step 03 expects:

  <out>/02_foldseek/foldseek_best_hit.csv   (via foldseek_scoring.build_best_and_top3)
  <out>/02_foldseek/foldseek_top3.csv
  <out>/split/gene_metadata.csv             (which genes are hypothetical)
  <out>/01_phold/combined/phold_all.tsv     (Phold per-CDS, concatenated)

It reuses phageFACTor's SAME scoring path (foldseek_scoring.build_best_and_top3),
so confidence tiers / description upgrade / phage-rescue / promiscuous cap / top-3
agreement are identical to native phageFACTor and to the 02w webAPI bridge.

Design mirror: this is a sibling of phagefactor/scripts/02w_foldseek_webapi.py
:: webm8_to_best_hit(). If you change one, glance at the other.

TAXONOMY NOTE
-------------
FFAL's FoldSeek result TSV currently carries NO taxname/taxid column (see
foldfirst.results.foldfirst_parsing.get_foldfirst_hits). phageFACTor uses taxname
for the same-host flag and the eukaryotic-kingdom demote. This adapter AUTODETECTS
a taxname column: if FFAL is patched to emit one (recommended upstream PR — the
pdb/af50m DBs ship *_taxonomy files), host + euka-by-taxonomy light up automatically.
Until then they degrade gracefully (same_host -> NA/False, euka-by-taxonomy off;
description-keyword euka still works).

Usage
-----
  python ffal_to_phagefactor.py \
      --ffal-out out_ffal/ALG-004 \
      --prefix ALG-004 --prophage ALG-004 \
      --pf-out runs/filipa_ffal \
      --phagefactor /path/to/phagefactor        # for imports (or set PHAGEFACTOR_ROOT)

Repeat per prophage into the SAME --pf-out to accumulate all four, then run:
  cd /path/to/phagefactor
  PHAGEFACTOR_RUN_DIR=<abs runs/filipa_ffal> python scripts/03_compare_annotations.py
  PHAGEFACTOR_RUN_DIR=<abs runs/filipa_ffal> python scripts/04_curate_annotations.py

(Exact env wiring for 03/04 depends on your config.py resolution; see the plan
§3.3 and the phageFACTor RUN_INSTRUCTIONS. The adapter only WRITES the inputs.)
"""

import argparse
import os
import sys
from pathlib import Path

try:
    import pandas as pd
except ImportError:
    raise SystemExit("pip install pandas")


# --------------------------------------------------------------------------- #
# phageFACTor imports (scoring core)
# --------------------------------------------------------------------------- #
def _wire_phagefactor(pf_root: str):
    """Put phageFACTor scripts/ + repo root on sys.path so we can import the
    SAME scoring functions the native pipeline uses."""
    root = Path(pf_root or os.environ.get("PHAGEFACTOR_ROOT", "")).expanduser()
    if not root or not root.exists():
        raise SystemExit(
            "Cannot find phageFACTor. Pass --phagefactor /path/to/phagefactor "
            "or export PHAGEFACTOR_ROOT."
        )
    # scripts/lib FIRST. The shared modules moved there; if an old copy is still
    # sitting in scripts/ (e.g. an rsync without --delete), putting scripts/
    # first would silently import the STALE foldseek_scoring/lexicon and the
    # curation would run against superseded rules while looking fine.
    lib = root / "scripts" / "lib"
    sys.path.insert(0, str(root))
    sys.path.insert(0, str(root / "config"))
    sys.path.insert(0, str(root / "scripts"))
    if lib.is_dir():
        sys.path.insert(0, str(lib))
        stale = [m for m in ("foldseek_scoring", "lexicon", "utils", "config")
                 if (root / "scripts" / f"{m}.py").exists()]
        if stale:
            log(f"  [WARN] stale duplicates in {root}/scripts: {stale} — "
                f"scripts/lib takes precedence, but DELETE them to avoid confusion.")
    return root


_VERBOSE = os.environ.get("VERBOSE", "0") == "1"


def vlog(msg):
    """Per-phage detail. Silent unless VERBOSE=1 -- 17 phages x 6 sanity lines
    buries the one number that matters (how many got named)."""
    if _VERBOSE:
        print(msg, flush=True)


def log(msg):
    print(msg, flush=True)


# --------------------------------------------------------------------------- #
# FFAL hit-table -> phageFACTor per-hit frame
# --------------------------------------------------------------------------- #
def _pick_description(row) -> str:
    """Prefer the API-enriched protein name, fall back to the mapped `function`.
    FFAL columns vary: pdb hits carry `pdb_name` (+ optional `uniprot_names`),
    af50m hits carry `uniprot_name`; both always carry `function`."""
    for col in ("pdb_name", "uniprot_name"):
        v = row.get(col)
        if isinstance(v, str) and v.strip() and not v.startswith("WARNING"):
            return v.strip()
    # uniprot_names is a list-like of "Name (UniProt ID: X)" strings
    v = row.get("uniprot_names")
    if isinstance(v, (list, tuple)) and v:
        first = str(v[0])
        if first and not first.startswith("WARNING") and first.lower() != "none":
            return first
    v = row.get("function")
    if isinstance(v, str) and v.strip():
        return v.strip()
    return ""


# FFAL db basename -> the DB label phageFACTor uses in its outputs
_DB_LABEL = {"pdb": "pdb100", "af50m": "afdb50",
             "afdb_swissprot": "afdb-swissprot", "swissprot": "afdb-swissprot"}


def _load_ffal_hits(ffal_out: Path) -> pd.DataFrame:
    """Read EVERY *_database_hits.tsv in the FFAL output dir and return one
    concatenated per-hit frame in phageFACTor schema.

    Globbing (rather than a fixed pdb/af50m list) means a newly added database —
    e.g. afdb-swissprot — is picked up automatically with no code change.

    Real observed schema (FFAL, --offline):
      query target bitscore fident evalue qStart qEnd qLen tStart tEnd tLen cds_id function
    With --uniprot, extra pdb_name / uniprot_name(s) columns appear.
    With the taxonomy patch, taxid / taxname appear.
    """
    frames = []
    for fp in sorted(ffal_out.glob("*_database_hits.tsv")):
        base = fp.name.replace("_database_hits.tsv", "")
        dbtag = _DB_LABEL.get(base, base)
        try:
            df = pd.read_csv(fp, sep="\t", low_memory=False)
        except Exception as e:
            log(f"  (skip) {fp.name}: unreadable ({e})")
            continue
        if df.empty:
            log(f"  (empty) {fp.name}")
            continue
        df["foldseek_subdb"] = dbtag
        vlog(f"  {fp.name}: {len(df)} hits -> db label '{dbtag}'")
        frames.append(df)
    if not frames:
        raise SystemExit(f"No FFAL hit tables found under {ffal_out}")
    raw = pd.concat(frames, ignore_index=True)

    # cds_id: FFAL already splits query on ':' into cds_id; fall back to query.
    gene_col = "cds_id" if "cds_id" in raw.columns else "query"

    out = pd.DataFrame()
    out["gene"] = raw[gene_col].astype(str)
    # strip any leftover "contig:" prefix defensively
    out["gene"] = out["gene"].apply(lambda g: g.split(":", 1)[1] if ":" in g else g)
    out["accession"] = raw.get("target", "").astype(str)
    out["description"] = raw.apply(_pick_description, axis=1)
    out["foldseek_subdb"] = raw["foldseek_subdb"]

    # numeric fields
    out["evalue"] = pd.to_numeric(raw.get("evalue"), errors="coerce")
    out["score"] = pd.to_numeric(raw.get("bitscore"), errors="coerce")
    fident = pd.to_numeric(raw.get("fident"), errors="coerce")
    out["pident"] = (fident * 100).where(fident.notna())
    # aligned length + query length for coverage fraction downstream
    qs = pd.to_numeric(raw.get("qStart"), errors="coerce")
    qe = pd.to_numeric(raw.get("qEnd"), errors="coerce")
    out["qcov_aa"] = (qe - qs + 1).where(qs.notna() & qe.notna())
    if "qLen" in raw.columns:
        out["aa_length"] = pd.to_numeric(raw["qLen"], errors="coerce")

    # taxname: autodetect (stock FFAL omits it; the taxonomy patch adds it)
    tax_col = next((c for c in ("taxname", "taxlineage", "taxid") if c in raw.columns), None)
    out["taxname"] = raw[tax_col].astype(str) if tax_col else ""
    if tax_col is None:
        log("  NOTE: no taxname column in FFAL output -> same_host/euka-by-taxonomy "
            "disabled. Apply ffal_patches/apply_taxonomy_patch.py to enable (plan §3.4).")

    _sanity_hits(out, tax_col)
    return out


def _sanity_hits(out, tax_col):
    """Loud sanity report on the mapped hit frame — catches a schema drift in
    FFAL's *_database_hits.tsv before it silently corrupts the curation."""
    n = len(out)
    if n == 0:
        raise SystemExit("SANITY FAIL: 0 hit rows after mapping — check FFAL output/columns.")
    ngenes = out["gene"].nunique()
    desc_ok = out["description"].astype(str).str.strip().ne("").mean()
    eval_ok = out["evalue"].notna().mean()
    score_ok = out["score"].notna().mean()
    vlog(f"  [sanity] {n} hits · {ngenes} genes · desc {desc_ok:.0%} · "
        f"evalue {eval_ok:.0%} · bitscore {score_ok:.0%} · taxname={'yes' if tax_col else 'no'}")
    warn = []
    if desc_ok < 0.5:  warn.append("majority of hits have EMPTY description (check pdb_name/uniprot_name/function cols)")
    if eval_ok < 0.5:  warn.append("majority of hits have no evalue (check the 'evalue' column name)")
    if score_ok < 0.5: warn.append("majority of hits have no bitscore (check the 'bitscore' column name)")
    for w in warn:
        vlog(f"  [sanity][WARN] {w}")


def build_best_top3(hits: pd.DataFrame, best_csv: Path, top3_csv: Path,
                    all_csv: Path = None):
    """Score the FFAL hits with phageFACTor's scorer and build best/top3 —
    identical logic to 02d local mode and 02w webAPI bridge."""
    from foldseek_scoring import (
        _is_informative_fs, _phage_boost_factor, _is_same_host_hit,
        _is_promiscuous_fold_hit, _has_eukaryotic_description, build_best_and_top3,
    )
    try:
        from config import FOLDSEEK_EVALUE_MAX, FOLDSEEK_SCORE_OVERRIDE
    except Exception:
        FOLDSEEK_EVALUE_MAX, FOLDSEEK_SCORE_OVERRIDE = 0.1, 200

    df = hits.copy()
    quality_ok = (df["evalue"].fillna(999) < FOLDSEEK_EVALUE_MAX) | \
                 (df["score"].fillna(0) >= FOLDSEEK_SCORE_OVERRIDE)
    df["informative"] = df["description"].apply(_is_informative_fs) & quality_ok
    df["phage_boost"] = df["description"].apply(_phage_boost_factor)
    df["composite_score"] = df["score"].fillna(0) * df["phage_boost"]
    df["same_host"] = df["taxname"].apply(_is_same_host_hit)
    df["defense_flag"] = False
    df["promiscuous_fold_flag"] = df["description"].apply(lambda d: _is_promiscuous_fold_hit(str(d)))
    df["eukaryotic_desc_flag"] = df["description"].apply(lambda d: _has_eukaryotic_description(str(d)))
    df = df.sort_values(["gene", "composite_score", "evalue"],
                        ascending=[True, False, True])

    n_inf = int(df["informative"].sum())
    vlog(f"  [sanity] scored {len(df)} hits · {n_inf} informative "
        f"({n_inf/max(len(df),1):.0%}) · quality gate evalue<{FOLDSEEK_EVALUE_MAX} or score>={FOLDSEEK_SCORE_OVERRIDE}")

    best, top3 = build_best_and_top3(df, sorted(df["gene"].unique()))
    best_csv.parent.mkdir(parents=True, exist_ok=True)
    best.to_csv(best_csv, index=False)
    top3.to_csv(top3_csv, index=False)
    if all_csv:
        df.to_csv(all_csv, index=False)
    # confidence distribution — the headline QC number to eyeball each run
    if "foldseek_confidence" in best.columns:
        dist = best["foldseek_confidence"].value_counts().to_dict()
        vlog(f"  [sanity] best-hit confidence distribution: {dist}")
    vlog(f"  best_hit -> {best_csv} ({len(best)} genes); top3 -> {top3_csv}")


# --------------------------------------------------------------------------- #
# FFAL phold per-CDS -> phold_all.tsv + gene_metadata.csv
# --------------------------------------------------------------------------- #
def build_phold_and_metadata(per_cds_tsv: Path, prophage: str,
                             phold_all_out: Path, gene_meta_out: Path,
                             append: bool, all_hypothetical: bool = True):
    """Concat the FFAL Phold per-CDS TSV into phold_all.tsv, and derive
    gene_metadata.csv (is_hypothetical flag) that step 03 needs."""
    pcds = pd.read_csv(per_cds_tsv, sep="\t", low_memory=False)

    # locus_tag from cds_id
    lt_col = "cds_id" if "cds_id" in pcds.columns else pcds.columns[0]
    pcds = pcds.rename(columns={lt_col: "cds_id"})
    pcds["cds_id"] = pcds["cds_id"].astype(str).apply(
        lambda x: x.split(":", 1)[1] if ":" in x else x
    )

    # --- phold_all.tsv (append across prophages) ---
    phold_all_out.parent.mkdir(parents=True, exist_ok=True)
    header = not (append and phold_all_out.exists())
    pcds.to_csv(phold_all_out, sep="\t", index=False, mode=("a" if not header else "w"),
                header=header)

    # --- gene_metadata.csv ---
    prod_col = "product" if "product" in pcds.columns else None
    func_col = "function" if "function" in pcds.columns else None
    len_col = next((c for c in ("length", "aa_length", "cds_length") if c in pcds.columns), None)

    def _is_hypo(p):
        return (not isinstance(p, str)) or p.strip().lower() in (
            "hypothetical protein", "", "nan", "unknown function"
        )

    meta = pd.DataFrame()
    meta["locus_tag"] = pcds["cds_id"]
    meta["prophage"] = prophage
    meta["function"] = pcds[func_col] if func_col else "unknown function"
    if len_col:
        meta["aa_length"] = pd.to_numeric(pcds[len_col], errors="coerce")
    else:
        meta["aa_length"] = 0
    # ---- WHICH GENES ENTER CURATION -------------------------------------------
    # This choice drives everything downstream, and getting it wrong silently
    # halves the result. Native phageFACTor (genome mode) targets the ORIGINAL
    # PHAROKKA hypotheticals, NOT phold's post-rescue product — see docs/modes.md:
    # "this preserves the merged-evidence rescues". Phold's call is then COMPETING
    # EVIDENCE that curation can merge with / agree with / override.
    #
    # Taking is_hypothetical from PHOLD's own product instead (the first version of
    # this adapter) excludes every gene phold named from the denominator AND from
    # the numerator, so phold can never appear in a curation decision: no `merged`,
    # no `both agree`, and phold's names never reach final_product. On 5 OMM12
    # prophages that gave 26/380 named (7%) vs 264/474 (56%) natively.
    #
    # FFAL has no independent prior annotation (FASTA input -> pyrodigal-gv calls
    # genes with no function; GenBank input -> phold overwrites product), so the
    # correct equivalent is: EVERY CDS enters curation, with phold as evidence.
    # --no-all-hypothetical restores the old phold-derived behaviour.
    # PREFERRED path: if phold recorded HOW each call was made, use it — this
    # reproduces native semantics exactly.
    #   annotation_method == 'pharokka'  -> SEQUENCE-HOMOLOGY call. Trusted, higher
    #       confidence than any structural prediction, so it is passed through and
    #       NOT re-annotated (is_hypothetical = False).
    #   anything else ('foldseek', 'none') -> STRUCTURAL/absent. Goes to curation,
    #       where phold's structural call competes with the FoldSeek hits.
    # This only works when FFAL was given a PHAROKKA GenBank; on raw FASTA or an
    # NCBI GenBank, FFAL sets every method to 'foldseek' (it discards the original
    # product), so there is no trusted layer and every CDS must be curated.
    meth_col = "annotation_method" if "annotation_method" in pcds.columns else None
    n_pharokka = int((pcds[meth_col] == "pharokka").sum()) if meth_col else 0
    if meth_col and n_pharokka > 0:
        meta["is_hypothetical"] = pcds[meth_col].ne("pharokka").values
        vlog(f"  pharokka-derived (trusted, passed through): {n_pharokka}; "
            f"{len(meta) - n_pharokka} go to curation")
    elif all_hypothetical:
        meta["is_hypothetical"] = True
    else:
        meta["is_hypothetical"] = (pcds[prod_col].apply(_is_hypo) if prod_col else True)

    gene_meta_out.parent.mkdir(parents=True, exist_ok=True)
    header = not (append and gene_meta_out.exists())
    meta.to_csv(gene_meta_out, index=False, mode=("a" if not header else "w"),
                header=header)
    n_hypo = int(meta["is_hypothetical"].sum())
    _mode = "ALL CDS enter curation (phold = evidence)" if all_hypothetical \
            else "only phold-hypothetical genes"
    vlog(f"  phold_all += {len(pcds)} rows; gene_metadata += {len(meta)} "
        f"({n_hypo} curation targets) — {_mode}")


# --------------------------------------------------------------------------- #
def main():
    ap = argparse.ArgumentParser(description="Bridge FFAL output into phageFACTor 03/04.")
    ap.add_argument("--ffal-out", required=True, help="FFAL output dir for ONE prophage")
    ap.add_argument("--prefix", required=True, help="FFAL --prefix used for that run")
    ap.add_argument("--prophage", required=True, help="prophage name for metadata/grouping")
    ap.add_argument("--pf-out", required=True, help="phageFACTor run dir to write inputs into")
    ap.add_argument("--phagefactor", default=None, help="path to phagefactor repo (or PHAGEFACTOR_ROOT)")
    ap.add_argument("--append", action="store_true",
                    help="append to phold_all/gene_metadata (use for prophages 2..N)")
    ap.add_argument("--all-hits", action="store_true", help="also dump all scored hits")
    ap.add_argument("--no-all-hypothetical", action="store_true",
                    help="derive is_hypothetical from PHOLD's product instead of "
                         "treating every CDS as a curation target. NOT recommended: "
                         "it prevents phold from ever being competing evidence "
                         "(no merged / both-agree decisions).")
    a = ap.parse_args()

    _wire_phagefactor(a.phagefactor)

    ffal_out = Path(a.ffal_out)
    pf_out = Path(a.pf_out)

    log(f"[FFAL->phageFACTor] {a.prophage}  ({ffal_out})")

    # 1) FoldSeek hit tables -> best_hit/top3
    hits = _load_ffal_hits(ffal_out)
    # CANONICAL phageFACTor paths (config.py): step 03 reads
    #   FOLDSEEK_BEST_HIT = <run>/02_foldseek/3di_tokens/best_hit.csv
    #   FOLDSEEK_TOP3     = <run>/02_foldseek/3di_tokens/top3.csv
    # best_hit/top3 are per-RUN (all genes together), so prophages 2..N append.
    fs_dir = pf_out / "02_foldseek" / "3di_tokens"
    best_csv = fs_dir / "best_hit.csv"
    top3_csv = fs_dir / "top3.csv"
    all_csv = (fs_dir / "all_hits.csv") if a.all_hits else None

    if a.append and best_csv.exists():
        # build into temp frames then append
        import tempfile
        tb = Path(tempfile.mktemp(suffix=".csv"))
        tt = Path(tempfile.mktemp(suffix=".csv"))
        build_best_top3(hits, tb, tt, all_csv)
        pd.read_csv(tb).to_csv(best_csv, mode="a", header=False, index=False)
        pd.read_csv(tt).to_csv(top3_csv, mode="a", header=False, index=False)
        tb.unlink(missing_ok=True); tt.unlink(missing_ok=True)
        log(f"  appended best/top3 rows for {a.prophage}")
    else:
        build_best_top3(hits, best_csv, top3_csv, all_csv)

    # 2) phold per-CDS -> phold_all.tsv + gene_metadata.csv
    per_cds = ffal_out / f"{a.prefix}_phold_per_cds_predictions.tsv"
    if not per_cds.exists():
        # fall back to any *_phold_per_cds_predictions.tsv in the dir
        cands = list(ffal_out.glob("*_phold_per_cds_predictions.tsv"))
        if not cands:
            raise SystemExit(f"No *_phold_per_cds_predictions.tsv in {ffal_out}")
        per_cds = cands[0]
    build_phold_and_metadata(
        per_cds, a.prophage,
        pf_out / "01_phold" / "combined" / "phold_all.tsv",
        pf_out / "split" / "gene_metadata.csv",
        append=a.append,
        all_hypothetical=not a.no_all_hypothetical,
    )

    # 3) sub-DB evidence (VFDB/CARD/ACR/NetFlaX/DefenseFinder). Step 03 globs
    #    PHOLD_OUT_DIR/**/sub_db_tophits/*_cds_predictions.tsv, so mirror FFAL's
    #    sub_db_tophits under <run>/01_phold/<prophage>/ where that glob looks.
    # FFAL names this dir `phold_sub_db_tophits` (phold itself uses
    # `sub_db_tophits`), so accept BOTH — otherwise the ACR/VFDB/CARD/NetFlaX/
    # DefenseFinder evidence is silently dropped and the sub-DB substitution in
    # step 03 never fires. These hits matter: they are the specific, structured
    # identities (anti-CRISPR, toxin-antitoxin, defense systems).
    src_sub = next((p for p in (ffal_out / "phold_sub_db_tophits",
                                ffal_out / "sub_db_tophits") if p.is_dir()), None)
    if src_sub is not None and src_sub.is_dir():
        dst_sub = pf_out / "01_phold" / a.prophage / "sub_db_tophits"
        dst_sub.mkdir(parents=True, exist_ok=True)
        n = 0
        for f in src_sub.glob("*_cds_predictions.tsv"):
            if f.stat().st_size == 0:      # phold writes empty files for DBs with no hit
                continue
            tgt = dst_sub / f.name
            if tgt.exists() or tgt.is_symlink():
                tgt.unlink()
            try:
                tgt.symlink_to(f.resolve())
            except OSError:
                import shutil
                shutil.copy2(f, tgt)
            n += 1
        vlog(f"  sub-DB evidence: {n} non-empty table(s) from {src_sub.name} -> {dst_sub}")
    else:
        log("  (no phold_sub_db_tophits/ or sub_db_tophits/ found — sub-DB substitution skipped)")

    log("[done] Now run phageFACTor scripts/03_compare_annotations.py then 04_curate_annotations.py "
        "against this run dir (sub-DB evidence: point PHOLD_OUT_DIR at the FFAL output dir).")


if __name__ == "__main__":
    main()
