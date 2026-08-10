#!/usr/bin/env python3
"""
synteny_hint.py — positional/strand heuristics phold & phynteny can't give
=========================================================================
Phold and Phynteny both stop at the PHROG *category* "transcription regulation"
— they cannot tell the CI/C1 immunity repressor from the Cro lytic anti-repressor.
That distinction is positional (the classic lambda immunity switch: cI and cro are
divergently transcribed; cI faces the lysogeny/integrase side, cro faces the
early-lytic/structural side) so it needs gene coordinates + strand, which phold
never uses. This module adds a `synteny_hint` column from start/end/strand.

What it emits (per prophage, genes sorted by coordinate):
  - C1/CI vs Cro for a divergent regulator pair (opposite strands, adjacent),
    oriented by which side each gene's transcription points to (integrase/lysogeny
    vs structural/lytic) and corroborated by size (CI larger, Cro small ~60-90 aa).
  - module tags: integration (att boundary), lysis cassette (holin+endolysin
    adjacency), late/structural operon, replication/early.

Heuristic, not ground truth — meant as a curation aid. Run AFTER step 05 on the
final table joined with per-gene coordinates (gene_metadata_rich.csv).

CLI:
  python synteny_hint.py --final 05_output/final_annotations_table.csv \
      --coords input/gene_metadata_rich.csv --out 05_output/final_with_synteny.csv
"""

import re
import argparse
from pathlib import Path

try:
    import pandas as pd
except ImportError:
    raise SystemExit("pandas required: pip install pandas")

_RE_REG = re.compile(r'transcription|repressor|regulator|\bcro\b|\bc1\b|\bci\b|'
                     r'xre|helix.turn.helix|\bhth\b|antirepress|\bcii\b|arc-like', re.I)
_RE_INT = re.compile(r'integrase|recombinase|excisionase|transposase', re.I)
_RE_STR = re.compile(r'terminase|capsid|portal|tail|baseplate|head|tape measure|'
                     r'holin|endolysin|spanin|connector|neck|sheath|tube|'
                     r'major capsid|decoration', re.I)
_RE_REP = re.compile(r'primase|helicase|replication|polymerase|exonuclease|'
                     r'\bdna\b.*metabol', re.I)
# regulators that are NOT phage immunity switches (host metabolic regulators)
_RE_REG_EXCLUDE = re.compile(r'zinc|\bzur\b|\bfur\b|ferric|arac|gntr|tetr|lysr|'
                             r'marr|metal|uptake', re.I)


def _module(product: str, function: str) -> str:
    p = str(product); f = str(function).lower()
    if _RE_INT.search(p):
        return "INT"
    if _RE_REG.search(p) or "transcription regulation" in f:
        return "REG"
    if _RE_STR.search(p):
        return "STR"
    if _RE_REP.search(p) or "nucleotide metabolism" in f:
        return "REP"
    return "OTHER"


def _is_immunity_regulator(product: str) -> bool:
    """A phage immunity-switch candidate (not a host metabolic regulator)."""
    return _RE_REG.search(str(product)) is not None and \
        _RE_REG_EXCLUDE.search(str(product)) is None


def annotate_synteny(df: pd.DataFrame) -> pd.Series:
    """df needs: prophage, start, end, strand, aa_length, final_product,
    final_function. Returns a `synteny_hint` Series aligned to df.index."""
    hint = pd.Series("", index=df.index, dtype=object)

    for _pp, g in df.groupby("prophage"):
        g = g.sort_values("start")
        mods = {i: _module(r.final_product, r.get("final_function", ""))
                for i, r in g.iterrows()}
        int_pos = [(g.at[i, "start"] + g.at[i, "end"]) / 2
                   for i in g.index if mods[i] == "INT"]
        str_pos = [(g.at[i, "start"] + g.at[i, "end"]) / 2
                   for i in g.index if mods[i] == "STR"]
        lyso_anchor = (sum(int_pos) / len(int_pos)) if int_pos else None
        lytic_anchor = (sum(str_pos) / len(str_pos)) if str_pos else None

        # module-level tags
        for i in g.index:
            if mods[i] == "INT":
                hint[i] = "integration module (att boundary / lysogeny)"
            elif mods[i] == "STR":
                hint[i] = "late / structural operon"

        # lysis cassette: holin adjacent to endolysin
        idx = list(g.index)
        for a, b in zip(idx, idx[1:]):
            pa, pb = str(g.at[a, "final_product"]).lower(), str(g.at[b, "final_product"]).lower()
            if ("holin" in pa and "lysin" in pb) or ("lysin" in pa and "holin" in pb):
                hint[a] = "lysis cassette (holin–endolysin)"
                hint[b] = "lysis cassette (holin–endolysin)"

        # ---- C1/Cro divergent switch ----------------------------------------
        reg = [i for i in g.index if _is_immunity_regulator(g.at[i, "final_product"])]

        def _faces(i):
            # which side does the gene transcribe toward? + -> higher coords, - -> lower
            mid = (g.at[i, "start"] + g.at[i, "end"]) / 2
            direction = 1 if str(g.at[i, "strand"]).strip() in ("+", "1") else -1
            target = mid + direction * 1e6
            d_lyso = abs(target - lyso_anchor) if lyso_anchor else 9e18
            d_lytic = abs(target - lytic_anchor) if lytic_anchor else 9e18
            return "lysogeny" if d_lyso < d_lytic else "lytic"

        # Collect ALL adjacent opposite-strand candidate pairs, then keep the SINGLE
        # most size-coherent one per prophage (handles prophages with >1 regulator
        # pair). A confident "[likely]" call requires the CI to face the lysogeny
        # side, be >=20% larger than Cro, AND be >100 aa.
        candidates = []
        for a, b in zip(reg, reg[1:]):
            if str(g.at[a, "strand"]) == str(g.at[b, "strand"]):
                continue
            if g.at[b, "start"] - g.at[a, "end"] > 1500:   # not a divergent promoter pair
                continue
            fa, fb = _faces(a), _faces(b)
            la, lb = g.at[a, "aa_length"] or 0, g.at[b, "aa_length"] or 0
            if fa == "lysogeny" and fb == "lytic":
                ci, cro = a, b
            elif fb == "lysogeny" and fa == "lytic":
                ci, cro = b, a
            else:                                          # orientation tie -> size
                ci, cro = (a, b) if la >= lb else (b, a)
            lci, lcro = g.at[ci, "aa_length"] or 0, g.at[cro, "aa_length"] or 0
            # Confident pair: CI faces lysogeny, CI >=20% larger AND >100aa, and Cro
            # is small (<100aa, the usual Cro size). A larger Cro -> tentative, not a
            # hard exclusion (Cro can occasionally exceed 100).
            coherent = (fa != fb) and lci >= 1.2 * lcro and lci > 100 and lcro < 100
            ratio = lci / max(lcro, 1)
            candidates.append(((1 if coherent else 0, ratio, lci), ci, cro, coherent))
        if candidates:
            candidates.sort(key=lambda t: t[0], reverse=True)   # best coherence first
            _, ci, cro, coherent = candidates[0]
            conf = "likely" if coherent else "tentative"
            hint[ci] = (f"C1/CI immunity repressor [{conf}] — divergent pair, faces "
                        f"lysogeny/integrase side, {g.at[ci,'aa_length']} aa")
            hint[cro] = (f"Cro lytic anti-repressor [{conf}] — divergent pair, faces "
                         f"structural/lytic side, {g.at[cro,'aa_length']} aa")
    return hint


def coords_from_gbk(gbk_path: str) -> pd.DataFrame:
    """Extract per-CDS coordinates from a GenBank file (works for genome mode,
    where pharokka/phold produced the GBK, AND protein mode's built GBK). Returns
    columns: locus_tag, start, end, strand, aa_length. The record id is ignored
    here because `prophage` is carried by the final table; we join on locus_tag."""
    from Bio import SeqIO
    rows = []
    for rec in SeqIO.parse(gbk_path, "genbank"):
        for feat in rec.features:
            if feat.type != "CDS":
                continue
            # Qualifier name varies by producer:
            #   pharokka / phold  -> ID
            #   NCBI RefSeq GBK   -> protein_id
            #   bakta / generic   -> locus_tag
            # FFAL on a nucleotide FASTA calls genes with pyrodigal-gv and writes
            # `ID`, so without it this returned an EMPTY frame and the downstream
            # merge died with KeyError: 'locus_tag'.
            lt = (feat.qualifiers.get("locus_tag") or
                  feat.qualifiers.get("protein_id") or
                  feat.qualifiers.get("ID") or [None])[0]
            if lt is None:
                continue
            tr = feat.qualifiers.get("translation", [""])[0]
            rows.append({
                "locus_tag": lt,
                "start": int(feat.location.start) + 1,   # 1-based
                "end": int(feat.location.end),
                "strand": "+" if feat.location.strand == 1 else "-",
                "aa_length": len(tr) if tr else None,
            })
    if not rows:
        # Return an EMPTY but correctly-shaped frame: an all-empty DataFrame has no
        # columns, so a downstream merge on locus_tag raises KeyError instead of
        # simply producing no hints.
        return pd.DataFrame(columns=["locus_tag", "start", "end", "strand", "aa_length"])
    return pd.DataFrame(rows)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--final", required=True, help="final_annotations_table.csv")
    ap.add_argument("--coords",
                    help="protein mode: gene_metadata_rich.csv (gene_id_fa,start,end,strand)")
    ap.add_argument("--gbk",
                    help="genome mode (or any mode): GenBank file to read CDS coords from "
                         "(e.g. 05_output/updated_prophages.gb). Use instead of --coords.")
    ap.add_argument("--out", required=True)
    a = ap.parse_args()
    if not (a.coords or a.gbk):
        ap.error("provide --coords (protein mode CSV) or --gbk (genome/any mode)")
    f = pd.read_csv(a.final)
    if a.gbk:
        r = coords_from_gbk(a.gbk)
        m = f.merge(r, on="locus_tag", how="left")
    else:
        r = pd.read_csv(a.coords)[["gene_id_fa", "start", "end", "strand"]]
        m = f.merge(r, left_on="locus_tag", right_on="gene_id_fa", how="left")
    # aa_length may already exist in the final table; prefer the table's value
    if "aa_length_x" in m.columns:
        m["aa_length"] = m["aa_length_x"].fillna(m.get("aa_length_y"))
    m["synteny_hint"] = annotate_synteny(m)
    keep = [c for c in f.columns] + ["synteny_hint"]
    m[keep].to_csv(a.out, index=False)
    n = (m["synteny_hint"] != "").sum()
    print(f"Wrote {a.out}: {n}/{len(m)} genes annotated with a synteny hint")
    for _, x in m[m.synteny_hint.str.contains("C1/CI|Cro", na=False)].iterrows():
        print(f"  {x.prophage} {x.locus_tag[-6:]} {x.strand} aa={x.aa_length}: "
              f"{x.final_product[:32]!r} -> {x.synteny_hint}")


if __name__ == "__main__":
    main()
