#!/usr/bin/env bash
# run_curation.sh — run the phageFACTor curation layer on FoldFirstAskLater output.
#
# One phage at a time; the curated result is written back INTO each phage's own
# FFAL output folder, as <phage>/phagefactor_curation/.
#
#   bash phagefactor_curation/run_curation.sh <ffal_output_dir>
#
# <ffal_output_dir> holds one subdirectory per phage, each containing
#   <prefix>_phold_per_cds_predictions.tsv   (FFAL's per-CDS table)
#   *_database_hits.tsv                      (one per searched database)
#   <prefix>_phold.gbk                       (used for the optional synteny step)
#
# Options (environment variables):
#   HOST_GENUS=Pseudomonas   host genus -> enables the host-similarity boost
#   HOST_MAP=hosts.tsv       per-phage override, two columns: <phage> <genus>
#   ONLY="PEV2 phiKZ"        only these phages
#   SKIP="test1"             skip these
#   VERBOSE=1                per-phage detail instead of one summary line
#
# Requires: python >= 3.9 with pandas, biopython, pyyaml, openpyxl.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FFAL_OUT="${1:-}"
[[ -n "$FFAL_OUT" && -d "$FFAL_OUT" ]] || {
    echo "usage: bash $0 <ffal_output_dir>" >&2; exit 1; }
FFAL_OUT="$(cd "$FFAL_OUT" && pwd)"

command -v python >/dev/null 2>&1 && PY=python || PY=python3
"$PY" - <<'PY' || { echo "ERROR: missing python dependencies (pandas, biopython, pyyaml)." >&2; exit 1; }
import pandas, yaml
from Bio import SeqIO
PY

HOST_MAP="${HOST_MAP:-}"
DEFAULT_HOST="${HOST_GENUS:-${PHAGEFACTOR_HOST_GENUS:-}}"
ONLY="${ONLY:-}"; SKIP="${SKIP:-}"

echo "=== FFAL -> phageFACTor curation ==="
echo "  ffal output : $FFAL_OUT"
echo "  curation    : $HERE"
echo "  host genus  : ${DEFAULT_HOST:-<none — host boost disabled>}"
echo ""

host_for() {
    local p="$1"
    [[ -f "$HOST_MAP" ]] || { echo ""; return; }
    awk -v p="$p" '$1==p {print $2; f=1} END{if(!f) print ""}' "$HOST_MAP" | head -1
}

ok=0; skipped=0
for d in "$FFAL_OUT"/*/; do
    name="$(basename "$d")"
    printf '[%s] ' "$name"
    [[ -z "$ONLY" || " $ONLY " == *" $name "* ]] || { echo "not in ONLY"; continue; }
    if [[ -n "$SKIP" && " $SKIP " == *" $name "* ]]; then
        echo "in SKIP"; skipped=$((skipped+1)); continue; fi

    pcds=$(ls "$d"/*_phold_per_cds_predictions.tsv 2>/dev/null | head -1 || true)
    [[ -n "$pcds" ]] || { echo "no per-CDS table — unfinished FFAL run"; skipped=$((skipped+1)); continue; }
    hits1=$(ls "$d"/*_database_hits.tsv 2>/dev/null | head -1 || true)
    [[ -n "$hits1" ]] || { echo "no *_database_hits.tsv"; skipped=$((skipped+1)); continue; }
    # Taxonomy drives the eukaryote demote and the host boost. Without it the
    # curation still runs, but two of its filters are silently inert -- say so.
    case "$(head -1 "$hits1")" in
        *taxname*) : ;;
        *) echo "hit table has no taxname column: eukaryote filter and host boost"
           echo "      will not fire. Re-run FFAL with the taxonomy patch to enable them." ;;
    esac

    prefix="$(basename "$pcds" _phold_per_cds_predictions.tsv)"
    host="$(host_for "$name")"; [[ -n "$host" ]] || host="$DEFAULT_HOST"
    run="$d/phagefactor_curation/_work"
    rm -rf "$run"; mkdir -p "$run"
    export PHAGEFACTOR_HOST_GENUS="$host"

    [[ "${VERBOSE:-0}" == "1" ]] && echo "    host=${host:-<none>} prefix=$prefix"

    # 1. FFAL hit tables -> the two CSVs step 03 reads
    "$PY" "$HERE/adapters/ffal_to_phagefactor.py" \
        --ffal-out "$d" --prefix "$prefix" --prophage "$name" \
        --pf-out "$run" --phagefactor "$HERE" \
        > "$run/01_bridge.log" 2>&1 || {
        echo "FAILED at the bridge step — see $run/01_bridge.log" >&2
        echo "      NOTE: any *.csv already in $d/phagefactor_curation/ are from an"  >&2
        echo "      EARLIER run and are now stale." >&2
        continue; }

    # 2. compare -> curate -> build output -> optional synteny
    (
        cd "$HERE"
        export PHAGEFACTOR_RUN_DIR="$run"
        "$PY" scripts/03_compare_annotations.py > "$run/03_compare.log" 2>&1 || exit 1
        "$PY" scripts/04_curate_annotations.py  > "$run/04_curate.log"  2>&1 || exit 1
        "$PY" scripts/05_build_output.py        > "$run/05_output.log"  2>&1 || true

        # Step 05 cannot build a GenBank without a Pharokka source GBK, but FFAL
        # already wrote one with full CDS coordinates. Synteny only needs gene
        # order and strand, so that file is enough.
        fin="$run/04_output/final_annotations_table.csv"
        gbk=$(ls "$run/04_output"/*.gb "$run/04_output"/*.gbk 2>/dev/null | head -1)
        [[ -n "$gbk" ]] || gbk=$(ls "$d"/*_phold.gbk 2>/dev/null | head -1)
        if [[ -f "$fin" && -n "$gbk" ]]; then
            "$PY" scripts/07_integrate.py --final "$fin" --gbk "$gbk" \
                --out "$run/04_output/final_annotations_with_synteny.csv" \
                > "$run/07_synteny.log" 2>&1 || true
        fi
    ) || { echo "FAILED — see $run/*.log" >&2; continue; }

    # 3. deliverables next to the FFAL output
    out="$d/phagefactor_curation"
    for f in "$run/03_comparison/comparison_per_gene.csv" \
             "$run/04_output/curation/curated_annotations.csv" \
             "$run/04_output/curation/review_suggested.csv" \
             "$run/04_output/final_annotations_table.csv" \
             "$run/04_output/final_annotations_table.xlsx" \
             "$run/04_output/final_annotations_with_synteny.csv"; do
        [[ -f "$f" ]] && cp -f "$f" "$out/"
    done
    cp -f "$run"/0*.log "$out/" 2>/dev/null || true

    "$PY" - "$out" <<'PY'
import os, re, sys
import pandas as pd
f = os.path.join(sys.argv[1], "curated_annotations.csv")
if not os.path.exists(f):
    print("  -> no curated table written"); raise SystemExit
df = pd.read_csv(f)
hyp = df["was_hypothetical"] == True if "was_hypothetical" in df else df.index == df.index
# A "structural-uncharacterised" call means a confident fold but STILL no function
# name. Counting it as resolved would inflate the rate, so it is reported apart.
UNINF = re.compile(r"hypothetical|uncharacteri[sz]ed|unknown function|^nan$|^$", re.I)
n = int(hyp.sum())
r = int(df.loc[hyp, "final_product"].apply(lambda p: not UNINF.search(str(p).strip())).sum()) if n else 0
su = int((df.loc[hyp, "annotation_source"] == "structural-uncharacterised").sum()) if "annotation_source" in df else 0
rev = int(df["needs_review"].sum()) if "needs_review" in df else 0
print(f"  -> {r}/{n} hypotheticals NAMED ({100*r/max(n,1):.0f}%)"
      f" | +{su} structural-uncharacterised | {rev} flagged for review")
PY
    ok=$((ok+1))
done

echo ""
echo "=== done: $ok curated, $skipped skipped ==="
echo "results: <phage>/phagefactor_curation/final_annotations_table.csv"
echo "review : <phage>/phagefactor_curation/review_suggested.csv"
