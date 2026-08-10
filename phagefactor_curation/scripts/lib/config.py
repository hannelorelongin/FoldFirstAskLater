#!/usr/bin/env python3
"""
config.py — phageFACTor configuration loader
=========================================================================
Reads config/config.yaml ONCE and exposes the same constant names the analysis
scripts already import (`from config import FOLDSEEK_EVALUE_MAX, ...`), so the
step scripts need ZERO changes when moving between machines.

Path resolution:
  ROOT = ${PHAGEFACTOR_ROOT}  if set, else the repo root (two levels up from here)
  databases.foldseek_db_root = ${FOLDSEEK_DB_ROOT} env wins over the yaml value.

This file holds CONFIGURATION only (paths, thresholds, DB locations). Every
word-list, regex and semantic map lives in lexicon.py — the single definition
site — and nothing is defined in both places.
"""

import os
from pathlib import Path

# Vocabulary has ONE definition site: lexicon.py. Re-exported here only so the
# existing `from config import UNINFORMATIVE_STRINGS` call sites keep working.
from lexicon import (
    UNINFORMATIVE_STRINGS,
    GENERIC_WORDS,
    COMPLEMENTARY_CATEGORY_MAP,
)

try:
    import yaml
except ImportError as e:  # pragma: no cover
    raise SystemExit("phageFACTor config needs PyYAML:  pip install pyyaml") from e

# -----------------------------------------------------------------------------
# Load YAML
# -----------------------------------------------------------------------------
# This module lives in scripts/ so that `from config import ...` and
# `from lexicon import ...` resolve the same way, with no shim and no
# importlib file-loading. The user-edited file stays where users expect it:
# config/config.yaml.
def _find_repo_root(start):
    """Nearest ancestor holding config/config.yaml. Location-independent, so
    this module works from scripts/ or scripts/lib/."""
    for d in [start, *start.parents]:
        if (d / "config" / "config.yaml").is_file():
            return d
    return start.parents[1]


_CONF_DIR = _find_repo_root(Path(__file__).resolve().parent) / "config"
_YAML = _CONF_DIR / "config.yaml"
with open(_YAML) as _fh:
    _C = yaml.safe_load(_fh)

# -----------------------------------------------------------------------------
# ROOT / INPUT / RUN
# -----------------------------------------------------------------------------
_env_root = os.environ.get("PHAGEFACTOR_ROOT")
PROJECT_ROOT = Path(_env_root).resolve() if _env_root else _CONF_DIR.parent

def _p(rel: str) -> Path:
    return PROJECT_ROOT / rel

_P = _C["paths"]

# --- INPUT: the user's data (fasta/ + prophage_list.txt). --------------------
# Priority: $PHAGEFACTOR_INPUT  >  paths.input_dir (absolute ok)  >  <root>/input
_env_input = os.environ.get("PHAGEFACTOR_INPUT")
INPUT_DIR = Path(_env_input).resolve() if _env_input else _p(_P.get("input_dir", "input"))

def _pin(rel: str) -> Path:
    """Resolve a path inside INPUT_DIR, tolerating a legacy 'input/' prefix."""
    rel = rel[6:] if rel.startswith("input/") else rel
    return INPUT_DIR / rel

# --- RUN: where ALL outputs + logs go, versioned (run, run_002, ...). --------
# Priority: $PHAGEFACTOR_RUN_DIR (set+exported by submit_all.sh)
#           >  <run_base>/.current_run pointer  >  <run_base>/<run_name>
# submit_all.sh does the versioning + creation; readers here just follow it.
_env_run_base = os.environ.get("PHAGEFACTOR_RUN_BASE")
RUN_BASE = Path(_env_run_base).resolve() if _env_run_base else _p(_P.get("run_base", "runs"))
RUN_NAME = _P.get("run_name", "run")

def _resolve_run_dir() -> Path:
    env = os.environ.get("PHAGEFACTOR_RUN_DIR")
    if env:
        return Path(env).resolve()
    pointer = RUN_BASE / ".current_run"
    if pointer.exists():
        return Path(pointer.read_text().strip()).resolve()
    return RUN_BASE / RUN_NAME

RUN_DIR = _resolve_run_dir()

def _pr(rel: str) -> Path:
    return RUN_DIR / rel

# -----------------------------------------------------------------------------
# PROJECT
# -----------------------------------------------------------------------------
# Env override (PHAGEFACTOR_HOST_GENUS) wins over the shipped config default, so a
# per-dataset host (e.g. Helicobacter) can be set from the job env file without
# editing config.yaml. Only the fs_same_host flag + the "confident-fs-relevant"
# gate use it; it never touches gene calling or structural search.
HOST_GENUS = os.environ.get("PHAGEFACTOR_HOST_GENUS") or _C["project"]["host_genus"]
_NOTE_TAG  = _C["project"]["note_tag"]

# -----------------------------------------------------------------------------
# DIRECTORIES   (input data via _pin; generated outputs via _pr / run dir)
# -----------------------------------------------------------------------------
FASTA_DIR        = INPUT_DIR / "fasta"
GBK_DIR          = INPUT_DIR / "gbk"
SPLIT_DIR        = _pr(_P.get("split_dir", "split"))   # generated (under run dir)
PHAROKKA_OUT_DIR = _pr(_P["pharokka_out"])
PHOLD_OUT_DIR    = _pr(_P["phold_out"])
FOLDSEEK_DIR     = _pr(_P["foldseek_out"])
COMPARISON_DIR   = _pr(_P["comparison_out"])
OUTPUT_DIR       = _pr(_P["output_out"])          # 04_output (deliverables)
CURATION_DIR     = _pr(_P["curation_out"])        # 04_output/curation
PHYNTENY_DIR     = _pr(_P["phynteny_out"])        # 05_phynteny
PHYNTENY_RUN_DIR = _pr(_P.get("phynteny_run", "05_phynteny/run"))
GOPHAGE_DIR      = _pr(_P["gophage_out"])
LOGS_DIR         = _pr(_P.get("logs_dir", "logs"))
SCRIPTS_DIR      = PROJECT_ROOT / "scripts"

# Combined phold outputs
PHOLD_COMB_DIR     = PHOLD_OUT_DIR / "combined"
PHOLD_COMBINED_TSV = PHOLD_COMB_DIR / "phold_all.tsv"
PHOLD_3DI_FASTA    = PHOLD_COMB_DIR / "phold_3di.fasta"
PHOLD_AA_FASTA     = PHOLD_COMB_DIR / "phold_aa.fasta"
RAW_GB             = PHOLD_COMB_DIR / "all_prophages_combined.gbk"

# Gene metadata + hypothetical target list
GENE_METADATA_CSV = SPLIT_DIR / "gene_metadata.csv"

# Protein-mode generated artifacts (kept OUT of INPUT_DIR — under the run dir)
PROTEIN_COMBINED_FAA = SPLIT_DIR / "all_proteins_combined.faa"
PROTEIN_BATCH_DIR    = SPLIT_DIR / "protein_batches"
PROTEIN_BATCH_LIST   = SPLIT_DIR / "protein_batch_list.txt"
PHAROKKA_CDS_TSV_GLOB = str(PHAROKKA_OUT_DIR / "*" / "*_cds_final_merged_output.tsv")
HYPO_TARGETS_DIR = FOLDSEEK_DIR / "targets"
HYPO_GENE_LIST   = HYPO_TARGETS_DIR / "hypothetical_genes.csv"

# Pre-CDS-computed sub-mode
_PC = _C.get("precomputed", {})
SOURCE_GENOME_FASTA    = _pin(_PC.get("source_genome_fasta", "input/genome/source_genome.fasta"))
PROPHAGE_WINDOWS_CSV   = _pin(_PC.get("prophage_windows_csv", "input/fasta/prophage_windows.csv"))
RICH_GENE_METADATA_CSV = _pin(_PC.get("rich_gene_metadata_csv", "input/gene_metadata_rich.csv"))

# -----------------------------------------------------------------------------
# PROPHAGE NAMES  (read from input/prophage_list.txt at runtime if present)
# -----------------------------------------------------------------------------
_plist = INPUT_DIR / "prophage_list.txt"
PROPHAGE_NAMES = (
    [l.strip() for l in _plist.read_text().splitlines() if l.strip()]
    if _plist.exists() else []
)

# -----------------------------------------------------------------------------
# STEP 01 — PHOLD
# -----------------------------------------------------------------------------
PHOLD_THREADS      = 8
PHOLD_TSV_FILENAME = "phold_per_cds_predictions.tsv"

# -----------------------------------------------------------------------------
# STEP 02 — FOLDSEEK
# -----------------------------------------------------------------------------
_F = _C["foldseek"]
FOLDSEEK_3DI_DIR  = FOLDSEEK_DIR / "3di_tokens"
FOLDSEEK_3DI_BEST = FOLDSEEK_3DI_DIR / "best_hit.csv"
FOLDSEEK_3DI_TOP3 = FOLDSEEK_3DI_DIR / "top3.csv"
FOLDSEEK_3DI_ALL  = FOLDSEEK_3DI_DIR / "all_hits.csv"
FOLDSEEK_BEST_HIT = FOLDSEEK_3DI_BEST   # compat aliases for downstream scripts
FOLDSEEK_TOP3     = FOLDSEEK_3DI_TOP3
FOLDSEEK_ALL_HITS = FOLDSEEK_3DI_ALL

FOLDSEEK_CMD     = "foldseek"
FOLDSEEK_THREADS = _F["threads"]
FOLDSEEK_API_URL = _F["api_url"]
FOLDSEEK_EVALUE_MAX     = _F["evalue_max"]
FOLDSEEK_SCORE_OVERRIDE = _F["score_override"]

# --- Host-aware ranking boost (OPTIONAL, on by default) ----------------------
# Multiplies composite_score for an informative, non-promiscuous hit whose taxname
# matches HOST_GENUS. Ranking only: it never feeds the confidence tier, so it can
# reorder hits WITHIN a tier but never promote one across a tier boundary.
# Usefulness is host-dependent (a well-represented host in AFDB benefits more, and
# also carries more DB-composition bias), so it is a user choice:
#   config.yaml -> foldseek.host_boost   (1.0 disables)
#   env override -> PHAGEFACTOR_HOST_BOOST=1.0
# Requires taxonomy in the hit table AND a configured host_genus; otherwise inert.
HOST_BOOST = float(os.environ.get("PHAGEFACTOR_HOST_BOOST")
                   or _F.get("host_boost", 1.20))
PROSTT5_MASK_THRESHOLD  = _F["prostt5_mask_threshold"]
FS_CONFIDENT_EVALUE  = _F["confident_evalue"]
FS_GOOD_EVALUE       = _F["good_evalue"]
FS_BORDERLINE_SCORE  = _F["borderline_score"]

# Database roots. These MUST match config.sh, which resolves:
#     DB_ROOT          = ${PHAGEFACTOR_DB_ROOT:-<repo>/databases}
#     FOLDSEEK_DB_ROOT = ${FOLDSEEK_DB_ROOT:-${DB_ROOT}/foldseek_dbs}
#     PHAROKKA_DB      = ${PHAROKKA_DB:-${DB_ROOT}/pharokka_db}
# This side used to fall back to BARE RELATIVE paths when neither the env var
# nor the yaml was set, so steps/00b_setup_databases.sh (which writes to the
# bash default) installed the DBs somewhere step 02 would never look, and the
# search reported "database not found: pdb100_db/pdb100".
# Precedence, per key: env > config.yaml > same default as config.sh.
_DB_ROOT_BASE = Path(os.environ.get("PHAGEFACTOR_DB_ROOT") or (PROJECT_ROOT / "databases"))

_db_root_str = os.environ.get("FOLDSEEK_DB_ROOT") or _C["databases"].get("foldseek_db_root") or ""
_DB_ROOT = Path(_db_root_str) if _db_root_str else _DB_ROOT_BASE / "foldseek_dbs"
FOLDSEEK_LOCAL_DBS = {
    name: _DB_ROOT / rel
    for name, rel in _C["databases"]["foldseek_local_dbs"].items()
}
FOLDSEEK_TAXON_FILTER = dict(_C["databases"].get("foldseek_taxon_filter", {}))
_pharokka_str = os.environ.get("PHAROKKA_DB") or _C["databases"].get("pharokka_db") or ""
PHAROKKA_DB = Path(_pharokka_str) if _pharokka_str else _DB_ROOT_BASE / "pharokka_db"

# -----------------------------------------------------------------------------
# STEP 03 — COMPARISON THRESHOLDS
# -----------------------------------------------------------------------------
_CM = _C["compare"]
FUZZY_STRONG_THRESHOLD  = _CM["fuzzy_strong_threshold"]
FUZZY_PARTIAL_THRESHOLD = _CM["fuzzy_partial_threshold"]
PHOLD_TRUSTED_CONF = set(_CM["phold_trusted_conf"])
PHOLD_WEAK_CONF    = set(_CM["phold_weak_conf"])

# -----------------------------------------------------------------------------
# STEP 06 — PHYNTENY
# -----------------------------------------------------------------------------
PHYNTENY_THRESHOLD = _C["phynteny"]["threshold"]

# -----------------------------------------------------------------------------
# STEP 05 — OUTPUT
# -----------------------------------------------------------------------------
FINAL_ANNOTATIONS_TABLE = OUTPUT_DIR / "final_annotations_table.csv"
FINAL_ANNOTATIONS_XLSX  = OUTPUT_DIR / "final_annotations_table.xlsx"
UPDATED_GB              = OUTPUT_DIR / "updated_prophages.gb"
NOTE_TEMPLATE = (f"pipeline={_NOTE_TAG}; source={{source}}; {{evidence}}; "
                 "original_pharokka=hypothetical protein")

