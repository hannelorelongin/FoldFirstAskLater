#!/usr/bin/env python3
"""
config.py — phageFACTor configuration loader
=========================================================================
Reads the settings file ONCE and exposes them as constants
(`from config import FOLDSEEK_EVALUE_MAX, ...`).

The code (this repo, or a read-only conda install) and the PROJECT (settings,
input, runs) are separate:
  project dir   : $PHAGEFACTOR_PROJECT, else the current directory
  settings file : $PHAGEFACTOR_CONFIG, else <project>/config/config.yaml,
                  else the defaults shipped with the code (config/config.yaml)
  relative paths in it (input_dir, run_dir, db_root) resolve against the project dir.
  databases     : $PHAGEFACTOR_DB_ROOT > databases.db_root > db_root in the user
                  settings (~/.config/phagefactor/settings.yaml, written by
                  `phagefactor databases`) > <project>/databases
Environment variables (PHAGEFACTOR_INPUT, PHAGEFACTOR_RUN_DIR, FOLDSEEK_DB_ROOT,
PHAROKKA_DB, PHAGEFACTOR_HOST_GENUS) override the file.

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
CODE_DIR = Path(__file__).resolve().parents[2]          # repo root / install tree
DEFAULT_CONFIG = CODE_DIR / "config" / "config.yaml"


_env_project = os.environ.get("PHAGEFACTOR_PROJECT")
PROJECT_ROOT = Path(_env_project).expanduser().resolve() if _env_project else Path.cwd().resolve()


def _settings_file() -> Path:
    env = os.environ.get("PHAGEFACTOR_CONFIG")
    if env:
        return Path(env).expanduser().resolve()
    local = PROJECT_ROOT / "config" / "config.yaml"
    return local if local.is_file() else DEFAULT_CONFIG


CONFIG_FILE = _settings_file()
with open(CONFIG_FILE) as _fh:
    _C = yaml.safe_load(_fh)

# Per-user settings (shared database location for every project)
USER_SETTINGS = (Path(os.environ.get("XDG_CONFIG_HOME") or Path.home() / ".config")
                 / "phagefactor" / "settings.yaml")
try:
    _U = yaml.safe_load(USER_SETTINGS.read_text()) or {} if USER_SETTINGS.is_file() else {}
except (OSError, yaml.YAMLError):
    _U = {}

# -----------------------------------------------------------------------------
# ROOT / INPUT / RUN
# -----------------------------------------------------------------------------
def _p(rel: str) -> Path:
    return PROJECT_ROOT / rel

_P = _C["paths"]

# --- INPUT: the user's data (fasta/ + prophage_list.txt). --------------------
# Priority: $PHAGEFACTOR_INPUT  >  paths.input_dir (absolute ok)  >  <project>/input
_env_input = os.environ.get("PHAGEFACTOR_INPUT")
INPUT_DIR = Path(_env_input).resolve() if _env_input else _p(_P.get("input_dir", "input"))

def _pin(rel: str) -> Path:
    """Resolve a path inside INPUT_DIR (an 'input/' prefix is accepted)."""
    rel = rel[6:] if rel.startswith("input/") else rel
    return INPUT_DIR / rel

# --- RUN: where ALL outputs + logs go. ----------------------------------------
# Priority: $PHAGEFACTOR_RUN_DIR (set by the Snakefile for every job)
#           >  paths.run_dir (absolute, or relative to the project dir)
_env_run = os.environ.get("PHAGEFACTOR_RUN_DIR")
RUN_DIR = Path(_env_run).resolve() if _env_run else _p(_P.get("run_dir", "runs/run"))

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
LOGS_DIR         = _pr(_P.get("logs_dir", "logs"))

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

# Pre-CDS-computed sub-mode (paths inside the input dir)
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
FOLDSEEK_MAX_SEQS       = int(_F.get("max_seqs", 1000))
# Ranking malus for a LOW-INFORMATION description (lexicon tier 1: domain /
# family / repeat suffix, gene label).
LOW_INFO_MALUS = float(_F.get("low_info_malus", 0.60))
# Structural-confidence tier thresholds. Defaults preserve behaviour when a key
# is absent from config.yaml, so tiering stays config-driven and back compatible.
FS_CONFIDENT_EVALUE            = float(_F.get("confident_evalue", 1.0e-3))
FS_CONFIDENT_SCORE             = float(_F.get("confident_score", 200))
FS_CONFIDENT_TOP3_SCORE        = float(_F.get("confident_top3_score", 300))
FS_GOOD_EVALUE                 = float(_F.get("good_evalue", 0.01))
FS_GOOD_SCORE                  = float(_F.get("good_score", 200))
FS_GOOD_TOP3_EVALUE            = float(_F.get("good_top3_evalue", 0.05))
FS_BORDERLINE_EVALUE           = float(_F.get("borderline_evalue", 0.1))
FS_BORDERLINE_SCORE            = float(_F.get("borderline_score", 90))
FS_BORDERLINE_SCOREONLY_EVALUE = float(_F.get("borderline_scoreonly_evalue", 0.5))

# Database roots, per key: env > settings file > user settings > default
#     DB_ROOT          = $PHAGEFACTOR_DB_ROOT | databases.db_root | user db_root | <project>/databases
#     FOLDSEEK_DB_ROOT = $FOLDSEEK_DB_ROOT    | databases.foldseek_db_root | <DB_ROOT>/foldseek_dbs
#     PHAROKKA_DB      = $PHAROKKA_DB         | databases.pharokka_db      | <DB_ROOT>/pharokka_db
#     PHOLD_DB         = $PHOLD_DB            | databases.phold_db         | <DB_ROOT>/phold_db
_db_base_str = (os.environ.get("PHAGEFACTOR_DB_ROOT") or _C["databases"].get("db_root")
                or _U.get("db_root") or "")
DB_ROOT = _p(os.path.expanduser(_db_base_str)) if _db_base_str else PROJECT_ROOT / "databases"
_DB_ROOT_BASE = DB_ROOT

_db_root_str = os.environ.get("FOLDSEEK_DB_ROOT") or _C["databases"].get("foldseek_db_root") or ""
_DB_ROOT = _p(os.path.expanduser(_db_root_str)) if _db_root_str else _DB_ROOT_BASE / "foldseek_dbs"
FOLDSEEK_LOCAL_DBS = {
    name: _DB_ROOT / rel
    for name, rel in _C["databases"]["foldseek_local_dbs"].items()
}
FOLDSEEK_TAXON_FILTER = dict(_C["databases"].get("foldseek_taxon_filter", {}))
_pharokka_str = os.environ.get("PHAROKKA_DB") or _C["databases"].get("pharokka_db") or ""
PHAROKKA_DB = _p(os.path.expanduser(_pharokka_str)) if _pharokka_str else _DB_ROOT_BASE / "pharokka_db"
FOLDSEEK_DB_ROOT = _DB_ROOT
# phold's own database; when absent, phold uses its built-in default location
_phold_str = os.environ.get("PHOLD_DB") or _C["databases"].get("phold_db") or ""
PHOLD_DB = _p(os.path.expanduser(_phold_str)) if _phold_str else _DB_ROOT_BASE / "phold_db"
# NCBI taxdump (nodes.dmp + merged.dmp): taxid -> domain for hits without a
# FoldSeek taxlineage (web-API mode, older runs). Optional: without it the
# organism-name keyword lists are the fallback. See lib/taxonomy.py.
_taxdump_str = os.environ.get("PHAGEFACTOR_TAXDUMP") or _C["databases"].get("taxdump") or ""
TAXDUMP_DIR = _p(os.path.expanduser(_taxdump_str)) if _taxdump_str else _DB_ROOT_BASE / "taxdump"

# -----------------------------------------------------------------------------
# STEP 03 — COMPARISON THRESHOLDS
# -----------------------------------------------------------------------------
_CM = _C["compare"]
FUZZY_STRONG_THRESHOLD  = _CM["fuzzy_strong_threshold"]
FUZZY_PARTIAL_THRESHOLD = _CM["fuzzy_partial_threshold"]
PHOLD_TRUSTED_CONF = set(_CM["phold_trusted_conf"])
PHOLD_WEAK_CONF    = set(_CM["phold_weak_conf"])

# -----------------------------------------------------------------------------
# STEP 04 — CURATION
# -----------------------------------------------------------------------------
_CU = _C.get("curate") or {}
# Identity (PHold fident, 0-1) at which a NetFlaX/VFDB/CARD hit counts as
# "close" to its reference entry.
SUBDB_CLOSE_MIN_FIDENT = float(_CU.get("subdb_close_min_fident", 0.4))

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


if __name__ == "__main__":
    # `python config.py` prints the resolved locations (used by bin/phagefactor)
    for k in ("PROJECT_ROOT", "CONFIG_FILE", "INPUT_DIR", "RUN_DIR", "DB_ROOT",
              "PHAROKKA_DB", "PHOLD_DB", "FOLDSEEK_DB_ROOT", "TAXDUMP_DIR"):
        print(f"{k}={globals()[k]}")
