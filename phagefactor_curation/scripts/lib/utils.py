#!/usr/bin/env python3
"""
utils.py -- Shared utilities for the phageFACTor prophage annotation pipeline.

Imported by all scripts. Do not run directly.
"""

import re
import sys
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

import pandas as pd

# Allow importing config whether running from project root or scripts/ subdir
_THIS_DIR = Path(__file__).parent
sys.path.insert(0, str(_THIS_DIR))
# scripts/lib/ holds the shared modules (config, utils, lexicon, ...).
# Harmless if they still sit next to this file.
_LIB_DIR = Path(__file__).resolve().parent / "lib"
if _LIB_DIR.is_dir():
    sys.path.insert(0, str(_LIB_DIR))
from config import UNINFORMATIVE_STRINGS, GENERIC_WORDS
from lexicon import UNCHAR_PATTERN


def _blocked(raw) -> bool:
    """True if a description carries no functional information.

    This side used to test the exact blocklist ONLY. Commit 2 moved eight of
    those entries into UNCHAR_PATTERN (as regex alternatives that also catch the
    variants an exact list cannot enumerate), so without consulting the pattern
    here those descriptions would silently become informative on the
    PHold/pharokka path.
    """
    s = str(raw).strip()
    return (s.lower() in UNINFORMATIVE_STRINGS
            or bool(UNCHAR_PATTERN.search(s)))


# -----------------------------------------------------------------------------
# LOGGING
# -----------------------------------------------------------------------------

def log(msg: str, end: str = "\n") -> None:
    """Print with immediate flush -- works well in long-running scripts."""
    print(msg, end=end, flush=True)


def section(title: str) -> None:
    """Print a section header."""
    bar = "=" * 60
    log(f"\n{bar}\n{title}\n{bar}")


# -----------------------------------------------------------------------------
# ANNOTATION QUALITY CHECKS
# -----------------------------------------------------------------------------

def is_informative(desc) -> bool:
    """
    Return True if desc carries real functional information.

    Treats None, NaN, empty string, and known uninformative phrases as False.
    Case-insensitive, strip-safe.
    """
    if desc is None:
        return False
    if not isinstance(desc, str):
        # e.g. float NaN
        try:
            import math
            if math.isnan(float(desc)):
                return False
        except (ValueError, TypeError):
            pass
        desc = str(desc)
    return not _blocked(desc)


def clean_str(v) -> str:
    """
    Normalise a value to a plain string.
    Returns 'NA' for None / NaN / known uninformative values.
    """
    if v is None:
        return "NA"
    if not isinstance(v, str):
        try:
            import math
            if math.isnan(float(v)):
                return "NA"
        except (ValueError, TypeError):
            pass
        v = str(v)
    s = v.strip()
    return "NA" if _blocked(s) else s


# -----------------------------------------------------------------------------
# FUZZY MATCHING
# -----------------------------------------------------------------------------

def tokenize(text: str) -> Set[str]:
    """
    Tokenise an annotation description into meaningful words.
    Returns empty set for uninformative inputs.
    """
    if not text or not isinstance(text, str):
        return set()
    if text.strip().lower() in UNINFORMATIVE_STRINGS:
        return set()
    tokens = re.findall(r"[a-zA-Z0-9]+", text.lower())
    return {t for t in tokens if t not in GENERIC_WORDS and len(t) > 2}


def fuzzy_score(d1: str, d2: str) -> float:
    """
    Jaccard similarity between tokenised descriptions.
    Returns 0.0 if either is uninformative or empty after tokenisation.
    """
    t1, t2 = tokenize(d1), tokenize(d2)
    if not t1 or not t2:
        return 0.0
    union = t1 | t2
    return len(t1 & t2) / len(union)


# -----------------------------------------------------------------------------
# GENBANK PARSING
# -----------------------------------------------------------------------------

def parse_prophage_name_from_fasta_header(header: str) -> Optional[str]:
    """
    Extract prophage name from an aa.fasta header.

    Expected format:
        >DSLQZLRW_CDS_0027, CJIE1_minor head protein and DNA pilot CDS
        -> returns "CJIE1"

    The prophage name is the text between ', ' and the first '_' that follows.
    Returns None if the pattern does not match.
    """
    # Pattern: after the first ", " there's PROPHAGE_NAME followed by "_"
    m = re.search(r",\s+([A-Z]+\d+)_", header)
    if m:
        return m.group(1)
    return None


def get_all_function_values(feature) -> List[str]:
    """
    Return a list of all /function qualifier values for a CDS feature.
    Handles both single and multi-valued /function qualifiers.
    Returns ['unknown function'] if absent.
    """
    funcs = feature.qualifiers.get("function", ["unknown function"])
    # BioPython returns a list always; clean each entry
    return [f.strip() for f in funcs if f.strip()]


def is_hypothetical_feature(feature) -> bool:
    """
    Return True if a CDS feature is annotated as hypothetical protein
    (i.e. product = 'hypothetical protein').
    """
    products = feature.qualifiers.get("product", [""])
    return any(p.strip().lower() == "hypothetical protein" for p in products)


def parse_gb_to_metadata(gb_path: Path) -> pd.DataFrame:
    """
    Parse the combined Pharokka GenBank file and return a DataFrame
    with one row per CDS gene:

    Columns:
        prophage        : LOCUS name (e.g. 'CJIE1')
        locus_tag       : e.g. 'DSLQZLRW_CDS_0001'
        product         : e.g. 'hypothetical protein' or 'CI-like repressor'
        function        : '; '-joined list of all /function values
        aa_length       : length of translated protein (aa)
        is_hypothetical : bool
        start           : CDS start (1-based, inclusive)
        end             : CDS end (1-based, inclusive)
        strand          : '+' or '-'
    """
    try:
        from Bio import SeqIO
    except ImportError:
        raise ImportError("BioPython is required. Install with: pip install biopython")

    rows = []
    for record in SeqIO.parse(str(gb_path), "genbank"):
        prophage_name = record.name  # LOCUS field
        for feature in record.features:
            if feature.type != "CDS":
                continue
            locus_tag = feature.qualifiers.get("locus_tag", [""])[0].strip()
            if not locus_tag:
                continue
            product = feature.qualifiers.get("product", ["unknown"])[0].strip()
            functions = get_all_function_values(feature)
            function_str = "; ".join(functions)
            translation = feature.qualifiers.get("translation", [""])[0].strip()
            aa_length = len(translation)
            is_hypo = is_hypothetical_feature(feature)

            # Coordinates
            loc = feature.location
            start = int(loc.start) + 1  # convert 0-based to 1-based
            end   = int(loc.end)
            strand = "+" if loc.strand == 1 else "-"

            rows.append({
                "prophage":        prophage_name,
                "locus_tag":       locus_tag,
                "product":         product,
                "function":        function_str,
                "aa_length":       aa_length,
                "is_hypothetical": is_hypo,
                "start":           start,
                "end":             end,
                "strand":          strand,
            })

    df = pd.DataFrame(rows)
    return df


# -----------------------------------------------------------------------------
# PROPHAGE NAME <-> LOCUS TAG CROSS-VALIDATION
# -----------------------------------------------------------------------------

def build_locus_to_prophage_map(metadata_df: pd.DataFrame) -> Dict[str, str]:
    """
    Return a dict mapping locus_tag -> prophage name from the metadata DataFrame.
    """
    return dict(zip(metadata_df["locus_tag"], metadata_df["prophage"]))


def validate_fasta_prophage_names(aa_fasta_path: Path,
                                  known_prophages: Set[str]) -> Tuple[int, List[str]]:
    """
    Check all aa.fasta headers for prophage name consistency.
    Returns (n_ok, list_of_problem_headers).
    """
    try:
        from Bio import SeqIO
    except ImportError:
        raise ImportError("BioPython is required.")

    problems = []
    n_ok = 0
    for rec in SeqIO.parse(str(aa_fasta_path), "fasta"):
        header = rec.description  # includes everything after '>'
        name = parse_prophage_name_from_fasta_header(header)
        if name is None:
            problems.append(f"Cannot extract prophage name: {header[:80]}")
        elif name not in known_prophages:
            problems.append(f"Unknown prophage '{name}': {header[:80]}")
        else:
            n_ok += 1
    return n_ok, problems


# -----------------------------------------------------------------------------
# PHOLD TSV HELPERS
# -----------------------------------------------------------------------------

# All 37 PHold per-CDS prediction columns (verified from real output)
PHOLD_COLUMNS = [
    "contig_id", "cds_id", "start", "end", "strand",
    "phrog", "function", "product", "annotation_method", "annotation_confidence",
    "transl_table", "bitscore", "fident", "evalue",
    "qStart", "qEnd", "qLen", "qCov",
    "tStart", "tEnd", "tLen", "tCov",
    "annotation_source", "tophit_protein",
    "function_with_highest_bitscore_proportion",
    "top_bitscore_proportion_not_unknown",
    "head_and_packaging_bitscore_proportion",
    "integration_and_excision_bitscore_proportion",
    "tail_bitscore_proportion",
    "moron_auxiliary_metabolic_gene_and_host_takeover_bitscore_proportion",
    "DNA_RNA_and_nucleotide_metabolism_bitscore_proportion",
    "connector_bitscore_proportion",
    "transcription_regulation_bitscore_proportion",
    "lysis_bitscore_proportion",
    "other_bitscore_proportion",
    "unknown_function_bitscore_proportion",
    "prostt5_confidence",
]

def load_phold_tsv(tsv_path: Path) -> pd.DataFrame:
    """Load a single phold_per_cds_predictions.tsv and return a DataFrame."""
    df = pd.read_csv(str(tsv_path), sep="\t", header=0,
                     low_memory=False, na_values=["", "NA", "na", "None"])
    # Validate column count
    if len(df.columns) != len(PHOLD_COLUMNS):
        log(f"  WARNING: {tsv_path.name} has {len(df.columns)} columns "
            f"(expected {len(PHOLD_COLUMNS)})")
    # Standardise column names to expected set
    df.columns = PHOLD_COLUMNS[:len(df.columns)]
    # Numeric conversions for key columns
    for col in ["bitscore", "fident", "evalue", "qCov", "tCov", "prostt5_confidence"]:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors="coerce")
    return df


def load_and_merge_phold_tsvs(phold_dir: Path,
                               prophage_names: List[str]) -> pd.DataFrame:
    """
    Load all per-prophage PHold TSV files and merge into one DataFrame.
    Adds 'prophage' column derived from the contig_id field.
    """
    dfs = []
    for name in prophage_names:
        tsv = phold_dir / name / PHOLD_TSV_FILENAME
        if not tsv.exists():
            # Also try with prefix naming
            tsv_alt = phold_dir / name / f"{name}_per_cds_predictions.tsv"
            if tsv_alt.exists():
                tsv = tsv_alt
            else:
                log(f"  WARNING: PHold output not found for {name}: {tsv}")
                continue
        df = load_phold_tsv(tsv)
        df["prophage"] = name
        dfs.append(df)
        log(f"  Loaded {name}: {len(df)} CDS")
    if not dfs:
        raise FileNotFoundError("No PHold TSV files found.")
    merged = pd.concat(dfs, ignore_index=True)
    return merged


# Import config name for use in scripts
try:
    from config import PHOLD_TSV_FILENAME
except ImportError:
    PHOLD_TSV_FILENAME = "phold_per_cds_predictions.tsv"


# -----------------------------------------------------------------------------
# FOLDSEEK M8 COLUMN DEFINITIONS
# -----------------------------------------------------------------------------

# 21-column Foldseek web API m8 format (verified from existing data)
M8_COLS = [
    "query", "target_raw", "pident", "alnlen", "mismatch", "gapopen",
    "qstart", "qend", "tstart", "tend",
    "prob", "evalue_raw", "score_raw", "lddtfull", "qcov_aa",
    "qaln", "taln", "tcoords", "tseq", "taxid", "taxname",
]
N_M8_COLS = len(M8_COLS)


def parse_m8_file(tsv_path: Path, gene_id: str) -> pd.DataFrame:
    """
    Parse a single Foldseek .tsv hits file for one gene.
    Returns DataFrame with standardised columns.
    """
    content = tsv_path.read_text().strip()
    if not content:
        return pd.DataFrame()
    lines = [l for l in content.splitlines() if l and not l.startswith("#")]
    if not lines:
        return pd.DataFrame()
    n_actual = len(lines[0].split("\t"))
    rows = [l.split("\t") for l in lines]
    # Pad/trim to expected column count
    rows = [r[:N_M8_COLS] + [""] * (N_M8_COLS - len(r)) for r in rows]
    df = pd.DataFrame(rows, columns=M8_COLS)
    df["gene"] = gene_id
    # Derive clean accession and description
    df["accession"]   = df["target_raw"].str.split(" ", n=1).str[0]
    df["description"] = df["target_raw"].str.split(" ", n=1).str[1].str.strip()
    # Numeric
    for col in ["pident", "prob", "evalue_raw", "score_raw", "qcov_aa", "lddtfull"]:
        df[col] = pd.to_numeric(df[col], errors="coerce")
    df["score"]  = df["score_raw"]
    df["evalue"] = df["evalue_raw"]
    return df


# -----------------------------------------------------------------------------
# ANNOTATION NOTE BUILDER
# -----------------------------------------------------------------------------

def build_note(source: str,
               phold_confidence: Optional[str] = None,
               phold_evalue: Optional[float] = None,
               phold_phrog: Optional[str] = None,
               foldseek_evalue: Optional[float] = None,
               foldseek_score: Optional[float] = None,
               foldseek_pident: Optional[float] = None) -> str:
    """
    Build a /note field string for an updated GenBank CDS feature.

    Example output:
        pipeline=phagefactor; source=phold+foldseek;
        phold_confidence=high; phold_evalue=1.2e-16; phold_phrog=phrog_373;
        foldseek_evalue=3.4e-12; foldseek_score=771; foldseek_pident=24.6%;
        original_pharokka=hypothetical protein
    """
    parts = ["pipeline=phagefactor", f"source={source}"]
    if phold_confidence:
        parts.append(f"phold_confidence={phold_confidence}")
    if phold_evalue is not None and not _is_nan(phold_evalue):
        parts.append(f"phold_evalue={phold_evalue:.3e}")
    if phold_phrog and str(phold_phrog) not in ("", "nan", "None", "No_PHROG"):
        parts.append(f"phold_phrog={phold_phrog}")
    if foldseek_evalue is not None and not _is_nan(foldseek_evalue):
        parts.append(f"foldseek_evalue={foldseek_evalue:.3e}")
    if foldseek_score is not None and not _is_nan(foldseek_score):
        parts.append(f"foldseek_score={foldseek_score:.0f}")
    if foldseek_pident is not None and not _is_nan(foldseek_pident):
        parts.append(f"foldseek_pident={foldseek_pident:.1f}%")
    parts.append("original_pharokka=hypothetical protein")
    return "; ".join(parts)


def _is_nan(v) -> bool:
    import math
    try:
        return math.isnan(float(v))
    except (TypeError, ValueError):
        return False
