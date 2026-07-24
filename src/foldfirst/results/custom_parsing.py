#!/usr/bin/env python3

# Compared to Phold, this file now only contains the functions to run searches against custom databases and parse their results. 
# Its functions are simply copied from the original Phold custom database functions in https://github.com/gbouras13/phold/blob/main/src/phold/results/topfunction.py.

from pathlib import Path

import polars as pl
from loguru import logger


def get_topcustom_hits(
    result_tsv: Path,
    structures: bool,
    proteins_flag: bool,
) -> pl.DataFrame:
    """Process Foldseek output to extract top custom-DB hits.
    The original
    ``foldseek_df.loc[foldseek_df.groupby("query")["evalue"].idxmin()]``
    pattern is replaced by ``sort.group_by.first()`` with a stable
    secondary sort on row order to reproduce pandas' idxmin tie-break.

    Args:
        result_tsv (Path): Path to the Foldseek custom result TSV file.
        structures (bool): Flag indicating whether structures have been added.
        proteins_flag (bool): Flag indicating whether proteins are used.

    Returns:
        pl.DataFrame: DataFrame containing the top hits extracted from the custom Foldseek output.
    """

    logger.info("Processing custom database Foldseek output")

    base_cols = [
        "query", "target", "bitscore", "fident", "evalue",
        "qStart", "qEnd", "qLen", "tStart", "tEnd", "tLen",
    ]

    # tmscore and lddt computed
    col_list = base_cols + (["alntmscore", "lddt"] if structures else [])

    foldseek_df = pl.read_csv(
        result_tsv,
        separator = "\t",
        has_header = False,
        new_columns = col_list,
        schema_overrides = {"evalue": pl.Utf8},
        infer_schema_length = 10_000,
    )

    # in case the foldseek output is empty
    if foldseek_df.is_empty():
        logger.warning(
            "Foldseek found no custom hits whatsoever - please check your custom database and input."
        )
        logger.warning("Fold First Ask Later will continue using only the default databases.")

    # issue #86 - convert all ~PIPE~ back to |
    foldseek_df = foldseek_df.with_columns(
        pl.col("query").str.replace_all("~PIPE~", "|", literal = True)
    )

    # Derive cds_id from query.
    if not structures and not proteins_flag:
        # prostt5 path: query = "<contig_id>:<cds_id>"
        # NOTE — the original code splits into contig_id+cds_id then
        # IMMEDIATELY drops contig_id. So we just produce cds_id directly.
        foldseek_df = foldseek_df.with_columns(
            pl.col("query")
            .str.splitn(":", 2)
            .struct.rename_fields(["_contig_id", "cds_id"])
            .alias("_q")
        ).unnest("_q").drop("_contig_id")
    else:
        # structures / proteins_flag: query is the cds_id (possibly with
        # ``.pdb``/``.cif`` suffix). Preserve the original pandas bug
        # where the second assignment overwrites the first — only
        # ``.cif`` actually gets stripped from the original query.
        foldseek_df = foldseek_df.with_columns(
            pl.col("query").str.replace_all(".cif", "", literal = True).alias("cds_id")
        )

    # Clean up ``.pdb`` and ``.cif`` suffixes from target.
    foldseek_df = foldseek_df.with_columns(
        pl.col("target")
        .str.replace_all(".pdb", "", literal = True)
        .str.replace_all(".cif", "", literal = True)
    )

    # Pick the lowest-evalue row per query. Add _orig_idx so ties break
    # on input order, matching pandas' idxmin (stable).
    tophit_custom = (
        foldseek_df
        .with_row_index("_orig_idx")
        .with_columns(pl.col("evalue").cast(pl.Float64).alias("_evalue_f"))
        .sort(["_evalue_f", "_orig_idx"])
        .group_by("query", maintain_order = False)
        .first()
        .drop(["_evalue_f", "_orig_idx"])
        .sort("query")  # match pandas groupby(sort = True) output order
    )

    # Drop the query column (downstream uses cds_id instead).
    tophit_custom = tophit_custom.drop("query")

    return tophit_custom
