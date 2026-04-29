#!/usr/bin/env python3

# Compared to Phold, this file now only contains the functions to run searches against custom databases and parse their results. 
# Its functions are simply copied from the original Phold custom database functions in https://github.com/gbouras13/phold/blob/main/src/phold/results/topfunction.py.

from pathlib import Path

import pandas as pd
from loguru import logger


def get_topcustom_hits(
    result_tsv: Path,
    structures: bool,
    proteins_flag: bool,
) -> pd.DataFrame:
    """
    Process Foldseek output to extract top hits for custom searches

    Args:
        result_tsv (Path): Path to the Foldseek custom result TSV file.
        structures (bool): Flag indicating whether structures have been added.
        proteins_flag (bool): Flag indicating whether proteins are used.

    Returns:
        pd.DataFrame: DataFrame containing the top hits extracted from the custom Foldseek output.
    """

    logger.info("Processing custom database Foldseek output")

    col_list = [
        "query",
        "target",
        "bitscore",
        "fident",
        "evalue",
        "qStart",
        "qEnd",
        "qLen",
        "tStart",
        "tEnd",
        "tLen",
    ]

    # tmscore and lddt computed
    if structures:
        col_list += ["alntmscore", "lddt"]

    foldseek_df = pd.read_csv(
        result_tsv, delimiter="\t", index_col=False, names=col_list
    )

    # in case the foldseek output is empty
    if foldseek_df.empty:
        logger.warning(
            "Foldseek found no custom hits whatsoever - please check your custom database and input."
        )
        logger.warning("Phold will continue using only the default databases.")

    # issue #86 - convert all ~PIPE~ back to |
    foldseek_df["query"] = foldseek_df["query"].str.replace("~PIPE~", "|", regex=False)

    # gets the cds
    if structures is False and proteins_flag is False:
        # prostt5
        foldseek_df[["contig_id", "cds_id"]] = foldseek_df["query"].str.split(
            ":", expand=True, n=1
        )
        # dont need it
        foldseek_df.drop(columns=["contig_id"], inplace=True)
    # structures or proteins_flag or both
    else:
        foldseek_df["cds_id"] = foldseek_df["query"].str.replace(".pdb", "")
        foldseek_df["cds_id"] = foldseek_df["query"].str.replace(".cif", "")

    # clean up pdb/cif suffixes - target will be the hit
    foldseek_df["target"] = foldseek_df["target"].str.replace(".pdb", "")
    foldseek_df["target"] = foldseek_df["target"].str.replace(".cif", "")
    # split the target column as this will have phrog:protein

    tophit_custom_df = foldseek_df.loc[
        foldseek_df.groupby("query")["evalue"].idxmin()
    ].reset_index(drop=True)

    # dont need query or contig_id any more
    tophit_custom_df.drop(columns=["query"], inplace=True)

    return tophit_custom_df
