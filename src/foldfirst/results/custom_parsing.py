#!/usr/bin/env python3

# Compared to Phold, this file now only contains the functions to run searches against custom databases and parse their results. 
# Its functions are simply copied from the original Phold custom database functions in https://github.com/gbouras13/phold/blob/main/src/phold/results/topfunction.py.

from pathlib import Path
from typing import Optional

import pandas as pd
from loguru import logger

from foldfirst.results.shared_parsing import filter_hits

def get_topcustom_hits(
    result_tsv: Path,
    input_confidence_path: Optional[Path],
    structures: bool,
    no_plddt: bool,
    proteins_flag: bool,
) -> pd.DataFrame:
    """
    Process Foldseek output to extract top hits for custom searches

    Args:
        result_tsv (Path): Path to the Foldseek custom result TSV file.
        input_confidence_path (Path): Path to the input confidence scores file (pLDDT or prostT5).
        structures (bool): Flag indicating whether structures have been added.
        no_plddt (bool): Flag indicating protein structures do not have pLDDT values stored in the Bfactor field.
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

    # prob, tmscore and lddt computed
    if structures:
        col_list += ["prob", "alntmscore", "lddt"]

    foldseek_df = pd.read_csv(
        result_tsv, delimiter="\t", index_col=False, names=col_list
    )

    # in case the foldseek output is empty
    if foldseek_df.empty:
        logger.warning(
            "Foldseek found no custom hits whatsoever - please check your custom database and input."
        )
        logger.warning("Fold First Ask Later will continue using only the default databases.")
    else:
        num_results = len(foldseek_df)
        logger.info(f"Foldseek found {num_results} hits in total.")


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

    # now apply filter to eliminate any hits based on poor input, low coverage, or low probability of homology
    filtered_df = filter_hits(foldseek_df, input_confidence_path, structures, no_plddt)
    num_filtered_results = len(filtered_df)
    if num_filtered_results == 0:
        logger.warning(
            "After filtering, no hits remain."
        )
    else:
        logger.info(f"After filtering, {num_filtered_results} hits remain.")

    tophit_custom_df = filtered_df.loc[
        filtered_df.groupby("query")["evalue"].idxmin()
    ].reset_index(drop=True)

    # dont need query or contig_id any more
    tophit_custom_df.drop(columns=["query"], inplace=True)

    return tophit_custom_df
