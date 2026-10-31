#!/usr/bin/env python3

# This file contains the shared functions to parse results.

from pathlib import Path
from typing import Optional

import pandas as pd
from loguru import logger

def filter_hits(
    foldseek_df: pd.DataFrame,
    input_confidence_path: Optional[Path],
    structures: bool,
    no_plddt: bool,
):
    """
    Filter FoldSeek hits before further processing.
    
    """

    logger.info(f"Filtering Foldseek output.")

    # calculate qcov and tcov
    foldseek_df = calculate_qcov_tcov(foldseek_df)

    # add in confidence of input query (pLDDT or prostT5)
    if not no_plddt:
        foldseek_df = add_confidence_scores(foldseek_df, input_confidence_path, structures)

    # let's filter the hits based on the criteria
    filtered_df = foldseek_df.copy()
    filtered_df = filtered_df[
        # ensure at least domain hit on one side
        ((filtered_df["qCov"] >= 0.8) | (filtered_df["tCov"] >= 0.8)) 
    ].reset_index(drop=True)

    # for structure-based hits, we can also filter on the probability of belonging to the same SCOPe superfamily
    if structures:
            filtered_df = filtered_df[
                # ensure at least 50 % probability of belonging to the same SCOPe superfamily
                (filtered_df["prob"] >= 0.5) 
            ].reset_index(drop=True)

    # also filter on input quality
    if structures and not no_plddt:
        filtered_df = filtered_df[
            # for structures, this means at least an average pLDDT of 50
            (filtered_df["avg_plddt"] >= 50) 
        ].reset_index(drop=True)
    elif not structures:
        filtered_df = filtered_df[
            # for prostT5, this means at least an average prostT5 score of 30 
            # reference 'When masking residues above ProstT5 confidence of 30, performance decreased ...' (NAR article)
            (filtered_df["avg_prostt5"] >= 30) 
        ].reset_index(drop=True)

    return filtered_df


def calculate_qcov_tcov(foldseek_df):
    """
    Calculates and adds qCov and tCov to Foldseek pandas dataframe.
    Original Phold parsing function.

    Args:
        foldseek_df (pd.DataFrame): Foldseek pandas dataframe

    Returns:
        pd.DataFrame: DataFrame containing the Foldseek results with qCov and tCov columns
    """
    # add qcov and tcov
    foldseek_df["qCov"] = (
        (foldseek_df["qEnd"] - foldseek_df["qStart"]) / foldseek_df["qLen"]
    ).round(2)
    foldseek_df["tCov"] = (
        (foldseek_df["tEnd"] - foldseek_df["tStart"]) / foldseek_df["tLen"]
    ).round(2)

    # reorder
    qLen_index = foldseek_df.columns.get_loc("qLen")
    tLen_index = foldseek_df.columns.get_loc("tLen")

    new_column_order = (
        list(
            [
                col
                for col in foldseek_df.columns[: qLen_index + 1]
                if col not in ["qCov", "tStart", "tEnd", "tLen", "tCov"]
            ]
        )
        + ["qCov", "tStart", "tEnd", "tLen", "tCov"]
        + list(
            [
                col
                for col in foldseek_df.columns[tLen_index + 1 :]
                if col not in ["qCov", "tStart", "tEnd", "tLen", "tCov"]
            ]
        )
    )
    foldseek_df = foldseek_df.reindex(columns=new_column_order)

    return foldseek_df


def add_confidence_scores(foldseek_df, input_confidence_path, structures):
    """
    Add confidence scores to the Foldseek results dataframe.

    Args:
        foldseek_df (pd.DataFrame): Foldseek pandas dataframe
        input_confidence_path (Path): Path to the confidence scores file
        structures (bool): Flag indicating whether structures files are used.

    Returns:
        merged_df (pd.DataFrame): DataFrame containing the Foldseek results with added confidence scores
    """
    # read in input confidence scores
    if structures:
        confidence_df = pd.read_csv(
            input_confidence_path,
            sep = ",",
            header = None,
            names = ["cds_id", "avg_plddt"],
        )

    if not structures:
         confidence_df = pd.read_csv(
            input_confidence_path,
            sep = ",",  
            header = None,
            names = ["cds_id", "avg_prostt5"],
        )
        
    # merge the two dataframes on query/cds_id
    merged_df = pd.merge(
        foldseek_df,
        confidence_df,
        how="left",
        on="cds_id",
    )

    return merged_df

