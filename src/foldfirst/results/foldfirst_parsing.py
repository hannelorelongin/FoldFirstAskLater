#!/usr/bin/env python3

# New script to deal with searching Fold First Ask Later specific databases and parsing their results.
# This code currently only works on the Alphafold and PDB databases, but can be easily extended to other FoldSeek databases in the future if needed.
# Its functions are inspired by the original Phold functions, but strongly deviate in the parsing.

import json
from urllib import response
import requests
import time
import pandas as pd
from loguru import logger
from pathlib import Path
from typing import Optional

import Bio
from Bio.PDB import *

from foldfirst.results.shared_parsing import filter_hits
from foldfirst.results.foldfirst_classification import is_uninformative, is_informative_tt

# settings for request
sess = requests.Session()
adapter = requests.adapters.HTTPAdapter(max_retries = 10)
sess.mount("https://", adapter)

# obsolete PDB identifiers
obsolete_ids = Bio.PDB.PDBList.get_all_obsolete(Bio.PDB.PDBList())

def get_foldfirst_hits(
    result_tsv: Path,
    database: Path,
    database_name: str,
    input_confidence_path: Optional[Path],
    structures: bool,
    no_plddt: bool,
    proteins_flag: bool,
    uniprot: bool,
    offline: bool,
    binary_class: bool,
) -> pd.DataFrame:
    """
    Process Foldseek output to extract top hits for custom searches

    Args:
        result_tsv (Path): Path to the Foldseek custom result TSV file.
        database (Path): Path to the database directory.
        database_name (str): Name of the database.
        structures (bool): Flag indicating whether structures have been added.
        proteins_flag (bool): Flag indicating whether proteins are used.
        uniprot (bool): Flag indicating whether to fetch UniProt information.
        offline (bool): Flag indicating whether the script is running in offline mode, not fetching any information through APIs.

    Returns:
        pd.DataFrame: DataFrame containing the top hits extracted from the custom Foldseek output.
    """

    logger.info(f"Processing Fold First Ask Later Foldseek output ({database_name} database).")

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
            "Foldseek found no Fold First Ask Later hits whatsoever."
        )
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

    # now apply filter to filter out any hits based on poor input, low coverage, or low probability of homology
    filtered_df = filter_hits(foldseek_df, input_confidence_path, structures, no_plddt)
    num_filtered_results = len(filtered_df)
    if num_filtered_results == 0:
        logger.warning(
            "After filtering, no hits remain."
        )
    else:
        logger.info(f"After filtering, {num_filtered_results} hits remain.")

    # read in the mapping tsv
    foldfirst_annot_mapping_file: Path = Path(database) / f"{database_name}_h"
    with open(foldfirst_annot_mapping_file, "r", encoding = "utf-8-sig") as f:
        data = [line.rstrip("\n").split(" ", 1) for line in f if line.strip()]
    foldfirst_annot_mapping_df = pd.DataFrame(data, columns=["target", "function"])
    foldfirst_annot_mapping_df = foldfirst_annot_mapping_df.drop_duplicates() # safeguard against duplicates, should not happen
    foldfirst_annot_mapping_df["target"] = foldfirst_annot_mapping_df["target"].astype("str")
    foldfirst_annot_mapping_df["target"] = foldfirst_annot_mapping_df["target"].str.removeprefix("\x00").str.strip() # deal with 0-byte
    foldfirst_map = foldfirst_annot_mapping_df.set_index("target")["function"]

    # join the dfs using map
    filtered_df["target"] = filtered_df["target"].astype("str")
    filtered_df["function"] = filtered_df["target"].map(foldfirst_map)

    # filling any possible NaNs
    filtered_df["function"] = filtered_df["function"].fillna("unknown function")

    # clean up pdb/cif suffixes in the target column
    filtered_df["target"] = filtered_df["target"].str.replace(".pdb", "")
    filtered_df["target"] = filtered_df["target"].str.replace(".cif", "")

    # classify uninformative annotations
    if database_name not in ["pdb"]:
        if binary_class:
            filtered_df["informativeness"] = filtered_df.apply(lambda row: is_uninformative(row["function"]), axis=1)
        else:
            filtered_df["informativeness"] = filtered_df.apply(lambda row: is_informative_tt(row["function"]), axis=1)
        

    if not offline:
        if database_name == "af50m": 
            if uniprot:
                # parse UniProt identifiers from the target column
                filtered_df["uniprot_id"] = filtered_df.apply(lambda row: get_UniProtID_from_target(row["target"]), axis = 1)
                # create overview of unique UniProt identifiers to limit API calls
                unique_uniprot_ids_af = filtered_df["uniprot_id"].unique()
                uniprot_id_to_name_af = fetch_UniProt_info(unique_uniprot_ids_af)
                # now add in the fetched information to the df
                filtered_df["uniprot_name"] = filtered_df["uniprot_id"].map(uniprot_id_to_name_af)
                if binary_class:
                    filtered_df["uniprot_informativeness"] = filtered_df.apply(lambda row: is_uninformative(row["uniprot_name"]), axis=1)
                else:
                    filtered_df["uniprot_informativeness"] = filtered_df.apply(lambda row: is_informative_tt(row["uniprot_name"]), axis=1)
        elif database_name == "pdb":
            # parse PDB identifiers and chains from the target column
            filtered_df[["pdb_id", "chain"]] = filtered_df.apply(lambda row: get_PDBid_chain_from_target(row["target"]), axis = 1, result_type = "expand")
            # create overview of unique PDB identifiers to limit API calls
            unique_pdb_ids = filtered_df["pdb_id"].unique()
            # handle obsolete PDB identifiers first
            filtered_df["pdb_name"] = filtered_df.apply(
                lambda row: "WARNING PDB entry is obsolete, no information fetched." 
                if row["pdb_id"] in obsolete_ids 
                else None, 
                axis=1
            )
            # fetch the names for the active PDB identifiers
            pdb_to_chains_and_names = get_pdb_names([pdb_id for pdb_id in unique_pdb_ids if pdb_id not in obsolete_ids])
            filtered_df["pdb_name"] = filtered_df.apply(lambda row: get_pdb_and_chain_name(pdb_to_chains_and_names, row["pdb_id"], row["chain"]), axis=1)
            if binary_class:
                filtered_df["informativeness"] = filtered_df.apply(lambda row: is_uninformative(row["pdb_name"]), axis=1)
            else:
                filtered_df["informativeness"] = filtered_df.apply(lambda row: is_informative_tt(row["pdb_name"]), axis=1)
            # loop over PDB entries and fetch UniProt information
            if uniprot:
                pdb_combo_to_uniprot = {} 
                # loop over PDB entries and fetch uniprot identifiers
                for pdb_id, chain in unique_pdb_combos:
                    entities = pdb_to_entities.get(pdb_id)
                    if entities == "obsolete":
                        pdb_combo_to_uniprot[(pdb_id, chain)] = ["no UniProt entry fetched since PDB obsolete"]
                        continue
                    try:
                        entity = get_PDB_entity_with_chain(entities, pdb_id, chain)
                        uniprot_ids = get_UniProt_from_PDB_entity(pdb_id, entity)
                        if uniprot_ids is None:
                            logger.warning(f"No UniProt entry associated with PDB ID {pdb_id} chain {chain}, hence, no information will be fetched for this entry.")
                            pdb_combo_to_uniprot[(pdb_id, chain)] = ["no UniProt entry associated"]
                        else:
                            pdb_combo_to_uniprot[(pdb_id, chain)] = uniprot_ids
                    except Exception as e:
                        logger.error(f"Failed to fetch protein information from PDB due to the following error: {e}.")
                        logger.error(f"You can retry with --restart if needed, or without --uniprot.")
                # now add in the fetched information to the df
                filtered_df["uniprot_id"] = filtered_df.apply(lambda row: pdb_combo_to_uniprot.get((row["pdb_id"], row["chain"])), axis=1)
                filtered_df = filtered_df.explode("uniprot_id")  # explode the list of UniProt IDs into separate rows
                # create overview of unique UniProt identifiers to limit API calls
                unique_uniprot_ids_pdb = filtered_df["uniprot_id"].unique()
                # fetch information on the unique UniProt identifiers + deal with obsolete PDB entries / no UniProt connected (eg 7kln A1)
                unique_uniprot_ids_pdb = [uniprot_id for uniprot_id in unique_uniprot_ids_pdb if uniprot_id not in ["no UniProt entry fetched since PDB obsolete", "no UniProt entry associated"]]
                uniprot_id_to_name_pdb = fetch_UniProt_info(unique_uniprot_ids_pdb)
                uniprot_id_to_name_pdb["no UniProt entry fetched since PDB obsolete"] = "WARNING PDB entry is obsolete, no information fetched."
                uniprot_id_to_name_pdb["no UniProt entry associated"] = "WARNING No UniProt entry associated with this PDB entry, no information fetched."
                # now add in the fetched information to the df
                filtered_df["uniprot_name"] = filtered_df.apply(lambda row: uniprot_id_to_name_pdb.get(row["uniprot_id"]), axis = 1)
                if binary_class:
                    filtered_df["uniprot_informativeness"] = filtered_df.apply(lambda row: is_uninformative(row["uniprot_name"]), axis=1)
                else:
                    filtered_df["uniprot_informativeness"] = filtered_df.apply(lambda row: is_informative_tt(row["uniprot_name"]), axis=1)

    return filtered_df

################################
# UniProt fetching functions
#################################

# parse UniProt identifiers from AlphaFold database identifiers
def get_UniProtID_from_target(target):
    uniprot_id = target.split("-")[1]
    return uniprot_id

# fetch information from UniProt for a list of UniProt identifiers
def fetch_UniProt_info(uniprot_ids):
    # dict to store mapping of UniProt ID to protein name
    uniprot_id_to_name = {}
    # add in a progress logger
    total_ids = len(uniprot_ids)
    logger.info(f"Fetching protein names for {total_ids} unique UniProt identifiers.")
    start_time = time.time()
    last_logged_percent = 0
    # loop over entries and fetch
    for i, uniprot_id in enumerate(uniprot_ids):
        try:
            protein_name = get_protein_name_string(uniprot_id)
            uniprot_id_to_name[uniprot_id] = protein_name
        except Exception as e:
            logger.error(f"Failed to fetch protein name from UniProt due to the following error: {e}.")
            logger.error(f"You can retry with --restart if needed, or without --uniprot.")
        # update logger every 10% of progress
        current_percent = int(((i + 1) / total_ids) * 100)
        if current_percent >= last_logged_percent + 10:
            elapsed = time.time() - start_time
            remaining = total_ids - (i + 1)
            if i + 1 > 0:  # Avoid division by zero
                rate = (i + 1) / elapsed
                eta_seconds = remaining / rate
                eta_str = f"{int(eta_seconds // 60)}m {int(eta_seconds % 60)}s" if eta_seconds < 3600 else f"{int(eta_seconds // 3600)}h {int((eta_seconds % 3600) // 60)}m"
            else:
                eta_str = "unknown"
            logger.info(f"UniProt fetching progress: {current_percent}% complete ({i + 1} of {total_ids} IDs - ETA: {eta_str}).")
            last_logged_percent = current_percent
    logger.info(f"Completed fetching protein names from UniProt.")
    return uniprot_id_to_name

# get protein name, for a single UniProt ID
def get_protein_name_string(uniprot_id):
    # note: unsure if protein_name is a required field, if we get errors, look into this!
    url = f"https://rest.uniprot.org/uniprotkb/search?query={uniprot_id}&fields=protein_name&format=tsv"
    r = sess.get(url)
    r.raise_for_status()
    content = r.text
    names = content.split("\n")[1:-1]
    # if the UniProt entry was marked as obsolete, access its UniParc accession
    if "deleted" in names:
        return "WARNING UniProt entry is obsolete, no information fetched."
    else:
        return ";".join([str(name) for name in names])

################################
# PDB fetching functions
#################################

# parse PDB identifiers from PDB FoldSeekdatabase identifiers
def get_PDBid_chain_from_target(target):
    pdb_id = target.split("-")[0]
    chain = target.split("_")[-1]
    return pdb_id, chain

def query_pdb_graphql(pdb_ids: list[str]) -> dict:
    URL = "https://data.rcsb.org/graphql"
    QUERY = """
    query ($ids: [String!]!) {
    entries(entry_ids: $ids) {
        rcsb_id
        polymer_entities {
        rcsb_polymer_entity { pdbx_description }
        rcsb_polymer_entity_container_identifiers { auth_asym_ids asym_ids }
        }
    }
    }"""
    r = requests.post(URL, json={"query": QUERY, "variables": {"ids": ids[i:i+chunk]}}, timeout=60)
    r.raise_for_status()
    return r.json()

def parse_graphql_json(response_json: dict) -> dict[str, dict[str, str]]:
    """Extract PDB chain identifiers and polymer descriptions from a response.

    Reads entries from the GraphQL response's ``data.entries`` field and
    builds a mapping from each entry's RCSB PDB ID to its author chain IDs and
    corresponding polymer descriptions. Entries without an RCSB ID are
    ignored.

    Args:
        response_json: Decoded JSON response from the RCSB GraphQL API.

    Returns:
        A dictionary mapping PDB IDs to dictionaries of chain IDs and polymer
        descriptions. Returns an empty dictionary if there are no usable
        entries.
    """
    pdb_to_chains_and_names = {}
    for entry in response_json.get("data", {}).get("entries", []):
        pdb_id = entry.get("rcsb_id")
        if not pdb_id:
            continue
        chains_and_names = {}
        for entity in entry.get("polymer_entities", []):
            description = entity.get("rcsb_polymer_entity", {}).get("pdbx_description")
            auth_asym_ids = entity.get("rcsb_polymer_entity_container_identifiers", {}).get("auth_asym_ids", [])
            for chain in auth_asym_ids:
                chains_and_names[chain] = description
        pdb_to_chains_and_names[pdb_id] = chains_and_names
    return pdb_to_chains_and_names

def get_pdb_names(pdb_ids: list[str]) -> dict:
    """
    Fetches polymer descriptions for a list of PDB IDs using the RCSB GraphQL API.

    Args:
        pdb_ids (list[str]): List of active PDB IDs.

    Returns:
        dict[str, dict[str, str]]: A dictionary mapping PDB IDs to dictionaries of chain IDs and polymer descriptions.
    """
    # handle obsolete PDB identifiers first
    obsolete_ids_in_list = [pdb_id for pdb_id in pdb_ids if pdb_id in obsolete_ids]
    if obsolete_ids_in_list:
        logger.warning(f"The following PDB IDs are obsolete and will be skipped: {', '.join(obsolete_ids_in_list)}")
        pdb_ids = [pdb_id for pdb_id in pdb_ids if pdb_id not in obsolete_ids]
    chunk = 100  # Number of PDB IDs to query at once
    pdb_to_chains_and_names = {}
    for i in range(0, len(pdb_ids), chunk):
        try:
            response_json = query_pdb_graphql(pdb_ids[i:i+chunk])
            pdb_to_chains_and_names.update(parse_graphql_json(response_json))
        except Exception as e:
                logger.error(f"Failed to fetch protein information from PDB due to the following error: {e}.")
                logger.error("You can retry with --restart if needed, or without --uniprot.")
    return pdb_to_chains_and_names

def get_pdb_and_chain_name(pdb_to_chains_and_names: dict, pdb_id: str, chain: str) -> Optional[str]:
    """
    Retrieves the polymer description for a specific PDB ID and chain.

    Args:
        pdb_to_chains_and_names (dict): A dictionary mapping PDB IDs to dictionaries of chain IDs and polymer descriptions.
        pdb_id (str): The PDB ID to look up.
        chain (str): The chain ID to look up.

    Returns:
        Optional[str]: The polymer description if found, otherwise None.
    """
    chains_and_names = pdb_to_chains_and_names.get(pdb_id)
    if chains_and_names is None:
        logger.warning(f"No information found for PDB ID {pdb_id}.")
        return None
    name = chains_and_names.get(chain)
    if name is None:
        logger.warning(f"No information found for PDB ID {pdb_id} chain {chain}.")
    return name



