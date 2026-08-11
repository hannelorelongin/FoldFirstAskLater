#!/usr/bin/env python3

# New script to deal with searching Fold First Ask Later specific databases and parsing their results.
# This code currently only works on the Alphafold and PDB databases, but can be easily extended to other FoldSeek databases in the future if needed.
# Its functions are inspired by the original Phold functions, but strongly deviate in the parsing.

import json
import requests
import time
import pandas as pd
from loguru import logger
from pathlib import Path

import Bio
from Bio.PDB import *

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
    structures: bool,
    proteins_flag: bool,
    uniprot: bool,
    offline: bool,
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

    # tmscore and lddt computed
    if structures:
        col_list += ["alntmscore", "lddt"]

    foldseek_df = pd.read_csv(
        result_tsv, delimiter="\t", index_col=False, names=col_list
    )

    # in case the foldseek output is empty
    if foldseek_df.empty:
        logger.warning(
            "Foldseek found no Fold First Ask Later hits whatsoever."
        )

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
    foldseek_df["target"] = foldseek_df["target"].astype("str")
    foldseek_df["function"] = foldseek_df["target"].map(foldfirst_map)

    # filling any possible NaNs
    foldseek_df["function"] = foldseek_df["function"].fillna("unknown function")

    # --- hit-cap: order then keep top N per CDS ----------------------------------------------------------
    # Writing every hit is expensive and only the best + top-3 per CDS are used.
    # BUT the ranking used to cap decides which hits survive, and that is not
    # neutral: capping on RAW BITSCORE deletes hits a composite (bitscore x
    # phage-context boost) ranking would have promoted. Bitscore-ranked
    # consumers never notice; composite-ranked ones silently lose winners.
    #   FFAL_CAP_ORDER=bitscore   (default) top-N by raw bitscore
    #   FFAL_CAP_ORDER=composite            top-N by phageFACTor ordering
    #                                       (needs PHAGEFACTOR_ROOT)
    #   FFAL_MAX_HITS_PER_CDS=0             disable the cap entirely
    # Run FFAL twice into different output trees to get both hit sets.
    import os as _os
    _cap = int(_os.environ.get("FFAL_MAX_HITS_PER_CDS", "50"))
    _order = _os.environ.get("FFAL_CAP_ORDER", "bitscore").strip().lower()
    if _cap > 0 and not foldseek_df.empty and "cds_id" in foldseek_df.columns:
        _before = len(foldseek_df)
        _rank = "bitscore"
        if _order == "composite":
            try:
                import sys as _sys
                _pf = _os.environ.get("PHAGEFACTOR_ROOT", "")
                if _pf and _pf + "/scripts" not in _sys.path:
                    _sys.path.insert(0, _pf + "/scripts")
                from foldseek_scoring import (_phage_boost_factor as _boost,
                                               _host_boost_factor as _hboost,
                                               _is_same_host_hit as _samehost)
                _score = foldseek_df["bitscore"] * foldseek_df["function"].map(
                    lambda d: _boost(str(d)))
                # host boost (x1.20 by default) needs taxonomy on the hit rows;
                # inert when PHAGEFACTOR_HOST_GENUS is unset or taxname absent,
                # so this is safe to always attempt.
                _tax = next((c for c in ("taxname", "taxonomy", "taxlineage")
                             if c in foldseek_df.columns), None)
                if _tax:
                    _score = _score * [
                        _hboost(str(d), _samehost(str(t)))
                        for d, t in zip(foldseek_df["function"], foldseek_df[_tax])]
                else:
                    logger.info("composite cap: no taxonomy column, host boost "
                                "inert (phage boost only).")
                foldseek_df["composite_score"] = _score
                _rank = "composite_score"
            except Exception as _e:
                logger.warning(
                    f"FFAL_CAP_ORDER=composite but phageFACTor scoring could not "
                    f"be imported ({_e}). Set PHAGEFACTOR_ROOT. FALLING BACK to "
                    f"bitscore -- this run is NOT a composite-ordered run.")
        foldseek_df = (foldseek_df
                       .sort_values(_rank, ascending=False)
                       .groupby("cds_id", sort=False)
                       .head(_cap)
                       .reset_index(drop=True))
        logger.info(
            f"Capped hits at {_cap}/CDS ordered by {_rank}: {_before} -> "
            f"{len(foldseek_df)} rows ({database_name})."
        )

    # clean up pdb/cif suffixes in the target column
    foldseek_df["target"] = foldseek_df["target"].str.replace(".pdb", "")
    foldseek_df["target"] = foldseek_df["target"].str.replace(".cif", "")

    if not offline:
        if database_name == "af50m": 
            if uniprot:
                # parse UniProt identifiers from the target column
                foldseek_df["uniprot_id"] = foldseek_df.apply(lambda row: get_UniProtID_from_target(row["target"]), axis = 1)
                # create overview of unique UniProt identifiers to limit API calls
                unique_uniprot_ids_af = foldseek_df["uniprot_id"].unique()
                uniprot_id_to_name_af = fetch_UniProt_info(unique_uniprot_ids_af)
                # now add in the fetched information to the df
                foldseek_df["uniprot_name"] = foldseek_df["uniprot_id"].map(uniprot_id_to_name_af)
        elif database_name == "pdb":
            # parse PDB identifiers and chains from the target column
            foldseek_df[["pdb_id", "chain"]] = foldseek_df.apply(lambda row: get_PDBid_chain_from_target(row["target"]), axis = 1, result_type = "expand")
            # create overview of unique PDB identifiers to limit API calls
            unique_pdb_ids = foldseek_df["pdb_id"].unique()
            pdb_to_entities = {}  
            # loop over PDB identifiers and fetch all possible entities
            for pdb_id in unique_pdb_ids:
                if not check_PDBid_active(pdb_id, obsolete_ids):
                    logger.warning(f"PDB ID {pdb_id} is obsolete, hence, no information will be fetched for this entry.")
                    pdb_to_entities[pdb_id] = "obsolete"
                    continue
                try:
                    entities = get_PDB_entities_from_entry(pdb_id)
                    pdb_to_entities[pdb_id] = entities
                except Exception as e:
                        logger.error(f"Failed to fetch protein information from PDB due to the following error: {e}.")
                        logger.error(f"You can retry with --restart if needed, or without --uniprot.")
            # loop over PDB identifiers and chains to get the corresponding protein names
            pdb_combo_to_name = {}
            # create overview of unique PDB identifier-chain combinations to limit API calls
            unique_pdb_combos = foldseek_df[["pdb_id", "chain"]].drop_duplicates().values.tolist()
            for pdb_id, chain in unique_pdb_combos:
                entities = pdb_to_entities.get(pdb_id)
                if entities == "obsolete":
                    pdb_combo_to_name[(pdb_id, chain)] = "WARNING PDB entry is obsolete, no information fetched."
                    continue
                try:
                    entity = get_PDB_entity_with_chain(entities, pdb_id, chain)
                    protein_name = get_protein_name_from_PDB_entity(pdb_id, entity)
                    pdb_combo_to_name[(pdb_id, chain)] = protein_name
                except Exception as e:
                    logger.error(f"Failed to fetch protein information from PDB due to the following error: {e}.")
                    logger.error(f"You can retry with --restart if needed, or without --uniprot.")
            foldseek_df["pdb_name"] = foldseek_df.apply(lambda row: pdb_combo_to_name.get((row["pdb_id"], row["chain"])), axis=1)
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
                foldseek_df["uniprot_ids"] = foldseek_df.apply(lambda row: pdb_combo_to_uniprot.get((row["pdb_id"], row["chain"])), axis=1)
                # create overview of unique UniProt identifiers to limit API calls
                uniprot_ids_pdb = [uniprot_id for uniprot_list in foldseek_df["uniprot_ids"] for uniprot_id in uniprot_list]
                unique_uniprot_ids_pdb = list(set(uniprot_ids_pdb))
                # fetch information on the unique UniProt identifiers + deal with obsolete PDB entries / no UniProt connected (eg 7kln A1)
                unique_uniprot_ids_pdb = [uniprot_id for uniprot_id in unique_uniprot_ids_pdb if uniprot_id not in ["no UniProt entry fetched since PDB obsolete", "no UniProt entry associated"]]
                uniprot_id_to_name_pdb = fetch_UniProt_info(unique_uniprot_ids_pdb)
                uniprot_id_to_name_pdb["no UniProt entry fetched since PDB obsolete"] = "WARNING PDB entry is obsolete, no information fetched."
                uniprot_id_to_name_pdb["no UniProt entry associated"] = "WARNING No UniProt entry associated with this PDB entry, no information fetched."
                # now add in the fetched information to the df (taking into account there can be multiples)
                foldseek_df["uniprot_names"] = foldseek_df["uniprot_ids"].apply(lambda ids: [f"{uniprot_id_to_name_pdb.get(uniprot_id)} (UniProt ID: {uniprot_id})" for uniprot_id in ids])

    return foldseek_df

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

# check if PDB ID is obsolete
def check_PDBid_active(pdb_id, obsolete_list):
    if pdb_id.upper() in obsolete_list:
        return False
    else:
        return True

# from PDB ID, get all PDB polymer entities
def get_PDB_entities_from_entry(pdb_id):
    url = f"https://data.rcsb.org/rest/v1/core/entry/{pdb_id}"
    r = sess.get(url)
    r.raise_for_status()
    data = json.loads(r.text)
    return data["rcsb_entry_container_identifiers"].get("polymer_entity_ids")

# match PDB entity to protein chain
def get_PDB_entity_with_chain(entity_ids, pdb_id, chain):
    for entity in entity_ids:
        url = f"https://data.rcsb.org/rest/v1/core/polymer_entity/{pdb_id}/{entity}"
        r = sess.get(url)
        r.raise_for_status()
        data = json.loads(r.text)
        # deal with chain of the format A-1 
        # appears to happen for cases where authors reused chain identifiers (e.g. for a protein and a small molecule, e.g. 7qfn A-2)
        if "-" in chain:
            chain = chain.split("-")[0]
        # assumption: FoldSeek gives author chain naming, not PDB renamed chain names
        if chain in data["rcsb_polymer_entity_container_identifiers"].get("auth_asym_ids"):
            return entity
        else:
            continue

# get protein name based on the PDB ID and entity 
def get_protein_name_from_PDB_entity(pdb_id, entity):
    url = f"https://data.rcsb.org/rest/v1/core/polymer_entity/{pdb_id}/{entity}"
    r = sess.get(url)
    r.raise_for_status()
    data = json.loads(r.text)
    return data["rcsb_polymer_entity"].get("pdbx_description")

# get UniProt ID based on the PDB ID and entity 
def get_UniProt_from_PDB_entity(pdb_id, entity):
    url = f"https://data.rcsb.org/rest/v1/core/polymer_entity/{pdb_id}/{entity}"
    r = sess.get(url)
    r.raise_for_status()
    data = json.loads(r.text)
    return data["rcsb_polymer_entity_container_identifiers"].get("uniprot_ids")



