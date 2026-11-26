# Generates reference output for the current PDB name retrieval implementation.
# Reads Foldseek PDB targets (one per line), resolves the protein name with the
# functions from foldfirst_parsing, and writes target + pdb_name to a TSV.
# Errors are logged to the console and to a log file next to this script.

import sys
from pathlib import Path

import pandas as pd
from loguru import logger

from foldfirst.results.foldfirst_parsing import (
    obsolete_ids,
    get_PDBid_chain_from_target,
    is_PDBid_active,
    get_PDB_entities_from_entry,
    get_PDB_entity_with_chain,
    get_protein_name_from_PDB_entity,
)

HERE = Path(__file__).resolve().parent
TARGETS_FILE = HERE / "foldseek_pdb_targets.txt"
OUTPUT_TSV = HERE / "pdb_names_reference.tsv"
LOG_FILE = HERE / "get_pdb_names.log"

# logger: console + file, so errors can be inspected after the run
logger.remove()
logger.add(sys.stderr, level="INFO")
logger.add(LOG_FILE, level="INFO", mode="w")

# read targets, one per row (skip blank lines, keep first occurrence of duplicates)
with open(TARGETS_FILE, "r", encoding="utf-8") as f:
    targets = [line.strip() for line in f if line.strip()]
filtered_df = pd.DataFrame({"target": targets}).drop_duplicates().reset_index(drop=True)
logger.info(f"Read {len(filtered_df)} unique targets from {TARGETS_FILE}.")

# parse PDB identifiers and chains from the target column
filtered_df[["pdb_id", "chain"]] = filtered_df.apply(lambda row: get_PDBid_chain_from_target(row["target"]), axis = 1, result_type = "expand")
# create overview of unique PDB identifiers to limit API calls
unique_pdb_ids = filtered_df["pdb_id"].unique()
logger.info(f"Found {len(unique_pdb_ids)} unique PDB IDs.")
pdb_to_entities = {}
# loop over PDB identifiers and fetch all possible entities
for pdb_id in unique_pdb_ids:
    if not is_PDBid_active(pdb_id, obsolete_ids):
        logger.warning(f"PDB ID {pdb_id} is obsolete, hence, no information will be fetched for this entry.")
        pdb_to_entities[pdb_id] = "obsolete"
        continue
    try:
        entities = get_PDB_entities_from_entry(pdb_id)
        pdb_to_entities[pdb_id] = entities
    except Exception as e:
        logger.error(f"[{pdb_id}] Failed to fetch entities from PDB: {e!r}")
# loop over PDB identifiers and chains to get the corresponding protein names
pdb_combo_to_name = {}
# create overview of unique PDB identifier-chain combinations to limit API calls
unique_pdb_combos = filtered_df[["pdb_id", "chain"]].drop_duplicates().values.tolist()
logger.info(f"Found {len(unique_pdb_combos)} unique PDB ID - chain combinations.")
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
        logger.error(f"[{pdb_id} chain {chain}] Failed to fetch protein name from PDB: {e!r}")
filtered_df["pdb_name"] = filtered_df.apply(lambda row: pdb_combo_to_name.get((row["pdb_id"], row["chain"])), axis=1)

n_missing = int(filtered_df["pdb_name"].isna().sum())
if n_missing:
    logger.warning(f"{n_missing} of {len(filtered_df)} targets have no pdb_name (see errors above).")

# save target (first column) and pdb_name (second column)
filtered_df[["target", "pdb_name"]].to_csv(OUTPUT_TSV, sep="\t", index=False)
logger.info(f"Saved results to {OUTPUT_TSV}.")
