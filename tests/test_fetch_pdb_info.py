from pathlib import Path

import pandas as pd
import pytest
from loguru import logger

from foldfirst.results.foldfirst_parsing import (
    fetch_pdb_data,
    get_pdb_and_chain_name_and_uniprot,
    get_PDBid_chain_from_target,
    obsolete_ids,
    parse_graphql_json,
    query_pdb_graphql,
)

TESTS_DIR = Path(__file__).parent


@pytest.fixture(scope="session")
def pdb_entries() -> pd.DataFrame:
    """Load the PDB reference data once for the test session."""
    entries = pd.read_csv(
        TESTS_DIR / "pdb_names_reference.tsv",
        sep="\t",
    )
    required_columns = {"target", "pdb_name", "uniprot_id"}
    missing_columns = required_columns.difference(entries.columns)
    assert not missing_columns, (
        f"TSV is missing required columns: {', '.join(sorted(missing_columns))}"
    )
    return entries


@pytest.fixture
def load_pdb_entries(pdb_entries: pd.DataFrame):
    """Return a deterministic fixture factory for a requested number of entries."""

    def load(number_of_entries: int) -> pd.DataFrame:
        subset_size = min(number_of_entries, len(pdb_entries))
        return pdb_entries.sample(n=subset_size, random_state=42)

    return load


@pytest.fixture
def log_messages():
    """Collect the loguru messages of level WARNING and up emitted during a test."""
    messages = []
    sink_id = logger.add(lambda message: messages.append(message.record["message"]), level="WARNING")
    yield messages
    logger.remove(sink_id)


def test_query_pdb_graphql_general(load_pdb_entries):
    """Verify fetching returns every requested id.

    Goal:
        Exercise ``query_pdb_graphql`` with 100 unique PDB IDs.
    Expected outcome:
        A response object is returned with every PDB ID present.
    """
    subset = load_pdb_entries(100)
    pdb_ids = subset["target"].apply(lambda target: get_PDBid_chain_from_target(target)[0]).tolist()

    response = query_pdb_graphql(pdb_ids)

    # Test the validity of the response
    fetched_pdb_ids = [entry.get('rcsb_id') for entry in response.get('data', {}).get('entries', [])]

    missing_pdb_ids = set([id.upper() for id in pdb_ids]) - set(fetched_pdb_ids)
    assert not missing_pdb_ids, f"Missing PDB IDs in response: {missing_pdb_ids}"

    # Test the validity of the parser
    parsed_dict = parse_graphql_json(response)

    for _, row in subset.iterrows():
        target = row["target"]
        pdb_id, chain_id = get_PDBid_chain_from_target(target)
        parsed_entry = parsed_dict.get(pdb_id.upper())
        assert parsed_entry is not None, f"Missing parsed entry for PDB ID: {pdb_id}"
        parsed_chain = parsed_entry.get(chain_id)
        assert parsed_chain is not None, f"Missing parsed chain for PDB ID {pdb_id} and chain {chain_id}"
        assert parsed_chain["description"] == row["pdb_name"], (
            f"Mismatch in pdb_name for PDB ID {pdb_id}: "
            f"expected {row['pdb_name']}, got {parsed_chain['description']}"
        )
        if row['uniprot_id'] == 'no UniProt entry associated':
            assert len(parsed_chain['uniprot_ids']) == 0, (
                f"Mismatch in uniprot_id for PDB ID {pdb_id}: "
                f"expected no associated uniprot IDs, got {parsed_chain['uniprot_ids']}"
            )
        else:
            assert row["uniprot_id"] in parsed_chain["uniprot_ids"], (
                f"Mismatch in uniprot_id for PDB ID {pdb_id}: "
                f"expected {row['uniprot_id']}, got {parsed_chain['uniprot_ids']}"
            )

def test_query_pdb_graphql_with_invalid_ids(load_pdb_entries):
    """Verify fetching returns every valid id.

    Goal:
        Exercise ``query_pdb_graphql`` with 10 valid and 2 invalid PDB IDs.
    Expected outcome:
        A response object is returned with 10 valid PDB ID present.
    """
    subset = load_pdb_entries(10)
    pdb_ids = subset["target"].apply(lambda target: get_PDBid_chain_from_target(target)[0]).tolist()
    pdb_ids+= ['INVALID1', 'INVALID2']  # Add some invalid PDB IDs to test error handling

    response = query_pdb_graphql(pdb_ids)

    # Test the validity of the response
    fetched_pdb_ids = [entry.get('rcsb_id') for entry in response.get('data', {}).get('entries', [])]

    # Filter out the invalid PDB IDs from the expected IDs
    valid_pdb_ids = [id for id in pdb_ids if id not in ['INVALID1', 'INVALID2']]
    missing_pdb_ids = set([id.upper() for id in valid_pdb_ids]) - set(fetched_pdb_ids)
    assert not missing_pdb_ids, f"Missing PDB IDs in response: {missing_pdb_ids}"

    # Are the invalid IDs in the response?
    invalid_ids_in_response = [id for id in ['INVALID1', 'INVALID2'] if id in fetched_pdb_ids]
    assert len(invalid_ids_in_response) == 0, f"Invalid PDB IDs should not be in response: {invalid_ids_in_response}"

    # Is the number of entries in the response equal to the number of valid PDB IDs?
    assert len(fetched_pdb_ids) == len(valid_pdb_ids), (
        f"Expected {len(valid_pdb_ids)} valid PDB IDs, but got {len(fetched_pdb_ids)} entries in response."
    )

    # test the validity of the parser
    parsed_dict = parse_graphql_json(response)

    # Test the validity of the parsed dictionary
    assert isinstance(parsed_dict, dict), "Parsed dictionary should be a dictionary."
    assert len(parsed_dict) == len(valid_pdb_ids), (
        f"Expected {len(valid_pdb_ids)} entries in parsed dictionary, but got {len(parsed_dict)}."
    )

def test_query_pdb_graphql_with_only_invalid_ids(load_pdb_entries):
    """Verify fetching returns nothing when only queried with invalid PDB IDs.

    Goal:
        Exercise ``query_pdb_graphql`` with 2 unique invalid PDB IDs.
    Expected outcome:
        An empty response object is returned.
    """
    pdb_ids = ['INVALID1', 'INVALID2']  # Add some invalid PDB IDs to test error handling

    response = query_pdb_graphql(pdb_ids)

    fetched_pdb_ids = [entry.get('rcsb_id') for entry in response.get('data', {}).get('entries', [])]

    # Test the validity of the response
    # Are the invalid IDs in the response?
    invalid_ids_in_response = [id for id in ['INVALID1', 'INVALID2'] if id in fetched_pdb_ids]
    assert len(invalid_ids_in_response) == 0, f"Invalid PDB IDs should not be in response: {invalid_ids_in_response}"

    # Is the number of entries in the response equal to 0?
    assert len(fetched_pdb_ids) == 0, (
        f"Expected 0 valid PDB IDs, but got {len(fetched_pdb_ids)} entries in response."
    )

    # Test the validity of the parser
    parsed_dict = parse_graphql_json(response)
    assert isinstance(parsed_dict, dict), "Parsed dictionary should be a dictionary."
    assert len(parsed_dict) == 0, (
        f"Expected 0 entries in parsed dictionary, but got {len(parsed_dict)}."
    )



def test_pdb_retrieval_large_batch(load_pdb_entries):
    reference_df = load_pdb_entries(235)  # Load a larger batch of entries for testing
    filtered_df = reference_df[['target']].copy()

    # Make sure the copying happened correctly
    assert filtered_df.shape[1] == 1, "Filtered DataFrame should only contain the 'target' column."
    assert reference_df.shape[1] == 3, "Reference DataFrame should contain 'target', 'pdb_name', and 'uniprot_id' columns."

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
    # fetch the names and uniprot IDs for the active PDB identifiers
    pdb_to_chains_and_names = fetch_pdb_data([pdb_id for pdb_id in unique_pdb_ids if pdb_id not in obsolete_ids])
    filtered_df[["pdb_name", "uniprot_id"]] = filtered_df.apply(lambda row: get_pdb_and_chain_name_and_uniprot(pdb_to_chains_and_names, row["pdb_id"], row["chain"]), axis=1, result_type="expand")
    filtered_df = filtered_df.explode("uniprot_id")

    # Test that all the information from the reference are also retrieved here
    for _, row in reference_df.iterrows():
        target = row['target']
        assert target in filtered_df['target'].values, f"Target {target} is missing in the filtered DataFrame."

        relevant_retrieved_rows = filtered_df[filtered_df['target'] == target]

        assert row['pdb_name'] in relevant_retrieved_rows['pdb_name'].values, (
            f"Mismatch in pdb_name for target {target}: "
            f"expected {row['pdb_name']}, got {relevant_retrieved_rows['pdb_name'].values}"
        )

        assert row['uniprot_id'] in relevant_retrieved_rows['uniprot_id'].values, (
            f"Mismatch in uniprot_id for target {target}: "
            f"expected {row['uniprot_id']}, got {relevant_retrieved_rows['uniprot_id'].values}"
        )

    
def test_pdb_retrieval_large_batch_including_invalids(load_pdb_entries, log_messages):
    reference_df = load_pdb_entries(235)  # Load a larger batch of entries for testing
    # Add some invalid targets
    filtered_df = pd.concat(
        [reference_df[['target']], pd.DataFrame({'target': ['INVALID1', 'INVALID2']})],
        ignore_index=True,
    )

    # Make sure the copying happened correctly
    assert filtered_df.shape[1] == 1, "Filtered DataFrame should only contain the 'target' column."
    assert reference_df.shape[1] == 3, "Reference DataFrame should contain 'target', 'pdb_name', and 'uniprot_id' columns."

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
    # fetch the names and uniprot IDs for the active PDB identifiers
    pdb_to_chains_and_names = fetch_pdb_data([pdb_id for pdb_id in unique_pdb_ids if pdb_id not in obsolete_ids])
    filtered_df[["pdb_name", "uniprot_id"]] = filtered_df.apply(lambda row: get_pdb_and_chain_name_and_uniprot(pdb_to_chains_and_names, row["pdb_id"], row["chain"]), axis=1, result_type="expand")
    filtered_df = filtered_df.explode("uniprot_id")

    # Test that all the information from the reference are also retrieved here
    for _, row in reference_df.iterrows():
        target = row['target']
        assert target in filtered_df['target'].values, f"Target {target} is missing in the filtered DataFrame."

        relevant_retrieved_rows = filtered_df[filtered_df['target'] == target]

        assert row['pdb_name'] in relevant_retrieved_rows['pdb_name'].values, (
            f"Mismatch in pdb_name for target {target}: "
            f"expected {row['pdb_name']}, got {relevant_retrieved_rows['pdb_name'].values}"
        )

        assert row['uniprot_id'] in relevant_retrieved_rows['uniprot_id'].values, (
            f"Mismatch in uniprot_id for target {target}: "
            f"expected {row['uniprot_id']}, got {relevant_retrieved_rows['uniprot_id'].values}"
        )

    # Test that the invalid targets are handled correctly
    for invalid_target in ['INVALID1', 'INVALID2']:
        assert invalid_target in filtered_df['target'].values, f"Invalid target {invalid_target} is missing in the filtered DataFrame."
        relevant_retrieved_rows = filtered_df[filtered_df['target'] == invalid_target]
        assert relevant_retrieved_rows['pdb_name'].isnull().all(), (
            f"Expected no pdb_name for invalid target {invalid_target}, but got {relevant_retrieved_rows['pdb_name'].values}"
        )
        assert relevant_retrieved_rows['uniprot_id'].isnull().all(), (
            f"Expected no uniprot_id for invalid target {invalid_target}, but got {relevant_retrieved_rows['uniprot_id'].values}"
        )
        # test that logs are generated for invalid targets
        expected_message = f"No information found for PDB ID {invalid_target}."
        assert expected_message in log_messages, f"Missing log message: {expected_message}"
