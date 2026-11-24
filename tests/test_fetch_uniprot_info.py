from pathlib import Path

import pandas as pd
import pytest

from foldfirst.results.foldfirst_parsing import fetch_UniProt_info, get_protein_names_string


TESTS_DIR = Path(__file__).parent


@pytest.fixture(scope="session")
def uniprot_entries() -> pd.DataFrame:
    """Load the UniProt reference data once for the test session."""
    entries = pd.read_csv(
        TESTS_DIR / "uniprot_ids_names_og.tsv",
        sep="\t",
    )
    required_columns = {"uniprot_id", "uniprot_name"}
    missing_columns = required_columns.difference(entries.columns)
    assert not missing_columns, (
        f"TSV is missing required columns: {', '.join(sorted(missing_columns))}"
    )
    return entries


@pytest.fixture
def load_uniprot_entries(uniprot_entries: pd.DataFrame):
    """Return a deterministic fixture factory for a requested number of entries."""

    def load(number_of_entries: int) -> pd.DataFrame:
        subset_size = min(number_of_entries, len(uniprot_entries))
        return uniprot_entries.sample(n=subset_size, random_state=42)

    return load


def test_fetch_uniprot_info(load_uniprot_entries):
    """Verify large-batch fetching returns every reference protein name.

    Goal:
        Exercise ``fetch_UniProt_info`` with a list larger than one API batch
        and with the last batch being smaller than the standard batch size.
    Expected outcome:
        The returned mapping contains the reference name for every requested ID.
    """
    subset = load_uniprot_entries(726)
    uniprot_ids = subset["uniprot_id"].tolist()

    fetched_names = fetch_UniProt_info(uniprot_ids)

    for row in subset.itertuples(index=False):
        assert fetched_names[str(row.uniprot_id)] == row.uniprot_name


def test_get_protein_names_string(load_uniprot_entries):
    """Verify a normal API batch is converted to an ID-to-name mapping.

    Goal:
        Exercise ``get_protein_names_string`` with 100 valid UniProt IDs.
    Expected outcome:
        Every returned name matches the corresponding reference TSV entry.
    """
    subset = load_uniprot_entries(100)
    uniprot_ids = subset["uniprot_id"].tolist()
    fetched_names = get_protein_names_string(uniprot_ids)

    for row in subset.itertuples(index=False):
        assert fetched_names[str(row.uniprot_id)] == row.uniprot_name


def test_worker_invalid_id(load_uniprot_entries):
    """Verify multiple invalid IDs are handled alongside valid IDs.

    Goal:
        Add two malformed IDs to a normal batch and fetch the mixed batch.
    Expected outcome:
        Valid IDs retain their reference names, while invalid IDs map to
        ``"IDENTIFIER NOT FOUND IN UNIPROT"``.
    """
    subset = load_uniprot_entries(4)
    invalid_id_rows = pd.DataFrame(
        {
            "uniprot_id": ["INVALID_ID_1", "INVALID_ID_2"],
            "uniprot_name": [
                "IDENTIFIER NOT FOUND IN UNIPROT",
                "IDENTIFIER NOT FOUND IN UNIPROT",
            ],
        }
    )
    subset = pd.concat([invalid_id_rows, subset], ignore_index=True)
    uniprot_ids = subset["uniprot_id"].tolist()
    fetched_names = get_protein_names_string(uniprot_ids)

    for row in subset.itertuples(index=False):
        assert fetched_names[str(row.uniprot_id)] == row.uniprot_name
    