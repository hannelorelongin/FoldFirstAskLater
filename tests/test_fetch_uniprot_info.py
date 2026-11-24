import time as real_time
from pathlib import Path
from types import SimpleNamespace

import pandas as pd
import pytest
from loguru import logger

import foldfirst.results.foldfirst_parsing as foldfirst_parsing
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
    

# ---------------------------------------------------------------------------
# Progress logging tests (no network, no real waiting)
# ---------------------------------------------------------------------------


@pytest.fixture
def log_messages():
    """Collect loguru messages in a list (caplog does not see loguru)."""
    messages: list[str] = []
    handler_id = logger.add(messages.append, format="{message}", level="INFO")
    yield messages
    logger.remove(handler_id)


@pytest.fixture
def fake_uniprot(monkeypatch):
    """Fake clock plus a mocked ``get_protein_names_string``.

    Returns a function ``configure(seconds_per_batch)`` that sets how far the
    fake clock advances on every mocked UniProt call.
    """
    clock = {"now": 1000.0, "step": 0.0}

    fake_time = SimpleNamespace(
        time=lambda: clock["now"],
        strftime=real_time.strftime,
        gmtime=real_time.gmtime,
    )
    monkeypatch.setattr(foldfirst_parsing, "time", fake_time)

    def fake_get_names(ids):
        clock["now"] += clock["step"]
        return {uniprot_id: f"name_{uniprot_id}" for uniprot_id in ids}

    monkeypatch.setattr(foldfirst_parsing, "get_protein_names_string", fake_get_names)

    def configure(seconds_per_batch: float) -> None:
        clock["step"] = seconds_per_batch

    return configure


def _dummy_ids(n: int) -> list[str]:
    return [f"P{i:05d}" for i in range(n)]


def test_progress_logged_every_interval(fake_uniprot, log_messages):
    """Progress is logged once the 120 s interval has elapsed.

    Goal:
        726 IDs -> 8 batches (7 x 100 + 26), each taking 30 s on the fake clock.
    Expected outcome:
        Exactly two progress lines (after batch 4 at t=120 s and batch 8 at
        t=240 s) with correct counts, percentage and ETA; all names returned.
    """
    fake_uniprot(30)
    ids = _dummy_ids(726)

    result = fetch_UniProt_info(ids)

    assert set(result) == set(ids)
    progress = [m for m in log_messages if "progress" in m]
    assert len(progress) == 2
    assert "400 of 726 IDs processed (55.1%) - ETA: 00:01:37" in progress[0]
    assert "726 of 726 IDs processed (100.0%) - ETA: 00:00:00" in progress[1]


def test_no_progress_logged_below_interval(fake_uniprot, log_messages):
    """No progress line is emitted when the run is shorter than the interval.

    Goal:
        726 IDs, 5 s per batch -> 40 s in total on the fake clock.
    Expected outcome:
        No progress lines, but the start and completion messages are logged
        (start first, completion last).
    """
    fake_uniprot(5)

    fetch_UniProt_info(_dummy_ids(726))

    assert not [m for m in log_messages if "progress" in m]
    assert "726 unique UniProt identifiers" in log_messages[0]
    assert "Completed fetching" in log_messages[-1]
