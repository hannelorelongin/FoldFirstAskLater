import os
import shutil
import sys
import tempfile
import time
from contextlib import contextmanager
from pathlib import Path
from typing import Any, Dict, Iterator, List, Union
from Bio import SeqIO
import click
from loguru import logger

@contextmanager
def atomic_write_path(target: Union[str, Path]) -> Iterator[Path]:
    """Yield a sibling temp path that is renamed over ``target`` on success.

    Usage::

        with atomic_write_path(final_path) as tmp:
            tmp.write_bytes(...)      # or SeqIO.write(..., tmp, "fasta"), etc.
        # On normal exit, tmp has been atomically renamed onto final_path.
        # On any exception (including KeyboardInterrupt), tmp is removed and
        # final_path is left exactly as it was before the with-block.

    The temp file is created in the **same directory** as ``target`` so the
    final ``os.replace`` is guaranteed atomic on POSIX (same filesystem) and
    on Windows (``os.replace`` overrides existing files atomically since
    Py3.3). ``BaseException`` is caught so Ctrl-C also triggers cleanup.

    Parameters
    ----------
    target
        Final destination path. Its parent directory is created if missing.
    """
    target = Path(target)
    target.parent.mkdir(parents=True, exist_ok=True)
    fd, tmp_name = tempfile.mkstemp(
        prefix=f".{target.name}.",
        suffix=".tmp",
        dir=str(target.parent),
    )
    os.close(fd)  # callers re-open via the path
    tmp_path = Path(tmp_name)
    try:
        yield tmp_path
    except BaseException:
        # Crash, Ctrl-C, SystemExit — drop the half-written temp and leave
        # the original target untouched.
        try:
            tmp_path.unlink()
        except FileNotFoundError:
            pass
        raise
    else:
        # Atomic rename. os.replace overwrites the destination if present.
        os.replace(tmp_path, target)


class OrderedCommands(click.Group):
    """This class will preserve the order of subcommands, which is useful when printing --help"""

    def list_commands(self, ctx: click.Context):
        return list(self.commands)


def foldfirst_base(rel_path):
    return os.path.join(os.path.dirname(os.path.realpath(__file__)), rel_path)


def get_version():
    with open(foldfirst_base("VERSION"), "r") as f:
        version = f.readline()
    return version


def echo_click(msg, log=None):
    click.echo(msg, nl=False, err=True)
    if log:
        with open(log, "a") as lo:
            lo.write(msg)


def print_citation():
    with open(foldfirst_base("CITATION"), "r") as f:
        for line in f:
            echo_click(line)


log_fmt = (
    "[<green>{time:YYYY-MM-DD HH:mm:ss}</green>] <level>{level: <8}</level> | "
    "<level>{message}</level>"
)

"""
begin and end functions
"""


def begin_foldfirst(params: Dict[str, Any], subcommand: str) -> float:
    """
    Begin Fold First Ask Later process.

    Parameters:
        params (Dict[str, Any]): A dictionary of parameters for Fold First Ask Later.
        subcommand (str): Subcommand indicating the foldfirst operation.

    Returns:
        float: Start time of the foldfirst process.
    """
    # get start time
    start_time = time.time()
    # initial logging stuff
    if subcommand != "autotune":
        log_file = os.path.join(params["--output"], f"foldfirst_{subcommand}_{start_time}.log")
        # adds log file
        logger.add(log_file)
    logger.add(lambda _: sys.exit(1), level="ERROR")

    logger.info("Fold First Ask Later: structure-informed function annotation of phage proteins")

    logger.info(f"You are using Fold First Ask Later version {get_version()}")
    logger.info("Repository homepage is https://github.com/hannelorelongin/FoldFirstAskLater")
    logger.info(f"You are running foldfirst {subcommand}")
    logger.info(f"Listing parameters")
    for key, value in params.items():
        logger.info(f"Parameter: {key} {value}")

    return start_time


def end_foldfirst(start_time: float, subcommand: str) -> None:
    """
    Finish Fold First Ask Later process and log elapsed time.

    Parameters:
        start_time (float): Start time of the process.
        subcommand (str): Subcommand name indicating the foldfirst operation.

    Returns:
        None
    """

    # Determine elapsed time
    elapsed_time = time.time() - start_time
    elapsed_time = round(elapsed_time, 2)

    # Show elapsed time for the process
    logger.info(f"foldfirst {subcommand} has finished")
    logger.info("Elapsed time: " + str(elapsed_time) + " seconds")



def remove_file(file_path: Path) -> None:
    """
    Remove a file if it exists.

    Parameters:
        file_path (Path): Path to the file to remove.

    Returns:
        None
    """
    if file_path.exists():
        file_path.unlink()  # Use unlink to remove the file


def remove_directory(dir_path: Path) -> None:
    """
    Remove a directory and all its contents if it exists.

    Parameters:
        dir_path (Path): Path to the directory to remove.

    Returns:
        None
    """
    if dir_path.exists():
        shutil.rmtree(dir_path)


def touch_file(path: Path) -> None:
    """
    Update the access and modification times of a file to the current time, creating the file if it does not exist.

    Parameters:
        path (Path): Path to the file.

    Returns:
        None
    """
    with open(path, "a"):
        os.utime(path, None)


def replace_pipe_in_fastq(input_path):
    """
    Solves issue #86 with the genbank format headers
    Reads a FASTA with Biopython, replace '~PIPE~' with '|' in headers, and write the result.
    """
    records = []
    for record in SeqIO.parse(input_path, "fasta"):
        record.id = record.id.replace("~PIPE~", "|")
        record.description = record.description.replace("~PIPE~", "|")
        records.append(record)
    
    # overwrites
    SeqIO.write(records, input_path, "fasta")

def clean_up_temporary_files(output: Path) -> None:
    """
    Clean up temporary files generated during the foldfirst process.

    Parameters:
        output (Path): Path to the output directory.

    Returns:
        None
    """
    result_high_tsv: Path = Path(output) / "foldseek_results_high.tsv"
    result_low_tsv: Path = Path(output) / "foldseek_results_low.tsv"
    result_tsv: Path = Path(output) / "foldseek_results_phold.tsv"
    result_tsv_custom: Path = Path(output) / "foldseek_results_custom.tsv"
    foldseek_db: Path = Path(output) / "foldseek_db"
    result_db_base: Path = Path(output) / "result_db"
    temp_db: Path = Path(output) / "temp_db"
    aln_db: Path = Path(output) / "aln_db"
    remove_directory(result_db_base)
    remove_directory(temp_db)
    remove_directory(foldseek_db)
    remove_directory(aln_db)
    remove_file(result_tsv)
    remove_file(result_tsv_custom)
    remove_file(result_high_tsv)
    remove_file(result_low_tsv)
