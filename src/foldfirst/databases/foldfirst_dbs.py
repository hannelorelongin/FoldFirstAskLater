# New script to deal with installing Fold First Ask Later specific databases and validating their installation.
# This code currently only contains the Alphafold and PDB databases, but can be easily extended to other FoldSeek databases in the future if needed.
# Its functions are strongly based on the original Phold install and validate functions.

import os
from pathlib import Path
from loguru import logger

from foldfirst.databases.phold_dbs import foldseek_makepaddedseqdb
from foldfirst.utils.external_tools import ExternalTool
from foldfirst.utils.util import remove_directory

# rename databases for better readability in code 
# AlphaFold database rename is required to circumvent path issues
# swissprot patch: Swiss-Prot = manually reviewed UniProt (~570k), small but
# its descriptions are curated protein NAMES (high signal for annotation).
foldfirst_databases = {"PDB": "pdb", "Alphafold/UniProt50-minimal": "af50m",
                       "Alphafold/Swiss-Prot": "afdb_swissprot"}

foldfirst_db_names = {"PDB": ["pdb", "pdb.dbtype", "pdb.index",
                         "pdb_ca", "pdb_ca.dbtype", "pdb_ca.index",
                         "pdb_clu", "pdb_clu.dbtype", "pdb_clu.index",
                         "pdb_h", "pdb_h.dbtype", "pdb_h.index",
                         "pdb_ss", "pdb_ss.dbtype", "pdb_ss.index",
                         "pdb.lookup", "pdb.source", "pdb.version",
                         "pdb_mapping", "pdb_taxonomy"], 
                "Alphafold/UniProt50-minimal":
                    ["af50m", "af50m.dbtype", "af50m.index",
                     "af50m_ca", "af50m_ca.dbtype", "af50m_ca.index",
                     "af50m_h", "af50m_h.dbtype", "af50m_h.index",
                     "af50m_ss", "af50m_ss.dbtype", "af50m_ss.index",
                     "af50m.lookup", "af50m.version",
                     "af50m_mapping", "af50m_taxonomy"],
                "Alphafold/Swiss-Prot":
                    ["afdb_swissprot", "afdb_swissprot.dbtype", "afdb_swissprot.index",
                     "afdb_swissprot_h", "afdb_swissprot_h.dbtype", "afdb_swissprot_h.index",
                     "afdb_swissprot_ss", "afdb_swissprot_ss.dbtype", "afdb_swissprot_ss.index",
                     "afdb_swissprot.lookup", "afdb_swissprot.version",
                     "afdb_swissprot_mapping", "afdb_swissprot_taxonomy"]
                }

foldfirst_gpu_db_names = {"PDB": ["pdb_gpu"],
                          "Alphafold/UniProt50-minimal": ["af50m_gpu"],
                          "Alphafold/Swiss-Prot": ["afdb_swissprot_gpu"]}

def install_database(db_dir: Path, foldseek_gpu: bool, threads: int) -> None:
    """
    Install Fold First Ask Later specific databases.

    Args:
        db_dir (Path): directory where databases should be installed
        foldseek_gpu (bool): whether to install foldseek-gpu compatible db
        threads (int): number of threads for downloading
    """
    logger.info(f"Checking Fold First Ask Later databases in {db_dir}")
    
    for db in foldfirst_databases.keys():

        # instantiate tmp_dir for foldseek
        tmp_dir = db_dir / "tmp" 

        # check regular database installation
        downloaded_flag = check_foldseek_db_installation(db, db_dir)

        # in case regular database is missing, download and install it
        if not downloaded_flag:
            logger.info(f"Downloading {db}...")
            foldseek_downloaddb(db, db_dir, tmp_dir, threads)
            downloaded_flag = check_foldseek_db_installation(db, db_dir)
        
        # if regular database is present, check whether GPU files are requested/installed
        if downloaded_flag:
            logger.info(f"Fold First Ask Later {db} database installation OK.")
            remove_directory(tmp_dir)
            
            if foldseek_gpu:
                gpu_flag = check_foldseek_gpu_db_installation(db, db_dir)
                if gpu_flag:
                    logger.info(
                        "All Fold First Ask Later database files compatible with Foldseek-GPU are present"
                    )
                else:
                    logger.info(
                        "Some Fold First Ask Later database files compatible with Foldseek-GPU are missing"
                    )
                    logger.info("Creating them")
                    foldseek_makepaddedseqdb(db_dir, foldfirst_databases.get(db))
        
        # flag failed installation
        else:
            logger.error(
                f"Error: Fold First Ask Later {db} database not properly installed."
            )
            if foldseek_gpu:
                logger.warning(
                    "Skipping Foldseek-GPU compatible database creation since base database installation failed."
                )
        


def foldseek_downloaddb(db: str, db_dir: Path, tmp_dir: Path, threads: int) -> None:
    """
    Fold First Ask Later specific function to download FoldSeek database.

    Args:
        db (str): name of the database to download
        db_dir (Path): directory where databases should be installed
        tmp_dir (Path): temporary directory for FoldSeek download
        threads (int): number of threads for downloading
    """

    db_dir = Path(db_dir).resolve()
    tmp_dir = Path(tmp_dir).resolve()
    logdir = Path(db_dir) / "logdir"
    db_name = Path(db_dir) / foldfirst_databases.get(db)

    foldseek_downloaddb = ExternalTool(
        tool="foldseek",
        input="",
        output="",
        params=f"databases {db} {db_name} {tmp_dir} --threads {threads} --remove-tmp-files",
        logdir=logdir,
    )

    ExternalTool.run_tool(foldseek_downloaddb)

def check_foldseek_db_installation(db: str, db_dir: Path) -> bool:
    """
    Check if FoldSeek database files are installed.

    Args:
        db (str): name of the database to check
        db_dir (Path): directory where databases should be installed

    Returns:
        bool: True if all required database files are present, False otherwise
    """
    downloaded_flag = True

    for file_name in foldfirst_db_names.get(db):
        path = Path(db_dir).resolve() / file_name
        if not path.is_file():
            logger.warning(f"Fold First Ask Later {db} database file {path} is missing.")
            downloaded_flag = False

    return downloaded_flag


def check_foldseek_gpu_db_installation(db: str, db_dir: Path) -> bool:
    """
    Check if GPU-compatible FoldSeek database files are installed.

    Args:
        db (str): name of the database to check
        db_dir (Path): directory where databases should be installed

    Returns:
        bool: True if all required database files are present, False otherwise
    """
    gpu_flag = True

    for file_name in foldfirst_gpu_db_names.get(db):
        path = Path(db_dir) / file_name
        if not path.is_file():
            logger.warning(f"Fold First Ask Later {db} GPU database file {path} is missing.")
            gpu_flag = False

    return gpu_flag


def validate_db(database: str, default_dir: str, foldseek_gpu: bool) -> Path:
    """
    Validates the Fold First Ask Later databases are installed.

    Args:
        database str: the directory where the database is installed (or None to use default_dir).
        default_dir str: default DB location
        foldseek_gpu bool: whether to require foldseek-gpu compatible DB files

    Returns:
        Path: resolved database directory path.
    """
    # set default DB if not specified
    if database is not None:
        database: Path = Path(database)
    else:
        database = Path(default_dir)

    # check the databases are installed
    logger.info(f"Checking Fold First Ask Later database installation in {database}")

    missing = []
    for db_key in foldfirst_databases.keys():
        downloaded_flag = check_foldseek_db_installation(db_key, database)
        if not downloaded_flag:
            missing.append(db_key)

    if not missing:
        logger.info("All Fold First Ask Later database files are present")
    else:
        missing_str = ", ".join(missing)
        if database == Path(default_dir):  # default
            logger.error(
                f"Fold First Ask Later databases not found ({missing_str}). Please run foldfirst install to download and install the Fold First Ask Later databases"
            )
        else:  # specific
            logger.error(
                f"Fold First Ask Later databases not found ({missing_str}). Please run foldfirst install -d {database} to download and install the Fold First Ask Later databases"
            )

    if foldseek_gpu:
        for db_key in foldfirst_databases.keys():
            gpu_flag = check_foldseek_gpu_db_installation(db_key, database)
            if gpu_flag:
                logger.info(
                    f"All Fold First Ask Later {db_key} database files compatible with Foldseek-GPU are present"
                )
            else:
                logger.error(
                    f"Fold First Ask Later {db_key} database files compatible with Foldseek-GPU not found. Please run foldfirst install -d {database} --foldseek_gpu"
                )

    return database
