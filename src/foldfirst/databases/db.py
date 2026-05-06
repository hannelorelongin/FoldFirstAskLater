# Compared to Phold, this file now only contains the high-level functions to install and validate all databases, 
# which call the specific functions in the respective modules for Phold and Fold First Ask Later databases. 
# Its functions are strongly based on the original Phold install and validate functions.

from pathlib import Path
from loguru import logger

from foldfirst.databases.phold_dbs import install_database as install_phold_db, validate_db as validate_phold_db
from foldfirst.databases.foldfirst_dbs import install_database as install_foldfirst_db, validate_db as validate_foldfirst_db

def install_all_databases(db_dir: Path, foldseek_gpu: bool, extended_db: bool, threads: int) -> None:
    """
    Install all databases (Phold + Fold First Ask Later) sequentially.

    Args:
        db_dir (Path): root directory for all databases
        foldseek_gpu (bool): whether to install foldseek-gpu compatible database
        extended_db (bool): whether to download extended Phold DB
        threads (int): number of threads for downloading
    """
    logger.info("Starting database installation pipeline...")
    
    try:
        logger.info("Step 1: Installing Phold database...")
        install_phold_db(db_dir, foldseek_gpu, extended_db, threads)
        logger.info("Phold database installation completed.")
        
        logger.info("Step 2: Installing Fold First Ask Later databases...")
        install_foldfirst_db(db_dir, foldseek_gpu, threads)
        logger.info("Fold First Ask Later database installation completed.")
        
        logger.info("All databases installed successfully.")
    except Exception as e:
        logger.error(f"Database installation failed: {e}")
        raise


def validate_all_databases(database: str, default_dir: str, foldseek_gpu: bool) -> Path:
    """
    Validate the presence of all required databases (Phold + Fold First Ask Later).

    Args:
        database (str): path to the root database directory
        default_dir (str): default directory to use if database path is not provided
        foldseek_gpu (bool): whether to validate foldseek-gpu compatible database files

    Returns:
        Path: The path to the validated database directory.
    """
    logger.info("Starting database validation pipeline...")
    
    try:
        logger.info("Step 1: Validating Phold database...")
        validated_phold_db = validate_phold_db(database, default_dir, foldseek_gpu)
        
        logger.info("Step 2: Validating Fold First Ask Later databases...")
        validated_foldfirst_db = validate_foldfirst_db(database, default_dir, foldseek_gpu)

        logger.info("All databases validated successfully.")
        if validated_phold_db == validated_foldfirst_db:
            return validated_phold_db
        else:
            raise ValueError("Mismatch in validated database paths between Phold and Fold First Ask Later.")

    except Exception as e:
        logger.error(f"Database validation failed: {e}")
        raise