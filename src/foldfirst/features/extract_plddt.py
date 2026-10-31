#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import numpy as np
from pathlib import Path

from loguru import logger
from Bio import SeqIO, PDB
from Bio.PDB import PDBParser, MMCIFParser


def write_plddt_from_structures(
    fasta_aa: Path,
    structure_dir: Path,
    output_path_mean: Path,
    proteins_flag: bool,
) -> None:
    """
    Write the mean pLDDT scores for each structure in the input to an output file.

    Args:
        fasta_aa (Path): Path to the amino-acid FASTA file.
        structure_dir (Path): Path to the directory containing .pdb or .cif structure files.
        output_path_mean (Path): Path to the output file where mean pLDDT scores will be stored.
        proteins_flag (bool): Flag - True if proteins-compare is run

    Returns:
        None
    """

    # read in amino-acid sequences
    sequences_aa = {}
    for record in SeqIO.parse(fasta_aa, "fasta"):
        sequences_aa[record.id] = str(record.seq)

    # lists all the structure files
    structure_files = [
        file
        for file in os.listdir(structure_dir)
        if file.endswith(".pdb") or file.endswith(".cif")
    ]

    # checks that proteins have structures
    for id in sequences_aa.keys():
        if proteins_flag:
            # will just be the CDS id if it is proteins-compare
            cds_id = id
        else:
            # in case the header has a colon in it - this will cause a bug if so
            cds_id = id.split(":")[1:]
            cds_id = ":".join(cds_id).strip()

        # record_id = id.split(":")[0]
        # this is potentially an issue if a contig has > 9999 AAs
        # need to fix with Pharokka possibly. Unlikely to occur but might!
        # enforce names as  "{cds_id}.pdb" or "{cds_id}.cif" (AF3)
        matching_files = [
            file
            for file in structure_files
            if f"{cds_id}.pdb" == file or f"{cds_id}.cif" == file
        ]

        # in case no structure is found, just skip and continue to the next one
            # user will have already gotten a warning about this from the create_foldseek_db_from_structures command
        if len(matching_files) == 0:
            continue

        # unlikely to happen but in case multiple structures are found, just use the first one and warn the user
        if len(matching_files) > 1:
            logger.warning(
                f"Multiple structures with matching CDS ids were found. Using the first one: {matching_files[0]}"
            )

        # now read in the structure and extract the pLDDT scores
        structure = matching_files[0]
        structure_path = Path(structure_dir) / structure
        structure_obj = load_structure(structure_path)
        scores = get_residue_confidences(structure_obj)
        scores = np.array(scores)
        # check if scores are normalized (0-1) or in the range 0-100. If normalized (happens for ESMatlas), multiply by 100
        if scores.max() <= 1.0:
            scores = scores * 100
            logger.warning(f"Detected normalized confidence (0-1) for {structure}. Assuming ESMatlas input, multiplying by 100. ")

        # now calculate average
        average_confidence = round(np.mean(scores), 2)

        # write to output file
        with open(output_path_mean, "a") as f:
            f.write(f"{cds_id},{average_confidence}\n")


def load_structure(filename):
    """
    Load a structure file. 
    This function checks if the file is a .pdb or .cif/.mmcif and uses the appropriate parser.

    Args:
        filename (Path): Path to the structure file.

    Returns:
        structure: The loaded structure object.
    """
    filename = Path(filename)
    if filename.suffix.lower() in [".cif", ".mmcif"]:
        parser = MMCIFParser(QUIET=True)
    elif filename.suffix.lower() == ".pdb":
        parser = PDBParser(QUIET=True)
    else:
        raise ValueError(f"Unsupported format: {filename}")

    return parser.get_structure("model", filename)


def get_residue_confidences(structure):
    """
    Extract confidence scores for each residue in the structure.

    Args:
        structure: The loaded structure object.

    Returns:
        List[float]: A list of confidence scores for each residue.
    """
    scores = []

    for residue in structure.get_residues():
        # ensure each residue is only counted once by only considering the alpha carbon (CA) atom
        if "CA" in residue:
            scores.append(residue["CA"].bfactor)

    return scores

