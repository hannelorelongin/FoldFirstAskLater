#!/bin/bash

#SBATCH --nodes=1 --ntasks-per-node=24
#SBATCH --time=01:30:00

module load cluster/wice/batch atools/1.5.1
source <(aenv --data data_foldseek_5.csv --sniff 10000)

cd $SLURM_SUBMIT_DIR
export OMP_PROC_BIND=false

source /data/leuven/331/vsc33164/miniconda3/bin/activate
conda activate foldseekEnv

pdb_db="/staging/leuven//stg_00144/foldseek_db/pdb_2024mar"
af50m_db="/staging/leuven//stg_00144/foldseek_db/af50m_2024mar"

foldseek easy-search \
$relaxed_pdb_1 \
$pdb_db \
$output_pdb_1 \
$TMPDIR \
--format-output "query,target,fident,alnlen,mismatch,gapopen,qstart,qend,tstart,tend,evalue,bits,prob,lddt,lddtfull"

foldseek easy-search \
$relaxed_pdb_1 \
$af50m_db \
$output_af50m_1 \
$TMPDIR \
--format-output "query,target,fident,alnlen,mismatch,gapopen,qstart,qend,tstart,tend,evalue,bits,prob,lddt,lddtfull"

foldseek easy-search \
$relaxed_pdb_2 \
$pdb_db \
$output_pdb_2 \
$TMPDIR \
--format-output "query,target,fident,alnlen,mismatch,gapopen,qstart,qend,tstart,tend,evalue,bits,prob,lddt,lddtfull"

foldseek easy-search \
$relaxed_pdb_2 \
$af50m_db \
$output_af50m_2 \
$TMPDIR \
--format-output "query,target,fident,alnlen,mismatch,gapopen,qstart,qend,tstart,tend,evalue,bits,prob,lddt,lddtfull"

foldseek easy-search \
$relaxed_pdb_3 \
$pdb_db \
$output_pdb_3 \
$TMPDIR \
--format-output "query,target,fident,alnlen,mismatch,gapopen,qstart,qend,tstart,tend,evalue,bits,prob,lddt,lddtfull"

foldseek easy-search \
$relaxed_pdb_3 \
$af50m_db \
$output_af50m_3 \
$TMPDIR \
--format-output "query,target,fident,alnlen,mismatch,gapopen,qstart,qend,tstart,tend,evalue,bits,prob,lddt,lddtfull"

foldseek easy-search \
$relaxed_pdb_4 \
$pdb_db \
$output_pdb_4 \
$TMPDIR \
--format-output "query,target,fident,alnlen,mismatch,gapopen,qstart,qend,tstart,tend,evalue,bits,prob,lddt,lddtfull"

foldseek easy-search \
$relaxed_pdb_4 \
$af50m_db \
$output_af50m_4 \
$TMPDIR \
--format-output "query,target,fident,alnlen,mismatch,gapopen,qstart,qend,tstart,tend,evalue,bits,prob,lddt,lddtfull"

foldseek easy-search \
$relaxed_pdb_5 \
$pdb_db \
$output_pdb_5 \
$TMPDIR \
--format-output "query,target,fident,alnlen,mismatch,gapopen,qstart,qend,tstart,tend,evalue,bits,prob,lddt,lddtfull"

foldseek easy-search \
$relaxed_pdb_5 \
$af50m_db \
$output_af50m_5 \
$TMPDIR \
--format-output "query,target,fident,alnlen,mismatch,gapopen,qstart,qend,tstart,tend,evalue,bits,prob,lddt,lddtfull"
