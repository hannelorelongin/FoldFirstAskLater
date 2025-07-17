#!/bin/bash
#SBATCH --nodes=1 --ntasks-per-node=72
#SBATCH --time=01:00:00

cd $SLURM_SUBMIT_DIR
export OMP_PROC_BIND=false

source /data/leuven/331/vsc33164/miniconda3/bin/activate
conda activate pharokka_env

pharokka_db="/data/leuven/331/vsc33164/pharokka_dbs"

pharokka_proteins.py -i ./input_fastas/proteins_batch_0.fasta -o ./output_batch_0 -t 72 -d $pharokka_db 
pharokka_proteins.py -i ./input_fastas/proteins_batch_1.fasta -o ./output_batch_1 -t 72 -d $pharokka_db 
pharokka_proteins.py -i ./input_fastas/proteins_batch_2.fasta -o ./output_batch_2 -t 72 -d $pharokka_db 
pharokka_proteins.py -i ./input_fastas/proteins_batch_3.fasta -o ./output_batch_3 -t 72 -d $pharokka_db 
pharokka_proteins.py -i ./input_fastas/proteins_batch_4.fasta -o ./output_batch_4 -t 72 -d $pharokka_db 
pharokka_proteins.py -i ./input_fastas/proteins_batch_5.fasta -o ./output_batch_5 -t 72 -d $pharokka_db 
pharokka_proteins.py -i ./input_fastas/proteins_batch_6.fasta -o ./output_batch_6 -t 72 -d $pharokka_db 
pharokka_proteins.py -i ./input_fastas/proteins_batch_7.fasta -o ./output_batch_7 -t 72 -d $pharokka_db 
pharokka_proteins.py -i ./input_fastas/proteins_batch_8.fasta -o ./output_batch_8 -t 72 -d $pharokka_db 
pharokka_proteins.py -i ./input_fastas/proteins_batch_9.fasta -o ./output_batch_9 -t 72 -d $pharokka_db 
pharokka_proteins.py -i ./input_fastas/proteins_batch_10.fasta -o ./output_batch_10 -t 72 -d $pharokka_db 
pharokka_proteins.py -i ./input_fastas/proteins_batch_11.fasta -o ./output_batch_11 -t 72 -d $pharokka_db 
pharokka_proteins.py -i ./input_fastas/proteins_batch_12.fasta -o ./output_batch_12 -t 72 -d $pharokka_db 
pharokka_proteins.py -i ./input_fastas/proteins_batch_13.fasta -o ./output_batch_13 -t 72 -d $pharokka_db 
pharokka_proteins.py -i ./input_fastas/proteins_batch_14.fasta -o ./output_batch_14 -t 72 -d $pharokka_db 
pharokka_proteins.py -i ./input_fastas/proteins_batch_15.fasta -o ./output_batch_15 -t 72 -d $pharokka_db 
pharokka_proteins.py -i ./input_fastas/proteins_batch_16.fasta -o ./output_batch_16 -t 72 -d $pharokka_db 
pharokka_proteins.py -i ./input_fastas/proteins_batch_17.fasta -o ./output_batch_17 -t 72 -d $pharokka_db 
pharokka_proteins.py -i ./input_fastas/proteins_batch_18.fasta -o ./output_batch_18 -t 72 -d $pharokka_db 
pharokka_proteins.py -i ./input_fastas/proteins_batch_19.fasta -o ./output_batch_19 -t 72 -d $pharokka_db 
pharokka_proteins.py -i ./input_fastas/proteins_batch_20.fasta -o ./output_batch_20 -t 72 -d $pharokka_db 

