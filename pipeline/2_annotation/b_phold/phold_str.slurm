#!/bin/bash
#SBATCH --cluster=wice --partition=gpu_a100
#SBATCH --nodes=1 --gpus-per-node=1 --ntasks=18
#SBATCH --time=01:00:00

cd $SLURM_SUBMIT_DIR

source /data/leuven/331/vsc33164/miniconda3/bin/activate
conda activate phold_env

phold_db="/data/leuven/331/vsc33164/phold_dbs"

phold proteins-compare -i ./input_fastas/proteins_batch_0.fasta --structures --structure_dir ./input_structures/input_structures_batch_0 -d $phold_db -o ./output_compare_str_batch_0 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_1.fasta --structures --structure_dir ./input_structures/input_structures_batch_1 -d $phold_db -o ./output_compare_str_batch_1 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_2.fasta --structures --structure_dir ./input_structures/input_structures_batch_2 -d $phold_db -o ./output_compare_str_batch_2 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_3.fasta --structures --structure_dir ./input_structures/input_structures_batch_3 -d $phold_db -o ./output_compare_str_batch_3 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_4.fasta --structures --structure_dir ./input_structures/input_structures_batch_4 -d $phold_db -o ./output_compare_str_batch_4 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_5.fasta --structures --structure_dir ./input_structures/input_structures_batch_5 -d $phold_db -o ./output_compare_str_batch_5 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_6.fasta --structures --structure_dir ./input_structures/input_structures_batch_6 -d $phold_db -o ./output_compare_str_batch_6 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_7.fasta --structures --structure_dir ./input_structures/input_structures_batch_7 -d $phold_db -o ./output_compare_str_batch_7 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_8.fasta --structures --structure_dir ./input_structures/input_structures_batch_8 -d $phold_db -o ./output_compare_str_batch_8 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_9.fasta --structures --structure_dir ./input_structures/input_structures_batch_9 -d $phold_db -o ./output_compare_str_batch_9 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_10.fasta --structures --structure_dir ./input_structures/input_structures_batch_10 -d $phold_db -o ./output_compare_str_batch_10 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_11.fasta --structures --structure_dir ./input_structures/input_structures_batch_11 -d $phold_db -o ./output_compare_str_batch_11 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_12.fasta --structures --structure_dir ./input_structures/input_structures_batch_12 -d $phold_db -o ./output_compare_str_batch_12 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_13.fasta --structures --structure_dir ./input_structures/input_structures_batch_13 -d $phold_db -o ./output_compare_str_batch_13 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_14.fasta --structures --structure_dir ./input_structures/input_structures_batch_14 -d $phold_db -o ./output_compare_str_batch_14 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_15.fasta --structures --structure_dir ./input_structures/input_structures_batch_15 -d $phold_db -o ./output_compare_str_batch_15 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_16.fasta --structures --structure_dir ./input_structures/input_structures_batch_16 -d $phold_db -o ./output_compare_str_batch_16 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_17.fasta --structures --structure_dir ./input_structures/input_structures_batch_17 -d $phold_db -o ./output_compare_str_batch_17 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_18.fasta --structures --structure_dir ./input_structures/input_structures_batch_18 -d $phold_db -o ./output_compare_str_batch_18 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_19.fasta --structures --structure_dir ./input_structures/input_structures_batch_19 -d $phold_db -o ./output_compare_str_batch_19 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_20.fasta --structures --structure_dir ./input_structures/input_structures_batch_20 -d $phold_db -o ./output_compare_str_batch_20 -t 18 

