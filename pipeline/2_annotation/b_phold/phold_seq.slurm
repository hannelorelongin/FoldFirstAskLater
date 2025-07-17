#!/bin/bash
#SBATCH --cluster=wice --partition=gpu_a100
#SBATCH --nodes=1 --gpus-per-node=1 --ntasks=18
#SBATCH --time=06:00:00

cd $SLURM_SUBMIT_DIR

source /data/leuven/331/vsc33164/miniconda3/bin/activate
conda activate phold_env

phold_db="/data/leuven/331/vsc33164/phold_dbs"

phold proteins-predict -i ./input_fastas/proteins_batch_0.fasta -d $phold_db -o ./output_predict_seq_batch_0 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_0.fasta --predictions_dir ./output_predict_seq_batch_0 -d $phold_db -o ./output_compare_seq_batch_0 -t 18 
phold proteins-predict -i ./input_fastas/proteins_batch_1.fasta -d $phold_db -o ./output_predict_seq_batch_1 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_1.fasta --predictions_dir ./output_predict_seq_batch_1 -d $phold_db -o ./output_compare_seq_batch_1 -t 18 
phold proteins-predict -i ./input_fastas/proteins_batch_2.fasta -d $phold_db -o ./output_predict_seq_batch_2 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_2.fasta --predictions_dir ./output_predict_seq_batch_2 -d $phold_db -o ./output_compare_seq_batch_2 -t 18 
phold proteins-predict -i ./input_fastas/proteins_batch_3.fasta -d $phold_db -o ./output_predict_seq_batch_3 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_3.fasta --predictions_dir ./output_predict_seq_batch_3 -d $phold_db -o ./output_compare_seq_batch_3 -t 18 
phold proteins-predict -i ./input_fastas/proteins_batch_4.fasta -d $phold_db -o ./output_predict_seq_batch_4 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_4.fasta --predictions_dir ./output_predict_seq_batch_4 -d $phold_db -o ./output_compare_seq_batch_4 -t 18 
phold proteins-predict -i ./input_fastas/proteins_batch_5.fasta -d $phold_db -o ./output_predict_seq_batch_5 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_5.fasta --predictions_dir ./output_predict_seq_batch_5 -d $phold_db -o ./output_compare_seq_batch_5 -t 18 
phold proteins-predict -i ./input_fastas/proteins_batch_6.fasta -d $phold_db -o ./output_predict_seq_batch_6 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_6.fasta --predictions_dir ./output_predict_seq_batch_6 -d $phold_db -o ./output_compare_seq_batch_6 -t 18 
phold proteins-predict -i ./input_fastas/proteins_batch_7.fasta -d $phold_db -o ./output_predict_seq_batch_7 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_7.fasta --predictions_dir ./output_predict_seq_batch_7 -d $phold_db -o ./output_compare_seq_batch_7 -t 18 
phold proteins-predict -i ./input_fastas/proteins_batch_8.fasta -d $phold_db -o ./output_predict_seq_batch_8 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_8.fasta --predictions_dir ./output_predict_seq_batch_8 -d $phold_db -o ./output_compare_seq_batch_8 -t 18 
phold proteins-predict -i ./input_fastas/proteins_batch_9.fasta -d $phold_db -o ./output_predict_seq_batch_9 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_9.fasta --predictions_dir ./output_predict_seq_batch_9 -d $phold_db -o ./output_compare_seq_batch_9 -t 18 
phold proteins-predict -i ./input_fastas/proteins_batch_10.fasta -d $phold_db -o ./output_predict_seq_batch_10 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_10.fasta --predictions_dir ./output_predict_seq_batch_10 -d $phold_db -o ./output_compare_seq_batch_10 -t 18 
phold proteins-predict -i ./input_fastas/proteins_batch_11.fasta -d $phold_db -o ./output_predict_seq_batch_11 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_11.fasta --predictions_dir ./output_predict_seq_batch_11 -d $phold_db -o ./output_compare_seq_batch_11 -t 18 
phold proteins-predict -i ./input_fastas/proteins_batch_12.fasta -d $phold_db -o ./output_predict_seq_batch_12 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_12.fasta --predictions_dir ./output_predict_seq_batch_12 -d $phold_db -o ./output_compare_seq_batch_12 -t 18 
phold proteins-predict -i ./input_fastas/proteins_batch_13.fasta -d $phold_db -o ./output_predict_seq_batch_13 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_13.fasta --predictions_dir ./output_predict_seq_batch_13 -d $phold_db -o ./output_compare_seq_batch_13 -t 18 
phold proteins-predict -i ./input_fastas/proteins_batch_14.fasta -d $phold_db -o ./output_predict_seq_batch_14 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_14.fasta --predictions_dir ./output_predict_seq_batch_14 -d $phold_db -o ./output_compare_seq_batch_14 -t 18 
phold proteins-predict -i ./input_fastas/proteins_batch_15.fasta -d $phold_db -o ./output_predict_seq_batch_15 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_15.fasta --predictions_dir ./output_predict_seq_batch_15 -d $phold_db -o ./output_compare_seq_batch_15 -t 18 
phold proteins-predict -i ./input_fastas/proteins_batch_16.fasta -d $phold_db -o ./output_predict_seq_batch_16 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_16.fasta --predictions_dir ./output_predict_seq_batch_16 -d $phold_db -o ./output_compare_seq_batch_16 -t 18 
phold proteins-predict -i ./input_fastas/proteins_batch_17.fasta -d $phold_db -o ./output_predict_seq_batch_17 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_17.fasta --predictions_dir ./output_predict_seq_batch_17 -d $phold_db -o ./output_compare_seq_batch_17 -t 18 
phold proteins-predict -i ./input_fastas/proteins_batch_18.fasta -d $phold_db -o ./output_predict_seq_batch_18 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_18.fasta --predictions_dir ./output_predict_seq_batch_18 -d $phold_db -o ./output_compare_seq_batch_18 -t 18 
phold proteins-predict -i ./input_fastas/proteins_batch_19.fasta -d $phold_db -o ./output_predict_seq_batch_19 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_19.fasta --predictions_dir ./output_predict_seq_batch_19 -d $phold_db -o ./output_compare_seq_batch_19 -t 18 
phold proteins-predict -i ./input_fastas/proteins_batch_20.fasta -d $phold_db -o ./output_predict_seq_batch_20 -t 18 
phold proteins-compare -i ./input_fastas/proteins_batch_20.fasta --predictions_dir ./output_predict_seq_batch_20 -d $phold_db -o ./output_compare_seq_batch_20 -t 18 

