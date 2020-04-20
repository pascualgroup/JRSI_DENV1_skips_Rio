#!/bin/bash
#SBATCH --job-name=sigma_M_Profile_A_7
#SBATCH --output=sigma_M_Profile_A_7_%A_%a.out
#SBATCH --error=error_sigma_M_Profile_A_7_%A_%a.err
#SBATCH --array=1-50
#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=28
#SBATCH --mem-per-cpu=2000
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=2000

echo $SLURM_ARRAY_TASK_ID

module load gcc
module load R/3.5.1
R CMD BATCH --vanilla '--args sigma_M A_7' MIF_run_Model_A_7.R OUT_si_M/out.$SLURM_ARRAY_TASK_ID 
