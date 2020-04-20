#!/bin/bash
#SBATCH --job-name=I_0_Profile_A_7
#SBATCH --output=I_0_Profile_A_7_%A_%a.out
#SBATCH --error=error_I_0_Profile_A_7_%A_%a.err
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
R CMD BATCH --vanilla '--args I_0 A_7' MIF_run_Model_A_7.R O/out.$SLURM_ARRAY_TASK_ID 
