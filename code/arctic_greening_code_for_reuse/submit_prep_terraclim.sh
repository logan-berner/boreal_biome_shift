#!/bin/bash
#SBATCH --job-name=prep_tclim
#SBATCH --output=/scratch/lb968/prep_tclim_%a.log
#SBATCH --chdir=/scratch/lb968/
#SBATCH --time=00:4:30
#SBATCH --mem=4500
#SBATCH --partition=all
#SBATCH --cpus-per-task=1
#SBATCH --array=1-144

echo prep tclim
date

module load R/3.6.2

# run application
srun Rscript /home/lb968/code/arctic_greening/0.prep_terraclim.R ${SLURM_ARRAY_TASK_ID} 
