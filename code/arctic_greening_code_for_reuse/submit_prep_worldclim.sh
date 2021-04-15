#!/bin/bash
#SBATCH --job-name=prep_worldclim
#SBATCH --output=/scratch/lb968/prep_worldclim_%a.log
#SBATCH --chdir=/scratch/lb968/
#SBATCH --time=02:30:00
#SBATCH --mem=3GB
#SBATCH --cpus-per-task=1
#SBATCH --array=1-10

echo prep world clim data
date

module load R/3.5.2
R --version

# run application
Rscript /home/lb968/code/arctic_greening/0.prep_worldclim.R ${SLURM_ARRAY_TASK_ID} 
