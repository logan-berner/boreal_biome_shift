#!/bin/bash
#SBATCH --job-name=clean_lsat
#SBATCH --output=/scratch/lb968/clean_lsat%a.log
#SBATCH --chdir=/scratch/lb968/
#SBATCH --time=00:1:00
#SBATCH --mem=2000
#SBATCH --array=1-299

echo clean lsat data
date

module load R/4.0.2

# run application
srun Rscript /projects/arctic/users/lberner/boreal_biome_shift/code/2.1.1_clean_lsat_data.R ${SLURM_ARRAY_TASK_ID} 
