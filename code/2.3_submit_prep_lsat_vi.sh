#!/bin/bash
#SBATCH --job-name=prep_lsat
#SBATCH --output=/scratch/lb968/prep_lsat_%a.log
#SBATCH --chdir=/scratch/lb968/
#SBATCH --time=05:00:00
#SBATCH --mem=70000
#SBATCH --array=1-1000

echo prep lsat vi
date

module load R/4.0.2

# run application
srun Rscript /projects/arctic/users/lberner/boreal_biome_shift/code/2.3_prep_lsat_vi_timeseries.R ${SLURM_ARRAY_TASK_ID} 
