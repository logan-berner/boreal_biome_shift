#!/bin/bash
#SBATCH --job-name=prep_lsat
#SBATCH --output=/scratch/lb968/prep_lsat_%a.log
#SBATCH --chdir=/scratch/lb968/
#SBATCH --time=00:26:00
#SBATCH --mem=10000
#SBATCH --partition=all
#SBATCH --cpus-per-task=1
#SBATCH --array=1-1000

echo prep lsat ndvi
date

# module load R/3.6.2
module load anaconda3
conda activate rstudio_env

# run application
srun Rscript /home/lb968/code/boreal_biome_shift/2.3_prep_lsat_vi_timeseries.R ${SLURM_ARRAY_TASK_ID} 
