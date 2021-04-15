#!/bin/bash
#SBATCH --job-name=calc_lsat_ndvi_trnd
#SBATCH --output=/scratch/lb968/calc_lsat_ndvi_trnd%a.log
#SBATCH --chdir=/scratch/lb968/
#SBATCH --time=00:4:00
#SBATCH --mem=750
#SBATCH --partition=all
#SBATCH --cpus-per-task=1
#SBATCH --array=1-1000

echo calc lsat ndvi trnd  
date

module load R

# run application
srun Rscript /home/lb968/code/boreal_biome_shift/2.4_calc_lsat_ndvi_trend_sites.R ${SLURM_ARRAY_TASK_ID} 
