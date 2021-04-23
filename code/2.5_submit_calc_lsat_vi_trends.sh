#!/bin/bash
#SBATCH --job-name=calc_lsat_ndvi_trnd
#SBATCH --output=/scratch/lb968/calc_lsat_ndvi_trnd%a.log
#SBATCH --chdir=/scratch/lb968/
#SBATCH --time=00:4:00
#SBATCH --mem=25000
#SBATCH --partition=all
#SBATCH --cpus-per-task=1
#SBATCH --array=1-100

echo calc lsat vi trnd  
date

module load R/4.0.2

# run application
srun Rscript /projects/arctic/users/lberner/boreal_biome_shift/code/2.5_calc_lsat_vi_trends.R ${SLURM_ARRAY_TASK_ID} 
