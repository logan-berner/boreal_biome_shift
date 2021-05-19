#!/bin/bash
#SBATCH --job-name=calc_lsat_vi_trnd
#SBATCH --output=/scratch/lb968/calc_lsat_vi_trnd%a.log
#SBATCH --chdir=/scratch/lb968/
#SBATCH --time=00:20:00
#SBATCH --mem=5000
#SBATCH --array=1-1000

echo calc lsat vi trnd  
date

module load R/4.0.2

# run application
srun Rscript /projects/arctic/users/lberner/boreal_biome_shift/code/2.5_calc_lsat_vi_trends.R ${SLURM_ARRAY_TASK_ID} 
