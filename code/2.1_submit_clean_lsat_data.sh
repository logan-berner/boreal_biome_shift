#!/bin/bash
#SBATCH --job-name=tclim_wy
#SBATCH --output=/scratch/lb968/clean_lsat%a.log
#SBATCH --chdir=/scratch/lb968/
#SBATCH --time=00:10:00
#SBATCH --mem=5000
#SBATCH --partition=all
#SBATCH --cpus-per-task=1
#SBATCH --array=1-32

echo clean lsat data
date

module load R

# run application
srun Rscript /home/lb968/code/boreal_biome_shift/2.1_clean_lsat_data.R ${SLURM_ARRAY_TASK_ID} 
