#!/bin/bash
#SBATCH --job-name=calcAGB
#SBATCH --output=/scratch/lb968/logs/calcAGB_%a.out
#SBATCH --workdir=/scratch/lb968/
#SBATCH --time=2:00:00
#SBATCH --mem=325000
#SBATCH --partition=all
#SBATCH --cpus-per-task=1
#SBATCH --array=1-1000

echo calcAGB
date

module load R

srun Rscript /home/lb968/code/nslope_biomass/calc_nslope_agb_mc.R ${SLURM_ARRAY_TASK_ID} 
