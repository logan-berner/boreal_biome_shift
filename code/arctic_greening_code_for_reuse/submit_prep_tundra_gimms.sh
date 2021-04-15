#!/bin/bash
#SBATCH --job-name=prepGIMMS3g
#SBATCH --output=/scratch/lb968/logs/prep_gimms3g_%a.log
#SBATCH --workdir=/scratch/lb968/
#SBATCH --time=00:05:00
#SBATCH --mem=1000
#SBATCH --partition=all
#SBATCH --cpus-per-task=1
#SBATCH --array=1-816

echo prep gimms3g
date

# run application
module load R
srun Rscript /home/lb968/code/arctic_greening/prep_tundra_gimms3g.R ${SLURM_ARRAY_TASK_ID} 
