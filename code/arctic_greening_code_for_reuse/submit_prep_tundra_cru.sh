#!/bin/bash
#SBATCH --job-name=prep_cru
#SBATCH --output=/scratch/lb968/logs/prep_tudra_clim.txt
#SBATCH --workdir=/scratch/lb968/
#SBATCH --time=3:00:00
#SBATCH --mem=100000
#SBATCH --partition=all
#SBATCH --cpus-per-task=1

echo prep_clim
date
module load R
srun Rscript /home/lb968/code/arctic_greening/prep_tundra_cru.R
