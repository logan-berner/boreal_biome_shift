#!/bin/bash
#SBATCH --job-name=smry_lsat_vi_trnd
#SBATCH --output=/scratch/lb968/smry_lsat_vi_trnd.log
#SBATCH --chdir=/scratch/lb968/
#SBATCH --time=01:30:00
#SBATCH --mem=75000

echo smry lsat vi trnds  
date

module load R/4.0.2

# run application
srun Rscript /projects/arctic/users/lberner/boreal_biome_shift/code/2.6_smry_lsat_vi_trends.R
