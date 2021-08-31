#!/bin/bash
#SBATCH --job-name=tclim_wy
#SBATCH --output=/scratch/lb968/tclim_wy_%a.log
#SBATCH --chdir=/scratch/lb968/
#SBATCH --time=00:15:00
#SBATCH --mem=2500
#SBATCH --array=1-4

echo prep tclim
date

module load R

# run application
srun Rscript /projects/arctic/users/lberner/boreal_biome_shift/code/1.3.3_calc_terraclim_wy.R ${SLURM_ARRAY_TASK_ID} 
