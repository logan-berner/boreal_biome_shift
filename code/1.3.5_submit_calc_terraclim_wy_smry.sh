#!/bin/bash
#SBATCH --job-name=tclim_wy_smry
#SBATCH --output=/scratch/lb968/tclim_wy_smry_%a.log
#SBATCH --chdir=/scratch/lb968/
#SBATCH --time=02:00:00
#SBATCH --mem=30000
#SBATCH --array=1-4

echo tclim_wy_smry
date

module load R/4.0.2

# run application
srun Rscript /projects/arctic/users/lberner/boreal_biome_shift/code/1.3.5_calc_terraclim_wy_smry.R ${SLURM_ARRAY_TASK_ID} 
