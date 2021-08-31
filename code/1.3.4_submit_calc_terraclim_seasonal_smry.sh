#!/bin/bash
#SBATCH --job-name=tclim_seas_smry
#SBATCH --output=/scratch/lb968/tclim_seas_smry_%a.log
#SBATCH --chdir=/scratch/lb968/
#SBATCH --time=01:30:00
#SBATCH --mem=30000
#SBATCH --array=1-32

echo tclim clim
date

module load R/4.0.2

# run application
srun Rscript /projects/arctic/users/lberner/boreal_biome_shift/code/1.3.4_calc_terraclim_seas_smry.R ${SLURM_ARRAY_TASK_ID} 
