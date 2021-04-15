#!/bin/bash
#SBATCH --job-name=tclim_wy_smry
#SBATCH --output=/scratch/lb968/tclim_wy_smry_%a.log
#SBATCH --chdir=/scratch/lb968/
#SBATCH --time=02:00:00
#SBATCH --mem=10000
#SBATCH --array=1-4

echo prep tclim
date

module load R

# run application
srun Rscript /home/lb968/code/boreal_biome_shift/1.3.5_calc_terraclim_wy_smry.R ${SLURM_ARRAY_TASK_ID} 
