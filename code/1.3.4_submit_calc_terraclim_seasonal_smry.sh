#!/bin/bash
#SBATCH --job-name=tclim_seas_smry
#SBATCH --output=/scratch/lb968/tclim_seas_smry_%a.log
#SBATCH --chdir=/scratch/lb968/
#SBATCH --time=02:00:00
#SBATCH --mem=10000
#SBATCH --array=1-32

echo prep tclim
date

module load R

# run application
srun Rscript /home/lb968/code/boreal_biome_shift/1.3.4_calc_terraclim_seas_smry.R ${SLURM_ARRAY_TASK_ID} 
