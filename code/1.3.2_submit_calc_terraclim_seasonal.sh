#!/bin/bash
#SBATCH --job-name=tclim_seas
#SBATCH --output=/scratch/lb968/tclim_seas_%a.log
#SBATCH --chdir=/scratch/lb968/
#SBATCH --time=00:45:00
#SBATCH --mem=2500
#SBATCH --cpus-per-task=1
#SBATCH --array=1-8

echo prep tclim
date

module load R

# run application
srun Rscript /home/lb968/code/boreal_biome_shift/1.3.2_calc_terraclim_seasonal.R ${SLURM_ARRAY_TASK_ID}
