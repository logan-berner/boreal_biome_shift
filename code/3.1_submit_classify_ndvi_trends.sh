#!/bin/bash
#SBATCH --job-name=fit_rf
#SBATCH --output=/scratch/lb968/fit_rf_%a.log
#SBATCH --chdir=/scratch/lb968/
#SBATCH --time=01:00:00
#SBATCH --mem=1000
#SBATCH --cpus-per-task=1
#SBATCH --array=1-1000

echo fit rf to ndvi trends
date

module load R/3.5.2
R --version

# run application
Rscript /home/lb968/code/boreal_biome_shift/3.1_classify_ndvi_trends.R ${SLURM_ARRAY_TASK_ID}
