#!/bin/bash
#SBATCH --job-name=fit_rf
#SBATCH --output=/scratch/lb968/fit_rf_%a.log
#SBATCH --chdir=/scratch/lb968/
#SBATCH --time=2:00:00
#SBATCH --mem=4000
#SBATCH --array=1-1000

echo attrib vi trends
date

module load R/4.0.2
R --version

# run application
srun Rscript /projects/arctic/users/lberner/boreal_biome_shift/code/3.1_attribute_lsat_vi_trends.R ${SLURM_ARRAY_TASK_ID}
