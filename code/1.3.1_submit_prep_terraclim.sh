#!/bin/bash
#SBATCH --job-name=prep_tclim
#SBATCH --output=/scratch/lb968/prep_tclim_%a.log
#SBATCH --chdir=/scratch/lb968/
#SBATCH --time=00:01:30
#SBATCH --mem=3500
#SBATCH --cpus-per-task=1
#SBATCH --array=1-5952

echo prep tclim
date

module load R/4.0.2 gdal/3.1.4
 
# run application
srun Rscript /home/lb968/code/boreal_biome_shift/1.3.1_prep_terraclim.R ${SLURM_ARRAY_TASK_ID} 
