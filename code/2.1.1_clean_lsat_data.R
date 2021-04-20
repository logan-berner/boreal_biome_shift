# This R script takes Landsat data downloaded from GEE and then cleans observations and consolidates data into one file.
# Date: 2020-1-06

# SET UP WORK SPACE ===========================================================================================
rm(list=ls())
args <- commandArgs(TRUE)
i = as.numeric(args[1])
require(data.table)
require(R.utils)
require(lsatTS)
setwd('/projects/arctic/users/lberner/boreal_biome_shift/data/lsat_samples/')
mkdirs('cleaned/')

# READ IN AND PROCESS SUBSETS OF LANDSAT DATA, THEN WIRTE OUT TEMPORARY FILES =======================
files <- list.files('raw', full.names = T)
n.files <- length(files)

lsat.dt <- fread(files[i])
lsat.dt <- lsat_general_prep(lsat.dt)
  
# count number of samples
n.obs.all <- nrow(lsat.dt)
  
# QAQC flags
lsat.dt <- lsat_clean_data(lsat.dt, filter.snow = T, filter.water = T)
n.obs.clear <- nrow(lsat.dt)

# write out lsat data subset
outname.data <- gsub('.csv','_cleaned.csv', gsub('raw','cleaned',files[i]))
fwrite(lsat.dt, outname.data)

# write out summary of data subset 
smry.dt <- data.table(file = files[i], n.obs.all = n.obs.all, n.obs.clear = n.obs.clear)
outname.smry <- gsub('.csv','_summary.csv', outname.data)
fwrite(smry.dt, outname.smry)

# END SCRIPT ========================================================================================