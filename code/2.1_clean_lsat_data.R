# This R script takes Landsat data downloaded from GEE and then cleans observations and consolidates data into one file.
# Date: 2020-1-06

# SET UP WORK SPACE ===========================================================================================
rm(list=ls())
args <- commandArgs(TRUE)
i = as.numeric(args[1])
require(data.table)
require(R.utils)
setwd('/projects/above_gedi/lberner/boreal_ndvi/data/lsat_samples/')
source('/home/lb968/code/boreal_ndvi/fun_lsat_tools.R')
mkdirs('cleaned/')


# READ IN AND PROCESS SUBSETS OF LANDSAT DATA, THEN WIRTE OUT TEMPORARY FILES =======================
files <- list.files('raw', full.names = T)
n.files <- length(files)

# build output table in which to store summary stats of samples
sample.smry.df <- data.frame(n.obs.all=NA, n.obs.clear=NA, n.obs.clear.ngb=NA)

# get details about each file
col.names <- colnames(fread(files[1], nrows=0))
n.rows <- nrow(fread(files[i], select = 1L, fill=T))
rows.per.grab <- 10^5 
row.seq <- seq(0, n.rows, rows.per.grab)
n.row.seq <- length(row.seq)
  
for (j in 1:n.row.seq){
  lsat.samples <- fread(files[i],nrows=rows.per.grab, skip=row.seq[j], fill=T)
  colnames(lsat.samples) <- col.names
  lsat.samples <- lsat_general_prep(lsat.samples)
  
  # drop 2018/2019 
  lsat.samples <- lsat.samples[year <= 2017]
  
  # count number of samples
  n.obs.all <- nrow(lsat.samples)
    
  # QAQC flags
  lsat.samples <- lsat_qaqc_flags(lsat.samples, filter.snow = T, filter.water = T)
  n.obs.clear <- nrow(lsat.samples)
    
  # make sure coordinates are numeric (isn't the case in all files)
  lsat.samples$longitude <- as.numeric(lsat.samples$longitude)
  lsat.samples$latitude <- as.numeric(lsat.samples$latitude)
    
  # neighborhood average
  lsat.samples <- lsat_ngb_mean(lsat.samples)
  n.obs.clear.ngb <- nrow(lsat.samples)
    
  # log grab info to data frame
  sample.smry.df <- rbind(sample.smry.df, data.frame(n.obs.all=n.obs.all, n.obs.clear=n.obs.clear, n.obs.clear.ngb=n.obs.clear.ngb))
    
  if (j == 1){
    lsat.data <- lsat.samples
  } else {
    lsat.data <- rbind(lsat.data, lsat.samples)
  }
  
  # progress
  print(j/n.row.seq)

}

# relabel sites to be unique among extracts
rcls <- data.table(old = sort(unique(lsat.data$site)))
rcls$new <- paste(i,1:nrow(rcls), sep='_')
lsat.data$site <- rcls$new[match(lsat.data$site, rcls$old)]

# write out lsat data subset
outname.data <- paste('cleaned/subset_',i,'_data.csv', sep='')
fwrite(lsat.data, outname.data)

# write out summary of data subset 
sample.smry.df <- na.omit(sample.smry.df)
sample.smry.df$n.sites.clear <- nrow(rcls)
sample.smry.df$subset <- i
outname.smry <- paste('cleaned/subset_',i,'_summary.csv', sep='')
fwrite(sample.smry.df, outname.smry)

# end script 
