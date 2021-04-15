# This R script takes global ESA Climate Change Initiative Land Cover data, clips the boreal biome and reprojects to AEA at 300 m.
# Author: Logan Berner
# Date: 2019-7-17
#---------------------------------------------------------------------------------------------------------------------
rm(list=ls())
require(R.utils)
require(raster)
require(Hmisc)
require(maptools)
setwd('/projects/above_gedi/')

#----------------------------------------------------------------------------------------------------------------------
# GET SLURM JOB ARRAY NUMBER
#----------------------------------------------------------------------------------------------------------------------
# args <- commandArgs(TRUE)
# i = as.numeric(args[1])
i=1

#----------------------------------------------------------------------------------------------------------------------
# IDENTIFY FILES, SET UP METADATA FOR EACH VARIABLE, CREATE MAIN OUTPUT DIR
#----------------------------------------------------------------------------------------------------------------------
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
laea <- CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

aoi.wgs84 <- readShapePoly('lberner/arctic_greening/gis_data/arctic_tundra_biome_wgs84_buf50km', proj4string = wgs84)
aoi.laea <- readShapePoly('lberner/arctic_greening/gis_data/arctic_tundra_biome_laea_buf50km', proj4string = laea)

tmplt.laea.50km <- raster('lberner/arctic_greening/gis_data/arctic_tundra_biome_buf50km_50km_laea.tif')

#----------------------------------------------------------------------------------------------------------------------
# LOOP THROUGHT TIME STEPS, THEN OUTPUT MONTHLY LAYER AS A GEOTIF (SOMETIMES RESCALED)
#----------------------------------------------------------------------------------------------------------------------
var.df <- data.frame(var='tmp',orig.unit ='degC', scalar=10, output.units='degCx10', output.bitdepth='INT2S')

# create output directory and names for each file
outdir <- paste('lberner/arctic_greening/gis_data/cru_4.01/monthly/', var.df$var[i], sep='')
mkdirs(outdir)

outnames <- list.files(paste('geodata/climate/cru_ts4.01/global_wgs84_tif/', as.character(var.df$var[i]), sep=''), full.names = F)
outnames <- paste(outdir, "/", gsub('.tif','_laea_50km.tif', outnames), sep='')    
  
# get files for variable
var.files <- list.files(paste('geodata/climate/cru_ts4.01/global_wgs84_tif/', as.character(var.df$var[i]), sep=''), full.names = T)
n.ts <- length(var.files)
    
# look throught each timestep
for(j in 1:n.ts){
  r <- raster(var.files[j])
  r.aoi <- raster::mask(crop(r, aoi.wgs84), aoi.wgs84)
  r.aoi.laea <- projectRaster(r.aoi, tmplt.laea.50km, method = 'bilinear')
  # r.aoi.laea <- raster::mask(r.aoi.laea, tmplt.laea.50km)
  r.aoi.laea <- round(r.aoi.laea)
  writeRaster(r.aoi.laea, outnames[j], overwrite=T, dataType=as.character(var.df$output.bitdepth[i]))
  print(j/n.ts)
}

#----------------------------------------------------------------------------------------------------------------------
# COMPUTE SUMMER WARMTH INDEX
#----------------------------------------------------------------------------------------------------------------------

# MONTHLY
swi.outdir <- paste('lberner/arctic_greening/gis_data/cru_4.01/monthly/swi', sep='')
mkdirs(swi.outdir)

tmp.files <- list.files(paste('lberner/arctic_greening/gis_data/cru_4.01/monthly/', var.df$var[i], sep=''), full.names = T)
swi.outnames <- gsub('tmp','swi', tmp.files)

for(j in 1:n.ts){
  r <- raster(tmp.files[j])
  r[r<0] <- 0
  writeRaster(r, swi.outnames[j], overwrite=T, dataType='INT2U')
  print(j/n.ts)
}

# ANNUAL
swi.yrly.outdir <- 'lberner/arctic_greening/gis_data/cru_4.01/annual/swi/'
mkdirs(swi.yrly.outdir)

yrs <- 1901:2016
n.yrs <- length(yrs)
swi.monthly.files <- list.files(swi.outdir, full.names = T)
swi.annual.outnames <- paste(swi.yrly.outdir, 'cru_ts4.01_swi_', yrs, '_degCx10_laea_50km2.tif', sep='') 

for (j in 1:n.yrs){
  yr.files <- swi.monthly.files[grep(yrs[j], swi.monthly.files)]
  stk <- stack(yr.files)
  stk.sum <- sum(stk)
  writeRaster(stk.sum, swi.annual.outnames[j], overwrite=T, dataType='INT2U')
  print(j/n.yrs)
}


#----------------------------------------------------------------------------------------------------------------------
# END SCRIPT
#----------------------------------------------------------------------------------------------------------------------
