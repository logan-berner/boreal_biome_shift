# This R script takes monthly Terra Climate data for the boreal domain and computes annual water-year composites
# Author: Logan Berner
# Date: 2021-01-15
#---------------------------------------------------------------------------------------------------------------------
rm(list=ls())
args <- commandArgs(TRUE)
i = as.numeric(args[1])
# i=1

require(R.utils)
require(raster)
require(rgdal)
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
laea <- CRS("+proj=laea +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
setwd('/projects/arctic/users/lberner/boreal_biome_shift/data/gis_data/terra_clim/')

all.files <- list.files('monthly', full.names = T)
yoi <- 1959:2019

tmp.dir <- paste('/scratch/lb968/Rtmp/tclim_wy_',i, '/', sep='')
mkdirs(tmp.dir)
rasterOptions(tmpdir = tmp.dir)

mkdirs('wateryear')


# METADATA ABOUT VARIABLES ==========================================================================================
meta.df <- data.frame(var=c('def','ppt','soil','swe'),
                      units = c('mm','mm','mm','mm'),
                      bitdepth = c('INT2U','INT2U','INT2U','INT2U'),
                      fun = c('sum','sum','mean','max'))

var <- as.character(meta.df$var[i])
var.meta.df <- meta.df[which(meta.df$var %in% var),]

# get files for specific variable
var.files <- grep(var, all.files, value = T)


# LOOP THROUGHT MONTHLY FILES AND GENERATE SEASONAL COMPOSITES =======================================================
wy.files <- var.files[-c(1:8, length(var.files)-2)]
wy.df <- data.frame(year = yoi, file.index = seq(1, length(yoi)*12, 12))

n.wy <- nrow(wy.df)
print('starting loop...')

# loop through the monthly bands in each file
for(j in 1:n.wy){
  yr <- wy.df$year[j]
  in.files <- wy.files[c(wy.df$file.index[j]:(wy.df$file.index[j]+11))]
  stk <- stack(in.files)
  wy.r <- stackApply(stk, rep(1,12), fun = as.character(var.meta.df$fun))
  out.file <- paste('wateryear/terraclim_boreal_', var,'_',yr,'_wy_', var.meta.df$fun, '_', var.meta.df$units, '_laea_4km.tif', sep='')
  writeRaster(wy.r, out.file, datatype = as.character(var.meta.df$bitdepth), overwrite = T)
  print(j)
}

# delete tmp directory
unlink(tmp.dir, recursive = T)
print('done')

# END SCRIPT ===========================================================================================================