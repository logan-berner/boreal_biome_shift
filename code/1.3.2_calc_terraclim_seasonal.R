# This R script takes monthly Terra Climate data for the boreal domain and computes seasonal composites
# Author: Logan Berner
# Date: 2021-01-15
#---------------------------------------------------------------------------------------------------------------------
rm(list=ls())
.libPaths(c(.libPaths(), "~/R/", '/home/lb968/R/4.0.2'))
require(R.utils)
require(raster)
require(rgdal)

args <- commandArgs(TRUE)
i = as.numeric(args[1])
# i = 2

wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
laea <- CRS("+proj=laea +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
setwd('/projects/arctic/users/lberner/boreal_biome_shift/data/gis_data/terra_clim/')

all.files <- list.files('monthly', full.names = T)
yoi <- 1959:2019
# yoi <- 2017

boreal.biome.laea.shp <- readOGR('../wwf_ecoreg/wwf_boreal_biome_buf5km_laea.shp')

mkdirs('seasonal')


# SET UP TEMPORARY DIRECTORY FOR RASTER ================================================================================
tmp.dir <- paste('/scratch/lb968/Rtmp/',i, '/', sep='')
mkdirs(tmp.dir)
rasterOptions(tmpdir = tmp.dir)


# METADATA ABOUT VARIABLES =============================================================================================
meta.df <- data.frame(var=c('def','pdsi','ppt','soil','swe','tmax','tmin','vpd'),
                      units = c('mm','unitlessx100','mm','mm','mm','Cx100','Cx100','kPax100'),
                      bitdepth = c('INT2U','INT2S','INT2U','INT2U','INT2U','INT2S','INT2S','INT2U'),
                      fun = c('sum','mean','sum','mean','mean','mean','mean','mean'))

var <- as.character(meta.df$var[i])
var.meta.df <- meta.df[which(meta.df$var %in% var),]

# get files for specific variable
var.files <- grep(var, all.files, value = T)


# LOOP THROUGHT MONTHLY FILES AND GENERATE SEASONAL COMPOSITES =======================================================
season.files <- var.files[-c(1:11, length(var.files)-1)]
season.df <- data.frame(year = sort(rep(yoi,4)), 
                        season = c('djf','mam','jja','son'), 
                        file.index = seq(1, length(yoi)*12, 3))

n.seasons <- nrow(season.df)
print('starting loop...')

# loop through the monthly bands in each file
for(j in 1:n.seasons){
  yr <- season.df$year[j]
  season <- season.df$season[j]
  in.files <- season.files[c(season.df$file.index[j]:(season.df$file.index[j]+2))]
  stk <- stack(in.files)
  seas.r <- stackApply(stk, rep(1,3), fun = as.character(var.meta.df$fun))
  seas.r <- mask(seas.r, boreal.biome.laea.shp)
  out.file <- paste('seasonal/terraclim_boreal_', var,'_',yr,'_',season,'_', var.meta.df$fun, '_', var.meta.df$units, '_laea_4km.tif', sep='')
  writeRaster(seas.r, out.file, datatype = as.character(var.meta.df$bitdepth), overwrite = T)
  print(j)
}

# delete tmp directory
unlink(tmp.dir, recursive = T)
print('done')

# END SCRIPT ===========================================================================================================