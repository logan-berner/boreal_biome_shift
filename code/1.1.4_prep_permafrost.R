# This R script preps ESA CCI Permafrost layers for boreal greening / browning analysis
# Author: Logan Berner, NAU
# Date: 2021-04-23
.libPaths(c(.libPaths(), "~/R/", '/home/lb968/R/4.0.2'))

rm(list=ls())
require(raster)
require(data.table)
require(rgdal)
require(R.utils)
require(rgeos)
require(gdalUtils)
require(ncdf4)

args <- commandArgs(TRUE)
# i = as.numeric(args[1])
i = 3

tmp.dir <- paste0('/scratch/lb968/R_tmp_pft')
tempfile(tmpdir=tmp.dir)
rasterOptions(tmpdir=tmp.dir)

epsg.laea <-  'EPSG:3571'
epsg.polarstereo <- 'EPSG:3995'

setwd('/projects/arctic/users/lberner/boreal_biome_shift/')
source('/home/lb968/code/arctic_greening/0.2_fun_stack_trend.R')

# options(warn=-1)
options(warn=0)

# LOAD FILES  ============================================================================

# domain files
highlat.shp.file <- '/projects/arctic/users/lberner/arctic_greening/data/gis_data/arctic_zones/45N.shp'
boreal.laea.shp <- readOGR(dsn = 'data/gis_data/wwf_ecoreg/wwf_boreal_biome_buf5km_laea.shp')
extnt.laea <- round(extent(boreal.laea.shp))

# identify permafrost variable for this slurm job
pf.vars <- c('active_layer_thickness','ground_temperature','permafrost_extent')
pf.abb <- c('ALT','GTD','PFR')
pf.units <- c('cm','degC','frac')
pf.yrs <- 1997:2018

# grab files for the specific variable
pf.files <- list.files(paste0('/projects/arctic/share/geol/esa_cci_', pf.vars[i], '/0_orig'), full.names = T)
pf.names <- list.files(paste0('/projects/arctic/share/geol/esa_cci_', pf.vars[i], '/0_orig'), full.names = F)


# CONVERT FORMAT AND CLIP/REPROJECT RASTERS =================================================
mkdirs('data/gis_data/permafrost/')
mkdirs('data/gis_data/permafrost/annual/')

j=1
for (j in 1:length(pf.yrs)){
  # nc --> geotiff
  r <- raster(pf.files[j])
  tmpfile1 <- paste0('/scratch/lb968/',pf.abb[i],'_format_',j,'.tif')
  writeRaster(r, tmpfile1, overwrite=T)

  # project from polar stereo to LAEA
  tmpfile2 <- paste0('/scratch/lb968/',pf.abb[i],'_proj_',j,'.tif')
  gdalwarp(srcfile = tmpfile1, dstfile = tmpfile2, s_srs = epsg.polarstereo, t_srs = epsg.laea, r = 'bilinear', tr = c(1000,1000), te = extnt.laea[c(1,3,2,4)], overwrite=T)
  print('finished projecting')

  # load raster into R
  pf.r <- raster(tmpfile2)
  
  # 
  plot(pf.r)
  plot(boreal.laea.shp, add=T)
  
  # mask to arctic
  pf.r  <- crop(pf.r, boreal.laea.shp)
  pf.r <- mask(pf.r, boreal.laea.shp)
  print('finished masking...')

  # if ground temperature, then rescale (divide 100) and convert deg K to C
  if (pf.abb[i] == 'GTD'){
    pf.r <- (pf.r/100)-273.15  
  }

  # write out
  outname <- paste0('data/gis_data/permafrost/annual/esa_cci_pf_', tolower(pf.abb[i]),'_', pf.units[i],'_',pf.yrs[j],'_laea_1km.tif')
  writeRaster(pf.r, outname, overwrite = T)

  print(j/length(pf.names))
}


# COMPUTE MEAN ANNUAL AND TREND IN PF VARIABLE =============================================
pf.files <- list.files('/projects/arctic/users/lberner/boreal_biome_shift/data/gis_data/permafrost/annual/', full.names = T)
pf.files <- grep(tolower(pf.abb[i]), pf.files, value = T)

pf.stk <- stack(pf.files)

# mean annual pf
pf.avg.r <- mean(pf.stk, na.rm=T)
writeRaster(pf.avg.r, 'data/gis_data/pegeol/esa_cci_pf_pf/esa_cci_pf_pf_degC_avg_2003to2016_1km_laea.tif', overwrite=T)

# pf trend
pf.trnd.stk <- stack.trend(pf.stk)
writeRaster(pf.trnd.stk, '/projects/arctic/users/lberner/arctic_greening/data/gis_data/geol/esa_cci_pf_pf/esa_cci_pf_pf_1m_degC_trend_2003to2016_1km_laea.tif', overwrite=T)


#### CLEAN UP ============================================================================
gc()
removeTmpFiles()
unlink(tmp.dir, recursive = T)
print("All done!!")

#### END SCRIPT  ============================================================================