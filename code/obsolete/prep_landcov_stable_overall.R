# This R script preps several geospatial data sets use in analysis of Landsat NDVI trends across the circumboreal domain.
# Author: Logan Berner, NAU
# Date: 2019-07-15
rm(list=ls())
args <- commandArgs(TRUE)
# i = as.numeric(args[1])
i = 1
require(raster)
require(sp)
require(rgdal)
require(R.utils)
require(gdalUtils)
require(rgeos)
tmp.dir <- paste0('/scratch/lb968/R_tmp_',i)
tempfile(tmpdir=tmp.dir)
rasterOptions(tmpdir=tmp.dir)

# SET UP ===============================================================================
setwd('/projects/above_gedi/')

wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
laea <- CRS("+proj=laea +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# load domain files
land.45n.laea.shp <- readOGR(dsn = 'lberner/boreal_ndvi/data/gis_data/land_45n_laea.shp')
extnt <- extent(land.45n.laea.shp)
boreal.tmplt.300m.laea.r <- raster('lberner/boreal_ndvi/data/gis_data/wwf_boreal_biome_laea_300m.tif')

# specify land cover data
lc.infile <- list.files('geodata/landcover/esa_cci_global_300/', full.names = T, pattern = glob2rx('*.tif'))[i]
lc.rcls.df <- read.csv('data/gis_data/esa_landcov/ESACCI-LC-Legend.csv')
lc.classes <- 1:11 # sort(unique(lc.rcls.df$rcls.val)) 
n.lc <- length(lc.classes) # number of land cover types
lc.yrs <- 1992:2015
lc.yoi <- lc.yrs[i]

# create output directories
mkdirs('/scratch/lb968/esa_lc/boreal_stable/')


# IDENTIFY STABLE COVER FOR EACH LAND COVER TYPE ===============================================


# loop through land cover type, identifying stable areas
clust <- makeCluster(4)
registerDoParallel(clust)
foreach(i=1:n.lc) %dopar% {
  require(raster)
  tmp.dir <- paste('C:/tmp/R_',i, sep='')
  tempfile(tmpdir=tmp.dir)
  rasterOptions(tmpdir=tmp.dir)
  
  infiles <- list.files(paste('/scratch/lb968/esa_lc/boreal_binary/', lc.classes[i], sep=''), full.names = T)
  lc.stk <- stack(infiles) 
  lc.stbl.r <- stackApply(lc.stk, rep(1, nlayers(lc.stk)), fun = prod)
  outname <- paste('/scratch/lb968/esa_lc/boreal_stable/', lc.classes[i], '.tif', sep='')
  writeRaster(lc.stbl.r, outname, datatype = 'LOG1S', overwrite = T)
  
  gc()
  removeTmpFiles()
  unlink(tmp.dir, recursive = T)
}
stopCluster(clust)

# GENERATE RASTER OF ALL STABLE LAND COVER TYPES ================================

# stable pixels (all land cover types)
stbl.stk <- stack(list.files('/scratch/lb968/esa_lc/boreal_stable/', full.names = T))
stbl.r <- stackApply(stbl.stk, rep(1, nlayers(stbl.stk)), fun = sum)
stbl.r <- mask(stbl.r, boreal.biome.laea.shp)
plot(stbl.r)
writeRaster(stbl.r, 'data/gis_data/esa_landcov/esa_cci_boreal_landcov_stability_1992to2015_300m_laea.tif', datatype = 'LOG1S', overwrite=T)

# land cover at stable pixels
lc.stbl.r <- raster(lc.files[1]) * stbl.r
plot(lc.stbl.r)
writeRaster(lc.stbl.r, 'data/gis_data/esa_landcov/esa_cci_boreal_stable_landcov_1992to2015_300m_laea.tif', datatype = 'INT1U', overwrite=T)

# land cover of interest at stable pixels
lc.oi.stbl.r <- lc.stbl.r
lc.oi.stbl.r[lc.stbl.r == 11] <- NA
lc.oi.stbl.r[lc.stbl.r == 0] <- NA
writeRaster(lc.oi.stbl.r, 'data/gis_data/esa_landcov/esa_cci_boreal_stable_landcov_oi_1992to2015_300m_laea.tif', datatype = 'INT1U', overwrite=T)
