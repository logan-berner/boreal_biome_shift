# This R script preps several geospatial data sets use in analysis of Landsat NDVI trends across the circumboreal domain.
# Author: Logan Berner, NAU
# Date: 2019-07-15
rm(list=ls())
require(raster)
require(data.table)
require(sp)
require(rgdal)
require(R.utils)
require(gdalUtilities)
require(rgeos)
require(foreach)
require(doParallel)

tmp.dir <- 'C:/tmp/R_tmp'
tempfile(tmpdir=tmp.dir)
rasterOptions(tmpdir=tmp.dir)

wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
laea <- CRS("+proj=laea +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
aea.rus <- '+proj=aea +lat_1=50 +lat_2=70 +lat_0=56 +lon_0=100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
# setwd('C:/Users/lb968/Google Drive/research/nau/nasa_above/boreal_ndvi/')
setwd('C:/Users/Logan/Google Drive/research/nau/nasa_above/boreal_ndvi/')

# LOAD FILES ===============================================================================
# domain files
land.45n.laea.shp <- readOGR(dsn = 'data/gis_data/land_45n_laea.shp')
extnt <- extent(land.45n.laea.shp)
boreal.tmplt.300m.laea <- raster('data/gis_data/wwf_boreal_biome_laea_300m.tif')

# land cover data
lc.infiles <- list.files('A:/research/data/landcover/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7/', full.names = T, pattern = glob2rx('*.tif'))
lc.rcls.df <- read.csv('data/gis_data/esa_landcov/ESACCI-LC-Legend.csv')
lc.classes <- 1:10 # sort(unique(lc.rcls.df$rcls.val)) 
n.lc <- length(lc.classes) # number of land cover types
lc.yoi <- 1992:2015
n.yoi <- length(lc.yoi)

# PROJECT ANNUAL LAND COVER MAPS TO LAEA AND CLIP / MASK TO BOREAL BIOME  ==================
# create temporary output directories
mkdirs('C:/tmp/esa_lc/')
mkdirs('C:/tmp/esa_lc/highlat_annual/')
mkdirs('C:/tmp/esa_lc/boreal_annual/')

# temporary files
lc.highlat.files <- paste('C:/tmp/esa_lc/highlat_annual/esa_cci_landcov_', lc.yoi, '_300m_laea.tif', sep='')
lc.boreal.files <- paste('C:/tmp/esa_lc/boreal_annual/esa_cci_landcov_', lc.yoi, '_300m_laea.tif', sep='')

n.files <- length(lc.infiles)

clust <- makeCluster(3)
registerDoParallel(clust)
foreach(i=1:n.files) %dopar% {
  require(raster)
  require(gdalUtilities)  
  tmp.dir <- paste('C:/tmp/R_',i, sep='')
  tempfile(tmpdir=tmp.dir)
  rasterOptions(tmpdir=tmp.dir)
  
  gdalwarp(srcfile = lc.infiles[i], dstfile = lc.highlat.files[i], s_srs = 'EPSG:4326', t_srs = 'EPSG:3571', r = 'near', 
           tr = c(300,300), te = extnt[c(1,3,2,4)], ot = 'Byte')
  lc.r <- raster(lc.highlat.files[i])
  lc.r <- mask(lc.r, boreal.tmplt.300m.laea)
  lc.r <- reclassify(lc.r, rcl = lc.rcls.df[,1:2])
  writeRaster(lc.r, lc.boreal.files[i], datatype = 'INT1U', overwrite = T)
  
  # clean up
  gc()
  removeTmpFiles()
  unlink(tmp.dir, recursive = T)
}
stopCluster(clust)


# GENERATE ANNUAL BINARY LAND COVER MAPS  ========================================================

# create output directories
mkdirs('C:/tmp/esa_lc/boreal_binary/')
for (i in 1:n.lc){
  mkdirs(paste('C:/tmp/esa_lc/boreal_binary/', lc.classes[i], sep=''))
}

# loop through years and land cover classes
  for (i in 1:n.yoi){
    lc.r <- raster(lc.boreal.files[i])
    clust <- makeCluster(4)
    registerDoParallel(clust)
    foreach(j=1:n.lc) %dopar% {
      require(raster)
      tmp.dir <- paste('C:/tmp/R_',i, sep='')
      tempfile(tmpdir=tmp.dir)
      rasterOptions(tmpdir=tmp.dir)
      
      lc.type <- lc.r == lc.classes[j]
      outname <- paste('C:/tmp/esa_lc/boreal_binary/', lc.classes[j], '/', lc.yoi[i], '.tif', sep='')
      writeRaster(lc.type, outname, datatype = 'INT1U', overwrite = T)
      
      gc()
      removeTmpFiles()
      unlink(tmp.dir, recursive = T)
    }
    stopCluster(clust)
    print(i/n.yoi)
  }

# IDENTIFY STABLE COVER FOR EACH LAND COVER TYPE ===============================================

# create outpud directories
mkdirs('C:/tmp/esa_lc/boreal_stable/')

# loop through land cover type, identifying stable areas
clust <- makeCluster(2)
registerDoParallel(clust)
foreach(i=1:n.lc) %dopar% {
  require(raster)
  tmp.dir <- paste('C:/tmp/R_',i, sep='')
  tempfile(tmpdir=tmp.dir)
  rasterOptions(tmpdir=tmp.dir)
  
  infiles <- list.files(paste('C:/tmp/esa_lc/boreal_binary/', lc.classes[i], sep=''), full.names = T)
  lc.stk <- stack(infiles) 
  lc.stbl.r <- stackApply(lc.stk, rep(1, nlayers(lc.stk)), fun = prod)
  outname <- paste('C:/tmp/esa_lc/boreal_stable/', lc.classes[i], '.tif', sep='')
  writeRaster(lc.stbl.r, outname, datatype = 'LOG1S', overwrite = T)
  
  gc()
  removeTmpFiles()
  unlink(tmp.dir, recursive = T)
}
stopCluster(clust)

# GENERATE RASTER OF ALL STABLE LAND COVER TYPES ================================

# stable pixels (all land cover types)
stbl.stk <- stack(list.files('C:/tmp/esa_lc/boreal_stable/', full.names = T))
stbl.r <- stackApply(stbl.stk, rep(1, nlayers(stbl.stk)), fun = sum)
stbl.r <- mask(stbl.r, boreal.tmplt.300m.laea)
plot(stbl.r)
writeRaster(stbl.r, 'data/gis_data/esa_landcov/esa_cci_boreal_landcov_stability_1992to2015_300m_laea.tif', datatype = 'LOG1S', overwrite=T)

# land cover at stable pixels
lc.stbl.r <- raster('C:/tmp/esa_lc/boreal_annual/esa_cci_landcov_1992_300m_laea.tif') * stbl.r
plot(lc.stbl.r)
writeRaster(lc.stbl.r, 'data/gis_data/esa_landcov/esa_cci_boreal_stable_landcov_1992to2015_300m_laea.tif', datatype = 'INT1U', overwrite=T)

# land cover of interest at stable pixels
lc.oi.stbl.r <- lc.stbl.r
lc.oi.stbl.r[lc.stbl.r == 10] <- NA
lc.oi.stbl.r[lc.stbl.r == 0] <- NA
writeRaster(lc.oi.stbl.r, 'data/gis_data/esa_landcov/esa_cci_boreal_stable_landcov_oi_1992to2015_300m_laea.tif', datatype = 'INT1U', overwrite=T)

# END SCRIPT ====================================================================
