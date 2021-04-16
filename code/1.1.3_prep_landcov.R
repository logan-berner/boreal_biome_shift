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

tmp.dir <- 'C:/tmp/R_tmp'
tempfile(tmpdir=tmp.dir)
rasterOptions(tmpdir=tmp.dir)

wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
laea <- CRS("+proj=laea +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
setwd('C:/Users/Logan/Google Drive/research/nau/nasa_above/boreal_biome_shift/')

# LOAD FILES ===============================================================================
# domain files
land.45n.laea.shp <- readOGR(dsn = 'data/gis_data/land_45n_laea.shp')
extnt <- extent(land.45n.laea.shp)
boreal.tmplt.300m.laea <- raster('data/gis_data/wwf_boreal_biome_laea_300m.tif')

# for 2018 file, convert NC to tiff
lc.2018.nc.file <- 'A:/research/data/landcover/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7/C3S-LC-L4-LCCS-Map-300m-P1Y-2018-v2.1.1.nc'
lc.2018.tif.file <- 'A:/research/data/landcover/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7/C3S-LC-L4-LCCS-Map-300m-P1Y-2018-v2.1.1.tif'
r <- raster(lc.2018.nc.file, varname = 'lccs_class')
writeRaster(r, lc.2018.tif.file)
gdalinfo(datasetname = lc.2018.nc.file)
gdal_translate(src_dataset = lc.2018.nc.file, dst_dataset = lc.2018.tif.file, ot = 'Byte', of = 'GTiff')


# land cover data
lc.infiles <- c('A:/research/data/landcover/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992-v2.0.7.tif',
                'A:/research/data/landcover/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2000-v2.0.7.tif',
                'A:/research/data/landcover/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7/C3S-LC-L4-LCCS-Map-300m-P1Y-2018-v2.1.1.tif')
lc.rcls.df <- read.csv('data/gis_data/landcov/ESACCI-LC-Legend.csv')
lc.yoi <- c(1992, 2000, 2018)
n.yoi <- length(lc.yoi)


# PROJECT ANNUAL LAND COVER MAPS TO LAEA AND CLIP / MASK TO BOREAL BIOME  ==================
# create temporary output directories
mkdirs('C:/tmp/esa_lc/')
mkdirs('C:/tmp/esa_lc/highlat_annual/')
mkdirs('C:/tmp/esa_lc/boreal_annual/')

# temporary files
lc.highlat.files <- paste('C:/tmp/esa_lc/highlat_annual/esa_cci_landcov_', lc.yoi, '_300m_laea.tif', sep='')
lc.boreal.files <- paste('C:/tmp/esa_lc/boreal_annual/esa_cci_landcov_orig_classes_', lc.yoi, '_300m_laea.tif', sep='')
lc.boreal.rcl.files <- paste('C:/tmp/esa_lc/boreal_annual/esa_cci_landcov_consol_classes_', lc.yoi, '_300m_laea.tif', sep='')

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
  
  writeRaster(lc.r, lc.boreal.files[i], datatype = 'INT1U', overwrite = T)
  
  # reclassify to consolidate classes
  lc.rcls.r <- reclassify(lc.r, rcl = lc.rcls.df[,1:2])
  writeRaster(lc.rcls.r, lc.boreal.rcl.files[i], datatype = 'INT1U', overwrite = T)
  
  # clean up
  gc()
  removeTmpFiles()
  unlink(tmp.dir, recursive = T)
}
stopCluster(clust)


# GENERATE LAND COVER MASK ---------------------------------------------------------------------------------------------------
# land cover of interest at stable pixels
lc.mask.r <- raster(lc.boreal.rcl.files[3])
lc.mask.r[lc.mask.r == 10] <- NA
lc.mask.r[lc.mask.r > 0] <- 1
lc.mask.r[lc.mask.r == 0] <- NA
writeRaster(lc.mask.r, 'data/gis_data/landcov/esa_cci_landcov_mask_2018_boreal_300m_laea.tif', datatype = 'INT1U', overwrite=T)

# END SCRIPT ====================================================================
