# This R script creates a set of simple random samples used to extract Landsat data for the 
# Author: Logan Berner, NAU
# Date: 2019-06-17

#### SET UP ===========================================================================
rm(list=ls())
require(sp)
require(raster)
require(rgdal)
require(geosphere)
require(R.utils)
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
laea <- CRS("+proj=laea +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
setwd('C:/Users/Logan/Google Drive/research/nau/nasa_above/boreal_ndvi/')

# domain
boreal.r <- raster('data/gis_data/wwf_boreal_biome_laea_300m.tif')

# masks
wild.r <- raster('data/gis_data/last_wild_boreal_2009_laea_300m.tif')
modis.unburned.r <- raster('data/gis_data/modis_notburned_2000_2017_nibble1cell_boreal_laea_300m.tif')
ak.unburned.r <- raster('data/gis_data/alaskan_forest_no_fires_1985_2017_nibble1cell_boreal_laea_300m.tif')
can.undisturb.r <- raster('data/gis_data/canadian_forest_no_fire_or_harv_since_1984_grow1cell_boreal_laea_300m.tif')
rus.undisturb.r <- raster('data/gis_data/russian_forest_stand_established_before_1985_nibble1cell_laea_300m.tif')
stb.lc.r <- raster('data/gis_data/esa_landcov/esa_cci_boreal_stable_landcov_mask_1992to2015_300m_laea.tif')

# ecoreg
ecoreg.r <- raster('data/gis_data/wwf_boreal_ecoregs_laea_300m.tif')
ecounit.r <- raster('data/gis_data/ecological_land_unit_boreal_300m_laea.tif')

# CREATE SAMPLING FRAME ===========================================================================
# mask out potentially disturbed areas  
aoi.r <- boreal.r * wild.r
aoi.r <- aoi.r * modis.unburned.r
aoi.r <- aoi.r * ak.unburned.r
aoi.r <- aoi.r * can.undisturb.r
aoi.r <- aoi.r * rus.undisturb.r
aoi.r <- aoi.r * stb.lc.r
aoi.r[aoi.r == 0] <- NA

plot(aoi.r)
writeRaster(aoi.r, 'data/gis_data/boreal_sampling_frame_300m_laea.tif', datatype='INT1U', overwrite = T)
# aoi.r <- raster('data/gis_data/boreal_sampling_frame_300m_laea.tif')

# area of boreal biome and study domain
biome.km2 <- cellStats(boreal.r, sum, na.rm=T) * 300^2 / 1000^2
aoi.km2 <- cellStats(aoi.r, sum, na.rm=T) * 300^2 / 1000^2
aoi.km2 / biome.km2

# anti boreal sampling domain
anti.aoi.r <- is.na(aoi.r)
anti.aoi.r <- mask(anti.aoi.r, boreal.r)
plot(anti.aoi.r)
writeRaster(anti.aoi.r, 'data/gis_data/boreal_nonsampling_frame_300m_laea.tif', datatype='INT1U', overwrite = T)

# mask eco land units to area of interest for stratifying sampling sites   
ecounit.aoi.r <- ecounit.r * aoi.r
writeRaster(ecounit.aoi.r, 'data/gis_data/ecological_land_unit_boreal_aoi_300m_laea.tif', datatype='INT2U', overwrite = T)

# CREATE RANDOM SAMPLE ====================================================================================
n.samples <- 50000
pts <- sampleRandom(aoi.r, size = n.samples, xy = T, sp = T)

# check point locations
plot(pts, add=T)

# extract ecoregion number for each site (used to cluser sites when downloading)
pts$eco.num <- raster::extract(ecoreg.r, pts)

# write out shapefile with all samples
dsn.name <- paste('data/gis_data/boreal_sample_sites_n', n.samples,'_laea.shp', sep='')
layer.name <- paste('boreal_sample_sites_n', n.samples,'_laea.shp', sep='')
writeOGR(obj = pts, layer = layer.name, dsn = dsn.name, driver = "ESRI Shapefile", overwrite_layer = T)

# TRANSFER FILES TO EARTH ENGINE ===============================================
# srs.aea <- readOGR(dsn = 'data/gis_data/boreal_sample_sites_n10000_laea.shp')
# srs.aea <- data.frame(pts)

# WRITE OUT A SHAPEFILE FOR EACH CLUSTER INTO TMP DIRECTORY (LATER DELETED)
tmp.cluster.dir <- 'C:/tmp/boreal_samples'
mkdirs(tmp.cluster.dir)
clsts <- sort(unique(pts$eco.num))
for (i in clsts){
  srs.clst <- subset(pts, eco.num == i)
  clst.dsn.name <- paste(tmp.cluster.dir, '/boreal_sample_sites_clst_', i,'_laea.shp', sep='')
  clst.layer.name <- paste('boreal_sample_sites_clst_', i,'_laea.shp', sep='')
  writeOGR(obj = srs.clst, layer = clst.layer.name, dsn = clst.dsn.name, driver = "ESRI Shapefile", overwrite_layer = T)
}

# ALSO WRITE OUT SHAPEFILE WITH ALL SAMPLE SITES TO TMP DIRECTORY
writeOGR(obj = pts, layer = layer.name, dsn = paste('C:/tmp/boreal_samples/', layer.name, sep=''), driver = "ESRI Shapefile", overwrite_layer = T)

# UPLOAD THE SHAPEFILES TO GOOGLE CLOUD STORAGE USING THE GOOGLE SDK TOOLS
upld.sys.call <- paste("C:/Users/Logan/AppData/Local/Google/CloudSDK/google-cloud-sdk/bin/gsutil -m cp C:/tmp/boreal_samples/* gs://above_geodata/boreal_samples/", sep='')
shell(upld.sys.call) # issue system call
unlink(tmp.cluster.dir, recursive = T) # delete files from tmp directory

# CREATE EARTH ENGINE ASSETS USING SHAPEFILES ON GOOGLE CLOUD STORAGE (PASTE THESE INTO THE CMD TERMINAL...) 
earthengine upload table --asset_id users/loganberner/boreal_sample_sites_n25000_laea gs://above_geodata/boreal_samples/boreal_sample_sites_n10000_laea.shp

earthengine upload table --asset_id users/loganberner/boreal_sample_sites_clst_1_laea gs://above_geodata/boreal_samples/boreal_sample_sites_clst_1_laea.shp
earthengine upload table --asset_id users/loganberner/boreal_sample_sites_clst_2_laea gs://above_geodata/boreal_samples/boreal_sample_sites_clst_2_laea.shp
earthengine upload table --asset_id users/loganberner/boreal_sample_sites_clst_3_laea gs://above_geodata/boreal_samples/boreal_sample_sites_clst_3_laea.shp
earthengine upload table --asset_id users/loganberner/boreal_sample_sites_clst_4_laea gs://above_geodata/boreal_samples/boreal_sample_sites_clst_4_laea.shp
earthengine upload table --asset_id users/loganberner/boreal_sample_sites_clst_5_laea gs://above_geodata/boreal_samples/boreal_sample_sites_clst_5_laea.shp
earthengine upload table --asset_id users/loganberner/boreal_sample_sites_clst_6_laea gs://above_geodata/boreal_samples/boreal_sample_sites_clst_6_laea.shp
earthengine upload table --asset_id users/loganberner/boreal_sample_sites_clst_7_laea gs://above_geodata/boreal_samples/boreal_sample_sites_clst_7_laea.shp
earthengine upload table --asset_id users/loganberner/boreal_sample_sites_clst_8_laea gs://above_geodata/boreal_samples/boreal_sample_sites_clst_8_laea.shp
earthengine upload table --asset_id users/loganberner/boreal_sample_sites_clst_9_laea gs://above_geodata/boreal_samples/boreal_sample_sites_clst_9_laea.shp
earthengine upload table --asset_id users/loganberner/boreal_sample_sites_clst_10_laea gs://above_geodata/boreal_samples/boreal_sample_sites_clst_10_laea.shp
earthengine upload table --asset_id users/loganberner/boreal_sample_sites_clst_11_laea gs://above_geodata/boreal_samples/boreal_sample_sites_clst_11_laea.shp
earthengine upload table --asset_id users/loganberner/boreal_sample_sites_clst_12_laea gs://above_geodata/boreal_samples/boreal_sample_sites_clst_12_laea.shp
earthengine upload table --asset_id users/loganberner/boreal_sample_sites_clst_13_laea gs://above_geodata/boreal_samples/boreal_sample_sites_clst_13_laea.shp
earthengine upload table --asset_id users/loganberner/boreal_sample_sites_clst_14_laea gs://above_geodata/boreal_samples/boreal_sample_sites_clst_14_laea.shp
# earthengine upload table --asset_id users/loganberner/boreal_sample_sites_clst_15_laea gs://above_geodata/boreal_samples/boreal_sample_sites_clst_15_laea.shp
earthengine upload table --asset_id users/loganberner/boreal_sample_sites_clst_16_laea gs://above_geodata/boreal_samples/boreal_sample_sites_clst_16_laea.shp
earthengine upload table --asset_id users/loganberner/boreal_sample_sites_clst_17_laea gs://above_geodata/boreal_samples/boreal_sample_sites_clst_17_laea.shp

# create temporary folder into which lsat data will originally be downloaded
mkdirs(paste(tmp.cluster.dir, '/raw', sep=''))

# AFTER THIS, FIRE UP MULTIPLE INSTANCES OF PYTHON AND DOWNLOAD LANDSAT DATA (OTHER GEOSPATIAL DATA) FOR THESE SITSE.
# END SCRIPT ---------------
