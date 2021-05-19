# This R script creates a set of simple random samples used to extract Landsat data for the 
# Author: Logan Berner, NAU
# Date: 2021-04-13

#### SET UP ===========================================================================
rm(list=ls())
require(sp)
require(raster)
require(rgdal)
require(geosphere)
require(R.utils)
require(rgee)
require(lsatTS)
require(sf)

wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
laea <- CRS("+proj=laea +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
setwd('C:/Users/Logan/Google Drive/research/nau/nasa_above/boreal_biome_shift/')

# domain
boreal.r <- raster('data/gis_data/wwf_boreal_biome_laea_300m.tif')

# masks
hfi.mask.r <- raster('data/gis_data/disturbance/human_footprint_2009_v3_lte2_mask_boreal_300m_laea.tif')
lc.mask.r <- raster('data/gis_data/landcov/esa_cci_landcov_mask_2018_boreal_300m_laea.tif')
gfc.mask.r <- raster('data/gis_data/disturbance/gfc_forest_stable_2001to2019_boreal_300m_laea.tif')
modis.unburned.r <- raster('data/gis_data/disturbance/modis_unburned_2000_2019_nibble1cell_boreal_laea_300m.tif')
ak.unburned.r <- raster('data/gis_data/disturbance/alaskan_forest_no_fires_1985_2017_boreal_laea_300m.tif')
can.undisturb.r <- raster('data/gis_data/disturbance/canadian_forest_no_fire_or_harv_since_1984_boreal_laea_300m.tif')
rus.undisturb.r <- raster('data/gis_data/disturbance/russian_forest_stand_established_before_1985_nibble1cell_laea_300m.tif')

# ecoreg
ecoreg.r <- raster('data/gis_data/wwf_boreal_ecoregs_laea_300m.tif')
ecounit.r <- raster('data/gis_data/ecological_land_unit_boreal_300m_laea.tif')

# CREATE SAMPLING FRAME ===========================================================================
# mask out potentially disturbed areas  
aoi.r <- boreal.r * hfi.mask.r
aoi.r <- aoi.r * lc.mask.r
aoi.r <- aoi.r * gfc.mask.r
aoi.r <- aoi.r * modis.unburned.r
aoi.r <- aoi.r * ak.unburned.r
aoi.r <- aoi.r * can.undisturb.r
aoi.r <- aoi.r * rus.undisturb.r
aoi.r[aoi.r == 0] <- NA

plot(aoi.r)
writeRaster(aoi.r, 'data/gis_data/boreal_sampling_frame_300m_laea.tif', datatype='INT1U', overwrite = T)
# aoi.r <- raster('data/gis_data/boreal_sampling_frame_300m_laea.tif')

# area of boreal biome and study domain
biome.km2 <- cellStats(boreal.r, sum, na.rm=T) * 300^2 / 1000^2
aoi.km2 <- cellStats(aoi.r, sum, na.rm=T) * 300^2 / 1000^2
biome.km2
aoi.km2
aoi.km2 / biome.km2

# anti boreal sampling domain
anti.aoi.r <- is.na(aoi.r)
anti.aoi.r <- mask(anti.aoi.r, boreal.r)
plot(anti.aoi.r)
writeRaster(anti.aoi.r, 'data/gis_data/boreal_nonsampling_frame_300m_laea.tif', datatype='INT1U', overwrite = T)

# mask eco land units to area of interest for stratifying sampling sites   
ecounit.aoi.r <- ecounit.r * aoi.r
writeRaster(ecounit.aoi.r, 'data/gis_data/ecological_land_unit_boreal_aoi_300m_laea.tif', datatype='INT2U', overwrite = T)
# ecounit.aoi.r <- raster('data/gis_data/ecological_land_unit_boreal_aoi_300m_laea.tif')

# CREATE RANDOM SAMPLE ====================================================================================
# necessary to oversample because NA gridcells 
n.samples <- 100000
pts <- sampleRandom(aoi.r, size = n.samples*5, xy = T, sp = T, na.rm=T)
pts <- pts[sample(1:nrow(pts), n.samples, replace = F),]

pts$site <- paste0('site_', 1:nrow(pts))

# check point locations
plot(aoi.r)
plot(pts, add=T)

# cluster sites based on a 300 km resolution grid
aoi.300km.r <- raster::aggregate(aoi.r, fact = 1000, fun = 'max')
vals <- values(aoi.300km.r)
vals <- vals[is.na(vals)==F]
aoi.300km.r[aoi.300km.r==1] <- 1:length(vals)
plot(aoi.300km.r)

pts$cluster <- paste0('cluster_', raster::extract(aoi.300km.r, pts))
length(unique(pts$cluster))

# EXTRACT LANDSAT DATA USING GEE ====================================================================================
ee_Initialize()
pts.sf <- st_as_sf(pts)
lsat_export_ts(pts.sf, startJulian = 152, endJulian = 243, start_date = '1985-06-01', end_date = '2019-08-31', BUFFER_DIST = 30,
               chunks_from = 'cluster', drive_export_dir = 'lsat_boreal_export', file_prefix = 'lsat_boreal_export')

# END SCRIPT ========================================================================================================