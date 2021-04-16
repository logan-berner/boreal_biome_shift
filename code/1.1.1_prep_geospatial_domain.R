# This R script takes the WWWF ecoregion data set and generates shapefiles with northern land masses, the boreal biome, and boreal ecoregions.
# These layers are used to define the overarching study domain
# Author: Logan Berner, NAU
# Date: 2019-11-13
rm(list=ls())
require(raster)
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
# setwd('C:/Users/lb968/Google Drive/research/nau/nasa_above/boreal_ndvi/')
setwd('C:/Users/Logan/Google Drive/research/nau/nasa_above/boreal_biome_shift/')

# WWF ecoregions
ecoreg.shp <- readOGR(dsn = 'A:/research/data/landcover/wwf_terrestrial_ecoreg_world/official/wwf_terr_ecos.shp')
 
# HIGH LATITUDE LAND MASS (>35 N and >45 N) =============================================================================
ecoreg.45n.shp <- crop(ecoreg.shp, extent(-180,180,45,90))
ecoreg.45n.shp$dslv <- 1

land.45n.shp <- aggregate(ecoreg.45n.shp, by ='dslv', dssolve = T)
land.45n.laea.shp <- spTransform(land.45n.shp, laea)

writeOGR(obj = land.45n.shp, dsn = 'data/gis_data/land_45n_wgs84.shp', 
         layer = "land_45n_wgs84", driver = "ESRI Shapefile", overwrite_layer = T)

writeOGR(obj = land.45n.laea.shp, dsn = 'data/gis_data/land_45n_laea.shp', 
         layer = "land_45n_laea.shp", driver = "ESRI Shapefile", overwrite_layer = T)


ecoreg.35n.shp <- crop(ecoreg.shp, extent(-180,180,35,90))
ecoreg.35n.shp$dslv <- 1

land.35n.shp <- aggregate(ecoreg.35n.shp, by ='dslv', dssolve = T)
land.35n.laea.shp <- spTransform(land.35n.shp, laea)

writeOGR(obj = land.35n.shp, dsn = 'data/gis_data/land_35n_wgs84.shp', 
         layer = "land_45n_wgs84", driver = "ESRI Shapefile", overwrite_layer = T)

writeOGR(obj = land.35n.laea.shp, dsn = 'data/gis_data/land_35n_laea.shp', 
         layer = "land_45n_laea.shp", driver = "ESRI Shapefile", overwrite_layer = T)

# BOREAL BIOME EXTENT AND ECOREGIONS PER WWF DATA SET (OUTPUT BOTH WGS84 AND LAEA) ============================

# select boreal biome / ecoregions, project, and write out -----------------
boreal.ecoreg.shp <- subset(ecoreg.45n.shp, BIOME == 6)
boreal.ecoreg.laea.shp <- spTransform(boreal.ecoreg.shp, laea)

boreal.biome.shp <- aggregate(boreal.ecoreg.shp, by = 'BIOME', dissolve = T)
boreal.biome.laea.shp <- spTransform(boreal.biome.shp, laea)
boreal.biome.laea.shp$BIOME <- 1

writeOGR(obj = boreal.ecoreg.shp, dsn = 'data/gis_data/wwf_boreal_ecoregs_wgs84.shp', 
         layer = "wwf_boreal_ecoregs_wgs84", driver = "ESRI Shapefile", overwrite_layer = T)

writeOGR(obj = boreal.ecoreg.laea.shp, dsn = 'data/gis_data/wwf_boreal_ecoregs_laea.shp', 
         layer = "wwf_boreal_ecoregs_laea", driver = "ESRI Shapefile", overwrite_layer = T)

writeOGR(obj = boreal.biome.shp, dsn = 'data/gis_data/wwf_boreal_biome_wgs84.shp', 
         layer = "wwf_boreal_biome_wgs84", driver = "ESRI Shapefile", overwrite_layer = T)

writeOGR(obj = boreal.biome.laea.shp, dsn = 'data/gis_data/wwf_boreal_biome_laea.shp', 
         layer = "wwf_boreal_biome_laea", driver = "ESRI Shapefile", overwrite_layer = T)

# rasterize boreal biome / ecoreg at 300 m and 5 km ------------------------
extnt <- extent(land.45n.laea.shp)
biome.gdalout.file <- 'data/gis_data/wwf_boreal_biome_laea_300m.tif'
gdal_rasterize(src_datasource = 'data/gis_data/wwf_boreal_biome_laea.shp', dst_filename = biome.gdalout.file, 
               burn = 1, tr = c(300,300), te = extnt[c(1,3,2,4)], ot = 'Byte', a_nodata = 0)

biome.gdalout.file <- 'data/gis_data/wwf_boreal_biome_laea_5km.tif'
gdal_rasterize(src_datasource = 'data/gis_data/wwf_boreal_biome_laea.shp', dst_filename = biome.gdalout.file, 
               burn = 1, tr = c(5000,5000), te = extnt[c(1,3,2,4)], ot = 'Byte', a_nodata = 0)


ecoreg.gdalout.file <- 'data/gis_data/wwf_boreal_ecoregs_laea_300m.tif'
gdal_rasterize(src_datasource = 'data/gis_data/wwf_boreal_ecoregs_laea.shp', dst_filename = ecoreg.gdalout.file, 
               a = 'ECO_NUM', tr = c(300,300), te = extnt[c(1,3,2,4)], ot = 'Byte', a_nodata = 0)

ecoreg.gdalout.file <- 'data/gis_data/wwf_boreal_ecoregs_laea_5km.tif'
gdal_rasterize(src_datasource = 'data/gis_data/wwf_boreal_ecoregs_laea.shp', dst_filename = ecoreg.gdalout.file, 
               a = 'ECO_NUM', tr = c(5000,5000), te = extnt[c(1,3,2,4)], ot = 'Byte', a_nodata = 0)


# blank (all NA) domain raster
boreal.r <- raster('data/gis_data/wwf_boreal_biome_laea_300m.tif')
boreal.r[boreal.r >= 0] <- NA
writeRaster(boreal.r, 'data/gis_data/boreal_domain_grid_allna_laea_300m.tif')

r <- raster(ecoreg.gdalout.file)
plot(r)
