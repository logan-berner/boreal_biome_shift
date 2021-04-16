# This R script preps several geospatial data sets use in analysis of Landsat NDVI trends across the circumboreal domain.
# Author: Logan Berner, NAU
# Date: 2020-12-18
rm(list=ls())
require(raster)
require(sf)
require(fasterize)
require(data.table)
require(sp)
require(rgdal)
require(R.utils)
require(gdalUtilities)
require(tmap)
require(rgeos)

tmp.dir <- 'C:/tmp/R_tmp'
tempfile(tmpdir=tmp.dir)
rasterOptions(tmpdir=tmp.dir)

wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
laea <- CRS("+proj=laea +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
aea.rus <- '+proj=aea +lat_1=50 +lat_2=70 +lat_0=56 +lon_0=100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
setwd('C:/Users/Logan/Google Drive/research/nau/nasa_above/boreal_biome_shift/')

##### LOAD FILES ------------------------------------------------------------------------------
# domain files
land.45n.laea.shp <- readOGR(dsn = 'data/gis_data/land_45n_laea.shp')
boreal.biome.laea.shp <- readOGR(dsn = 'data/gis_data/wwf_boreal_biome_laea.shp')
boreal.tmplt.300m.laea <- raster('data/gis_data/wwf_boreal_biome_laea_300m.tif')
extnt <- extent(land.45n.laea.shp)

# disturbance data sets
ak.fire.file <- 'data/gis_data/disturbance/ak_fires_1985_2017_wgs84_ca300m.tif'
canlad.file <- 'A:/research/data/fires/canadian_landsat_disturbance/CanLaD_20151984_latest_type.tif'
hfi.infile <- 'A:/research/data/landcover/human_footprint/wildareas-v3-2009-human-footprint-geotiff/wildareas-v3-2009-human-footprint.tif'

gfc.r <- raster('C:/tmp/gfc_v1-7_boreal_forest_loss_2001to2019_max_300m_laea.tif') # exported from GEE, mosaiced and resampled with ArcGIS
modis.fire.r <- raster('C:/Users/Logan/Google Drive/earth_engine/modis_v6_boreal_burned_area_2000to2019_500m_laea.tif') # exported from GEE

rus.yng.for.r <- raster('A:/research/data/forest_age/Young_Russian_Forest_Map_1330/data/Russia_Stand_Age.tif')

# ecological land units
elu.file <- 'A:/research/data/landcover/World_ELU_2015/globalelu/World_ELU_2015.tif'

# MODIS tree cover 
modis.treecov.r <- raster('C:/Users/Logan/Google Drive/earth_engine/modis_v6_boreal_treecov_median_2017to2019_250m_laea.tif') # exported from GEE


#### ECOLOGICAL LAND UNITS ------------------------------------------------------------------------------
# project to laea, clip / mask to boreal biome
elu.gdalout.file <- 'C:/tmp/elu_laea_300m.tif'
gdalwarp(srcfile = elu.file, dstfile = elu.gdalout.file, t_srs = 'EPSG:3571', r = 'mode', tr = c(300,300), te = extnt[c(1,3,2,4)])

elu.r <- raster(elu.gdalout.file)
elu.r <- mask(elu.r, boreal.tmplt.300m.laea)

uniq.vals <- unique(values(elu.r))
length(uniq.vals)

writeRaster(elu.r, 'data/gis_data/ecological_land_unit_boreal_300m_laea.tif', datatype = 'INT2U', overwrite = T)


#### MODIS TREE COVER ============================================================================
modis.treecov.r <- crop(modis.treecov.r, boreal.biome.laea.shp)
modis.treecov.r <- raster::resample(modis.treecov.r, boreal.tmplt.300m.laea, method = 'bilinear')
modis.treecov.r <- mask(modis.treecov.r, boreal.tmplt.300m.laea)
writeRaster(modis.treecov.r, 'data/gis_data/landcov/modis_treecov_median_2017to2019_boreal_laea_300m.tif', datatype = 'INT1U', overwrite = T)


#### MODIS BURNED AREA ============================================================================
modis.fire.r <- crop(modis.fire.r, boreal.biome.laea.shp)
modis.fire.r <- raster::resample(modis.fire.r, boreal.tmplt.300m.laea, method = 'ngb')
modis.fire.r <- mask(modis.fire.r, boreal.tmplt.300m.laea)
writeRaster(modis.fire.r, 'data/gis_data/disturbance/modis_burned_2000_2019_boreal_laea_300m.tif', datatype = 'INT1U', overwrite = T)

modis.nofire.r <- modis.fire.r == 0
modis.nofire.r <- mask(modis.nofire.r, boreal.tmplt.300m.laea)
writeRaster(modis.nofire.r, 'data/gis_data/disturbance/modis_unburned_2000_2019_boreal_laea_300m.tif', datatype = 'INT1U', overwrite = T)

# expand burned area cells by one cell in every direction  
val.cells <- raster::Which(modis.fire.r == 1, cells = T)
adj.cells <- adjacent(modis.fire.r, cells=val.cells, directions=8, pairs=F)
modis.fire.r[adj.cells] <- 1
writeRaster(modis.fire.r, 'data/gis_data/disturbance/modis_burned_2000_2019_grow1cell_boreal_laea_300m.tif', datatype = 'INT1U', overwrite = T)

modis.nofire.r <- modis.fire.r == 0
modis.nofire.r <- mask(modis.nofire.r, boreal.tmplt.300m.laea)
writeRaster(modis.nofire.r, 'data/gis_data/disturbance/modis_unburned_2000_2019_nibble1cell_boreal_laea_300m.tif', datatype = 'INT1U', overwrite = T)


#### HUMAN FOOTPRINT INDEX AT 300 M ============================================================================
hfi.highlat.file <- 'C:/tmp/R_tmp/hfi_2009_highlat.tif'
gdalwarp(srcfile = hfi.infile, dstfile = hfi.highlat.file, s_srs = 'EPSG:54009', t_srs = 'EPSG:3571', 
         r = 'bilinear', tr = c(300,300), te = extnt[c(1,3,2,4)], srcnodata = 128)
hfi.r <- raster(hfi.highlat.file)
hfi.r <- crop(hfi.r, boreal.tmplt.300m.laea)
extent(hfi.r) <- alignExtent(hfi.r, boreal.tmplt.300m.laea)
hfi.r <- mask(hfi.r, boreal.tmplt.300m.laea)
hfi.r <- round(hfi.r)
writeRaster(hfi.r, 'data/gis_data/disturbance/human_footprint_2009_v3_boreal_300m_laea.tif', datatype = 'INT1U', overwrite = T)

# identify areas with low human pressure for sample domain mask 
hfi.mask.r <- hfi.r <= 2
writeRaster(hfi.mask.r, 'data/gis_data/disturbance/human_footprint_2009_v3_lte2_mask_boreal_300m_laea.tif', datatype = 'INT1U', overwrite = T)


#### GLOBAL FOREST CHANGE (2001 - 2019) ============================================================================
gfc.r <- crop(gfc.r, boreal.tmplt.300m.laea)
gfc.r <- raster::resample(gfc.r, boreal.tmplt.300m.laea, method = 'ngb')
gfc.r <- mask(gfc.r, boreal.tmplt.300m.laea)
writeRaster(gfc.r, 'data/gis_data/disturbance/gfc_forest_loss_2001to2019_boreal_300m_laea.tif', datatype = 'INT1U', overwrite = T)

# identify areas with low human pressure for sample domain mask 
gfc.mask.r <- gfc.r == 0
gfc.mask.r <- mask(gfc.mask.r, boreal.tmplt.300m.laea)
writeRaster(gfc.mask.r, 'data/gis_data/disturbance/gfc_forest_stable_2001to2019_boreal_300m_laea.tif', datatype = 'INT1U', overwrite = T)


#### ALASKA LARGE FIRES ============================================================================
ak.fire.shp <- st_read('A:/research/data/fires/alaskan_large_fire_poly/alaska_fire_polygons_1940_2019.shp')
ak.fire.shp <- subset(ak.fire.shp, FIREYEAR >= 1984)
ak.fire.shp <- st_transform(ak.fire.shp, laea)
ak.fire.shp$BURN <- 1

ak.fire.r <- fasterize(ak.fire.shp, boreal.tmplt.300m.laea, field = 'BURN')
ak.fire.r <- mask(ak.fire.r, boreal.tmplt.300m.laea)
writeRaster(ak.fire.r, 'data/gis_data/disturbance/alaskan_forest_fires_1985_2017_boreal_laea_300m.tif', datatype = 'INT1U', overwrite = T)

ak.nofire.r <- is.na(ak.fire.r)
ak.nofire.r <- mask(ak.nofire.r, boreal.tmplt.300m.laea)
writeRaster(ak.nofire.r, 'data/gis_data/disturbance/alaskan_forest_no_fires_1985_2017_boreal_laea_300m.tif', datatype = 'INT1U', overwrite = T)

# # expand burned area cells by one cell in every direction  
# ak.fire.r <- raster(ak.fire.gdalout.file)
# val.cells <- raster::Which(ak.fire.r == 1, cells = T)
# adj.cells <- adjacent(ak.fire.r, cells=val.cells, directions=8, pairs=F)
# ak.fire.r[adj.cells] <- 1
# ak.fire.r <- mask(ak.fire.r, boreal.tmplt.300m.laea)
# writeRaster(ak.fire.r, 'data/gis_data/alaskan_forest_fires_1985_2017_grow1cell_boreal_laea_300m.tif', datatype = 'INT1U', overwrite = T)
# 
# ak.nofire.r <- is.na(ak.fire.r)
# ak.nofire.r <- mask(ak.nofire.r, boreal.tmplt.300m.laea)
# writeRaster(ak.nofire.r, 'data/gis_data/alaskan_forest_no_fires_1985_2017_nibble1cell_boreal_laea_300m.tif', datatype = 'INT1U', overwrite = T)


#### CANADIAN LANDSAT DISTURBANCE (FIRE AND HARV) ============================================================================
# project to laea, clip / mask to boreal biome
canlad.gdalout.file <- 'C:/tmp/canlad_laea_300m.tif'
canlad.proj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

gdalwarp(srcfile = canlad.file, dstfile = canlad.gdalout.file, 
         s_srs = canlad.proj, t_srs = 'EPSG:3571', r = 'max', tr = c(300,300), te = extnt[c(1,3,2,4)])

canlad.r <- raster(canlad.gdalout.file)
canlad.r[canlad.r <= 2] <- 1 # set disturbance to eql 1
canlad.r <- mask(canlad.r, boreal.tmplt.300m.laea)
writeRaster(canlad.r, 'data/gis_data/disturbance/canadian_forest_fire_or_harv_since_1984_boreal_laea_300m.tif', datatype = 'INT1U', overwrite = T)

# no fire or disturb
canlad.nodisturb.r <- is.na(canlad.r)
canlad.nodisturb.r <- mask(canlad.nodisturb.r, boreal.tmplt.300m.laea)
writeRaster(canlad.nodisturb.r, 'data/gis_data/disturbance/canadian_forest_no_fire_or_harv_since_1984_boreal_laea_300m.tif', datatype = 'INT1U', overwrite = T)


#### RUSSIAN YOUNG FORESTS ============================================================================
# expand 'young forest' cells by one cell in every direction  
rus.yng.for.r[rus.yng.for.r <= 27] <- 1 # young = 1
rus.yng.for.r[rus.yng.for.r > 27] <- NA # all else == NA
val.cells <- raster::Which(rus.yng.for.r == 1, cells = T)
adj.cells <- adjacent(rus.yng.for.r, cells=val.cells, directions=8, pairs=F)
rus.yng.for.r[adj.cells] <- 1
writeRaster(rus.yng.for.r, 'C:/tmp/rus_yng_for_raea_460m.tif', datatype='INT1U', overwrite=T)

# project to laea, clip / mask to boreal biome
rus.yng.for.outfile <- 'data/gis_data/russian_forest_stand_established_since_1985_grow1cell_laea_300m.tif'
gdalwarp(srcfile = 'C:/tmp/rus_yng_for_raea_460m.tif', dstfile = rus.yng.for.outfile, ot = 'Byte',  
         s_srs = aea.rus, t_srs = 'EPSG:3571', r = 'near', tr = c(300,300), te = extnt[c(1,3,2,4)])

rus.yng.for.r <- raster(rus.yng.for.outfile)
rus.older.for.r <- is.na(rus.yng.for.r)
rus.older.for.r <- mask(rus.older.for.r, boreal.tmplt.300m.laea)
writeRaster(rus.older.for.r, 'data/gis_data/russian_forest_stand_established_before_1985_nibble1cell_laea_300m.tif', datatype = 'INT1U', overwrite = T)


#### CLEAN UP ============================================================================
gc()
removeTmpFiles()
unlink(tmp.dir, recursive = T)

#### END SCRIPT ============================================================================