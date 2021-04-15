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
require(tmap)
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

##### LOAD FILES ------------------------------------------------------------------------------
# domain files
land.45n.laea.shp <- readOGR(dsn = 'data/gis_data/land_45n_laea.shp')
boreal.biome.laea.shp <- readOGR(dsn = 'data/gis_data/wwf_boreal_biome_laea.shp')
boreal.tmplt.300m.laea <- raster('data/gis_data/wwf_boreal_biome_laea_300m.tif')
extnt <- extent(land.45n.laea.shp)

# disturbance data sets
ak.fire.file <- 'data/gis_data/fires/ak_fires_1985_2017_wgs84_ca300m.tif'
canlad.file <- 'A:/research/data/fires/canadian_landsat_disturbance/CanLaD_20151984_latest_type.tif'
low.2009.file <-  'data/field_data/last_wild_boreal_2009_laea.shp'
hfi.infile <- 'A:/research/data/landcover/human_footprint/wildareas-v3-2009-human-footprint-geotiff/wildareas-v3-2009-human-footprint.tif'
modis.fire.r <- raster('C:/Users/Logan/Google Drive/earth_engine/modis_v6_boreal_burned_area_2000to2017_500m_laea.tif')
rus.yng.for.r <- raster('A:/research/data/forest_age/Young_Russian_Forest_Map_1330/data/Russia_Stand_Age.tif')

# land cover 
lc.r <- raster('data/gis_data/landcov/esa_cci_boreal_stable_landcov_oi_1992to2015_300m_laea.tif')

# ecological land units
elu.file <- 'A:/research/data/landcover/World_ELU_2015/globalelu/World_ELU_2015.tif'

# VCF 
vcf.infiles <- list.files('A:/research/data/landcover/avhrr_vcf_1982to2016/annual/', full.names=T, pattern = glob2rx('*.tif'))

# MODIS tree cover 
modis.treecov.r <- raster('C:/Users/Logan/Google Drive/earth_engine/modis_v6_boreal_treecov_median_2000to2017_250m_laea.tif')


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
writeRaster(modis.treecov.r, 'data/gis_data/landcov/modis_boreal_treecov_median_2000to2017_laea_300m.tif', datatype = 'INT1U', overwrite = T)


#### MODIS BURNED AREA ============================================================================
# expand burned area cells by one cell in every direction  
val.cells <- raster::Which(modis.fire.r == 1, cells = T)
adj.cells <- adjacent(modis.fire.r, cells=val.cells, directions=8, pairs=F)
modis.fire.r[adj.cells] <- 1
modis.fire.r <- crop(modis.fire.r, boreal.biome.laea.shp)
modis.fire.r <- raster::resample(modis.fire.r, boreal.tmplt.300m.laea, method = 'ngb')
modis.fire.r <- mask(modis.fire.r, boreal.tmplt.300m.laea)
writeRaster(modis.fire.r, 'data/gis_data/modis_burned_2000_2017_grow1cell_boreal_laea_300m.tif', datatype = 'INT1U', overwrite = T)

modis.nofire.r <- modis.fire.r == 0
modis.nofire.r <- mask(modis.nofire.r, boreal.tmplt.300m.laea)
writeRaster(modis.nofire.r, 'data/gis_data/modis_notburned_2000_2017_nibble1cell_boreal_laea_300m.tif', datatype = 'INT1U', overwrite = T)

#### ALASKA LARGE FIRES ============================================================================
# (subset years and rasterized using ArcGIS)
# project to laea, clip / mask to boreal biome
ak.fire.gdalout.file <- 'C:/tmp/ak_fires_laea_300m.tif'
gdalwarp(srcfile = ak.fire.file, dstfile = ak.fire.gdalout.file, 
         s_srs = 'EPSG:4269', t_srs = 'EPSG:3571', r = 'max', tr = c(300,300), te = extnt[c(1,3,2,4)])

# expand burned area cells by one cell in every direction  
ak.fire.r <- raster(ak.fire.gdalout.file)
val.cells <- raster::Which(ak.fire.r == 1, cells = T)
adj.cells <- adjacent(ak.fire.r, cells=val.cells, directions=8, pairs=F)
ak.fire.r[adj.cells] <- 1
ak.fire.r <- mask(ak.fire.r, boreal.tmplt.300m.laea)
writeRaster(ak.fire.r, 'data/gis_data/alaskan_forest_fires_1985_2017_grow1cell_boreal_laea_300m.tif', datatype = 'INT1U', overwrite = T)

ak.nofire.r <- is.na(ak.fire.r)
ak.nofire.r <- mask(ak.nofire.r, boreal.tmplt.300m.laea)
writeRaster(ak.nofire.r, 'data/gis_data/alaskan_forest_no_fires_1985_2017_nibble1cell_boreal_laea_300m.tif', datatype = 'INT1U', overwrite = T)

#### CANADIAN LANDSAT DISTURBANCE (FIRE AND HARV) ============================================================================
# project to laea, clip / mask to boreal biome
canlad.gdalout.file <- 'C:/tmp/canlad_laea_300m.tif'
gdalwarp(srcfile = canlad.file, dstfile = canlad.gdalout.file, 
         s_srs = 'EPSG:42309', t_srs = 'EPSG:3571', r = 'max', tr = c(300,300), te = extnt[c(1,3,2,4)])

# expand disturbed cells by one cell in every direction  
canlad.r <- raster(canlad.gdalout.file)
canlad.r[canlad.r <= 2] <- 1 # set disturbance to eql 1
val.cells <- raster::Which(canlad.r == 1, cells = T)
adj.cells <- adjacent(canlad.r, cells=val.cells, directions=8, pairs=F)
canlad.r[adj.cells] <- 1
canlad.r <- mask(canlad.r, boreal.tmplt.300m.laea)
writeRaster(canlad.r, 'data/gis_data/canadian_forest_fire_or_harv_since_1984_grow1cell_boreal_laea_300m.tif', datatype = 'INT1U', overwrite = T)

canlad.nodisturb.r <- is.na(canlad.r)
canlad.nodisturb.r <- mask(canlad.nodisturb.r, boreal.tmplt.300m.laea)
writeRaster(canlad.nodisturb.r, 'data/gis_data/canadian_forest_no_fire_or_harv_since_1984_grow1cell_boreal_laea_300m.tif', datatype = 'INT1U', overwrite = T)

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

#### HUMAN FOOTPRINT INDEX AT 300 M ============================================================================
hfi.highlat.file <- 'C:/tmp/R_tmp/hfi_2009_highlat.tif'
gdalwarp(srcfile = hfi.infile, dstfile = hfi.highlat.file, s_srs = 'EPSG:54009', t_srs = 'EPSG:3571', 
         r = 'bilinear', tr = c(300,300), te = extnt[c(1,3,2,4)], srcnodata = 128)
hfi.r <- raster(hfi.highlat.file)
hfi.r <- crop(hfi.r, boreal.tmplt.300m.laea)
extent(hfi.r) <- alignExtent(hfi.r, boreal.tmplt.300m.laea)
hfi.r <- mask(hfi.r, boreal.tmplt.300m.laea)
hfi.r <- round(hfi.r)
writeRaster(hfi.r, 'data/gis_data/human_footprint_2009_v3_boreal_300m_laea.tif', datatype = 'INT1U', overwrite = T)


#### ESA LAND COVER (1992-2015) ------------------------------------------------------------------------------
lc.r <- raster::resample(lc.r, boreal.tmplt.300m.laea)
writeRaster(lc.r, 'data/gis_data/landcov/esa_cci_boreal_stable_landcov_oi_1992to2015_300m_laea.tif', datatype='INT1U', overwrite=T)

lc.r <- raster('data/gis_data/landcov/esa_cci_boreal_stable_landcov_oi_1992to2015_300m_laea.tif')
lc.r[lc.r >= 1] <- 1
writeRaster(lc.r, 'data/gis_data/landcov/esa_cci_boreal_stable_landcov_mask_1992to2015_300m_laea.tif', datatype='INT1U', overwrite=T)


#### CLEAN UP ============================================================================
gc()
removeTmpFiles()
unlink(tmp.dir, recursive = T)

#### END SCRIPT ----------------------------------------------------------------------------
# #### VCF LAND COVER 1982-2016 ============================================================================
# mkdirs('C:/tmp/vcf/')
# mkdirs('data/gis_data/avhrr_vcf')
# mkdirs('data/gis_data/avhrr_vcf/annual_tree/')
# mkdirs('data/gis_data/avhrr_vcf/annual_shortveg/')
# mkdirs('data/gis_data/avhrr_vcf/annual_barren/')
# 
# vcf.yoi <- 1982:2016
# vcf.yoi <- vcf.yoi[-which(vcf.yoi == 1994 | vcf.yoi == 2000)] # data set doesn't include these years
# 
# vcf.tree.outfiles <- paste(getwd(), '/data/gis_data/avhrr_vcf/annual_tree/avhrr_vcf_tree_', vcf.yoi, '_5km_laea.tif', sep='')
# vcf.shortveg.outfiles <- paste(getwd(), '/data/gis_data/avhrr_vcf/annual_shortveg/avhrr_vcf_shortveg_', vcf.yoi, '_5km_laea.tif', sep='')
# vcf.bare.outfiles <- paste(getwd(), '/data/gis_data/avhrr_vcf/annual_barren/avhrr_vcf_barren_', vcf.yoi, '_5km_laea.tif', sep='')
# 
# boreal.biome.shp <- readOGR('data/gis_data/wwf_boreal_biome_wgs84.shp')
# boreal.biome.laea.shp <- readOGR('data/gis_data/wwf_boreal_biome_laea.shp')
# land.45n.wgs84.shp <- readOGR('data/gis_data/land_45n_wgs84.shp')
# 
# n.files <- length(vcf.infiles)
# for (i in 1:n.files){
#   brk <- brick(vcf.infiles[i])
#   brk[brk == 255] <- NA
#   brk.boreal.wgs84 <- crop(brk, land.45n.wgs84.shp)
#   brk.boreal.laea <- projectRaster(from = brk.boreal.wgs84, crs = laea, res = 5000, method = 'bilinear')
#   brk.boreal.laea <- mask(crop(brk.boreal.laea, boreal.biome.laea.shp), boreal.biome.laea.shp)
#   brk.boreal.laea <- round(brk.boreal.laea)
#   
#   # write out each land cover to a seperate TIFF
#   writeRaster(brk.boreal.laea[[1]], vcf.tree.outfiles[i], overwrite=T, dataType='INT1U')
#   writeRaster(brk.boreal.laea[[2]], vcf.shortveg.outfiles[i], overwrite=T, dataType='INT1U')
#   writeRaster(brk.boreal.laea[[3]], vcf.bare.outfiles[i], overwrite=T, dataType='INT1U')
#   print(i/n.files)
# }

# ULTIMATELY CLIPPED / PROJECTED LARGE SHAPEFILES USING ARCGIS

# #### PREP MAP OF ABOVE-GROUND BIOMASS --------------------------------------------------------------------------
# agb.45n.r <- crop(agb.r, land.45n.shp)
# agb.45n.laea.r <- projectRaster(agb.45n.r, boreal.tmplt.5km.laea, res = 5000, method = 'bilinear')
# agb.boreal.laea.r <- mask(crop(agb.45n.laea.r, boreal.tmplt.5km.laea), boreal.tmplt.5km.laea) 
# writeRaster(agb.boreal.laea.r, 'data/gis_data/boreal_forest_agb_kgCm2.tif', overwrite=T)
# 
# tm_shape(agb.boreal.laea.r) + tm_raster()
# 

# #### TAIGA - TUNDRA ECOTONE --------------------------------------------------------------------------
# tte.laea.shp <- spTransform(tte.shp, laea)
# tte.laea.shp <- SpatialPolygonsDataFrame(tte.laea.shp, data = data.frame(tte.laea.shp[,4])) # just grab col specifying TTE type
# tte.boreal.laea.shp <- intersect(tte.laea.shp, boreal.biome.laea.shp)
# writeOGR(obj = tte.laea.shp, dsn = 'data/gis_data/ranson_taiga_tundra_ecotone_laea.shp', 
#          layer = "ranson_taiga_tundra_ecotone_laea", driver = "ESRI Shapefile", overwrite_layer = T)
# 
# # rasterize TTE onto 300 m ESA LC grid
# # tte.in <- 'data/gis_data/ranson_taiga_tundra_ecotone_laea.shp'
# # tte.out <- 'data/gis_data/ranson_taiga_tundra_ecotone_laea.tif'
# # gdal_rasterize(tte.in, tte.out, tr = c(300,300), te = extnt[c(1,3,2,4)])
# 
# #### INTACT FORESTS LANDSCAPES --------------------------------------------------------------------------
# ifl.2016.shp <- intersect(ifl.2016.shp, land.45n.shp)
# ifl.2016.laea.shp <- spTransform(ifl.2016.shp, laea)
# 
# writeOGR(obj = ifl.2016.laea.shp, dsn = 'data/gis_data/intact_forest_landscape_2016_laea.shp', 
#          layer = "intact_forest_landscape_2016_laea", driver = "ESRI Shapefile", overwrite_layer = T)
# 
# #### LAST OF THE WILDERNESS --------------------------------------------------------------------------
# low.2009.laea.shp <- spTransform(low.2009.shp, laea)
# qtm(low.2009.laea.shp)
# # low.2009.laea.shp <- SpatialPolygonsDataFrame(ifl.laea.shp, data = data.frame(ifl.laea.shp[,4])) # just grab col specifying ifl type
# 
# writeOGR(obj = low.2009.laea.shp, dsn = 'data/gis_data/last_wilderness_2009_laea.shp', 
#          layer = "last_wilderness_2009_laea", driver = "ESRI Shapefile", overwrite_layer = T)

##### LAST OF THE WILDERNESS ============================================================================
# low.2009.laea.shp <- spTransform(low.2009.shp, laea)
# qtm(low.2009.laea.shp)
# writeOGR(obj = low.2009.laea.shp, dsn = 'data/gis_data/last_wilderness_2009_laea.shp',
#          layer = "last_wilderness_2009_laea", driver = "ESRI Shapefile", overwrite_layer = T)

# # rasterize
# low.gdalout.file <- 'data/gis_data/last_wild_boreal_2009_laea_300m.tif'
# gdal_rasterize(src_datasource = 'data/gis_data/last_wild_boreal_2009_laea.shp', dst_filename = low.gdalout.file, 
#                burn = 1, tr = c(300,300), te = extnt[c(1,3,2,4)], ot = 'Byte', a_nodata = 0)
