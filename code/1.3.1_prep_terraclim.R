# This R script takes takes select monthly global Terra Climate data and clips / reprojects to high latitudes
# Author: Logan Berner
# Date: 2021-01-14
#---------------------------------------------------------------------------------------------------------------------
rm(list=ls())

args <- commandArgs(TRUE)
i = as.numeric(args[1])

.libPaths(c(.libPaths(), "~/R/", '/home/lb968/R/4.0.2/'))
require(R.utils)
require(raster)
require(rgdal)

wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
laea <- CRS("+proj=laea +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
setwd('/projects/arctic/')


# SET UP TEMPORARY DIRECTORY FOR RASTER ============================================================================
tmp.dir <- paste('/scratch/lb968/Rtmp/',i, '/', sep='')
mkdirs(tmp.dir)
rasterOptions(tmpdir = tmp.dir)


# METADATA ABOUT VARIABLES =========================================================================================
meta.df <- data.frame(var = c('def','pdsi','ppt','soil','swe','tmax','tmin','vpd'),
                      units = c('mm','unitlessx100','mm','mm','mm','Cx100','Cx100','kPax100'),
                      bitdepth = c('INT2U','INT2S','INT2U','INT2U','INT2U','INT2S','INT2S','INT2U'))


# IDENTIFY INPUT CLIMATE FILE, SET UP METADATA FOR EACH VARIABLE, CREATE MAIN OUTPUT DIR ===========================
all.files <- list.files('share/clim/terra_climate/9_final/')
voi.files <- all.files[unique(grep(paste(meta.df$var, collapse="|"), tolower(all.files)))]

# get input climate file and parse 
in.file <- paste0('share/clim/terra_climate/9_final/', voi.files[i])
var.yr <- unlist(strsplit(gsub('.nc','', gsub('TerraClimate_', '', voi.files[i])), '_'))
var.yr <- tolower(var.yr)
var <- var.yr[1]
yr <- var.yr[2]
mon <- gsub('.tif','', var.yr[3])
var.meta.df <- meta.df[which(meta.df$var %in% var),]

# load highlat and domain shapefiles
land.45n.wgs84.shp <- readOGR('users/lberner/boreal_biome_shift/data/gis_data/land_45n_wgs84.shp')
boreal.biome.laea.shp <- readOGR('users/lberner/boreal_biome_shift/data/gis_data/wwf_ecoreg/wwf_boreal_biome_buf5km_laea.shp')


# PROJECT TO LAEA, CLIPPING TO HIGH LATITUDES ==========================================================================
out.file <- paste('users/lberner/boreal_biome_shift/data/gis_data/terra_clim/monthly/terraclim_boreal_', var,'_',yr,'_',mon,'_', var.meta.df$units, '_laea_4km.tif', sep='')

print('starting to process...')

r <- raster::raster(in.file)
r.45n <- raster::crop(r, land.45n.wgs84.shp)
r.45n.laea <- raster::projectRaster(r.45n, res = 4000, crs = laea, method = 'bilinear')
r.45n.laea.boreal <- raster::crop(r.45n.laea, boreal.biome.laea.shp) 
r.45n.laea.boreal <- raster::mask(r.45n.laea.boreal, boreal.biome.laea.shp) 
  
# scale a few variables to maintain sig digits 
if (var == 'vpd' | var == 'tmax' | var == 'tmin' | var == 'pdsi'){
  r.45n.laea.boreal <- r.45n.laea.boreal*100
}
  
r.45n.laea.boreal <- round(r.45n.laea.boreal) # ensure everything is an integer
raster::writeRaster(r.45n.laea.boreal, out.file, datatype = as.character(var.meta.df$bitdepth), overwrite = T)
print(in.file)

# delete tmp directory
unlink(tmp.dir, recursive = T)

# END SCRIPT ================================================================================================================