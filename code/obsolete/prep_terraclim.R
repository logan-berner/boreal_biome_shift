# This R script takes takes global Terra Climate data and clips / reprojects to high latitudes
# Author: Logan Berner
# Date: 2019-10-02
#---------------------------------------------------------------------------------------------------------------------
rm(list=ls())
require(R.utils)
require(raster)
require(ncdf4)
require(Hmisc)
require(maptools)
require(gdalUtils)
require(rgdal)
require(rgeos)
setwd('/projects/above_gedi/')

#----------------------------------------------------------------------------------------------------------------------
# GET SLURM JOB ARRAY NUMBER
#----------------------------------------------------------------------------------------------------------------------
args <- commandArgs(TRUE)
i = as.numeric(args[1])

#----------------------------------------------------------------------------------------------------------------------
# METADATA ABOUT VARIABLES
#----------------------------------------------------------------------------------------------------------------------
var.df <- data.frame(var=c('def','pdsi','ppt','soil','srad','tmax','tmin','vpd'),
                     bitdepth.gdal = c('INT16','BYTE','INT16','INT16','INT16','BYTE','BYTE','BYTE'),
                     bitdepth.raster = c('INT2U','INT1S','INT2U','INT2U','INT2U','INT1S','INT1S','INT1U'),
                     scale = c(rep(1,8)))

#----------------------------------------------------------------------------------------------------------------------
# IDENTIFY INPUT CLIMATE FILE, SET UP METADATA FOR EACH VARIABLE, CREATE MAIN OUTPUT DIR
#----------------------------------------------------------------------------------------------------------------------
# get input climate file and parse 
in.file <- list.files('geodata/climate/terra_climate', full.names = T)[i]
var.yr <- unlist(strsplit(gsub('.nc','', gsub('TerraClimate_', '', list.files('geodata/climate/terra_climate', full.names = F)[i])), '_'))
var <- var.yr[1]
yr <- var.yr[2]

# load domain shapefile and get extent
land.45n.laea.shp <- readOGR('lberner/boreal_ndvi/data/gis_data/land_45n_laea.shp')
land.45n.extent <- extent(land.45n.laea.shp)

# load boreal biome extent shapefile
boreal.biome.laea.shp <- readOGR('lberner/boreal_ndvi/data/gis_data/wwf_ecoreg/wwf_boreal_biome_laea.shp')

# create main output directory
mkdirs('lberner/boreal_ndvi/data/gis_data/terra_clim/monthly')
mkdirs('lberner/boreal_ndvi/data/gis_data/terra_clim/tmp/tifs/')
mkdirs('lberner/boreal_ndvi/data/gis_data/terra_clim/tmp/highlat/')

#----------------------------------------------------------------------------------------------------------------------
# LOOP THROUGHT EACH ANNUAL CLIMATE FILE, OUTPUT MONTHLY LAYER AS A GEOTIF, THEN PROJECT TO LAEA, CLIPPING TO HIGH LATITUDES 
#----------------------------------------------------------------------------------------------------------------------
month.nums <- c('01','02','03','04','05','06','07','08','09','10','11','12')    
tif.files <- paste('/projects/above_gedi/lberner/boreal_ndvi/data/gis_data/terra_clim/tmp/tifs/', var,'_',yr,'_',month.nums,'.tif', sep='')
highlat.files <- paste('/projects/above_gedi/lberner/boreal_ndvi/data/gis_data/terra_clim/tmp/highlat/', var,'_',yr,'_',month.nums,'.tif', sep='')
out.files <- paste('/projects/above_gedi/lberner/boreal_ndvi/data/gis_data/terra_clim/monthly/terraclim_boreal_', var,'_',yr,'_',month.nums,'_laea_5km.tif', sep='')

# loop through the monthly bands in each file
for(j in 1:12){
  gdal_translate(src_dataset = in.file, dst_dataset = tif.files[j], b = j, of = 'GTiff', ot = var.df$bitdepth.gdal[i])
  gdalwarp(srcfile = tif.files[j], dstfile = out.files[j], s_srs = 'EPSG:4326', t_srs = 'EPSG:3572', r = 'cubic', tr = c(5000,5000), te = domain.extent[c(1,3,2,4)])
}

#----------------------------------------------------------------------------------------------------------------------
# END SCRIPT
#----------------------------------------------------------------------------------------------------------------------