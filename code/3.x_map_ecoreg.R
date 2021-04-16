# This R script preps maps mean and trends in AVHRR Vegetation Continuous Fields (tree, short veg, barren) 
# from 1982 to 2016 for the boreal biome.
# Author: Logan Berner, NAU
# Date: 2019-07-26
rm(list=ls())
require(raster)
require(sp)
require(rgdal)
require(R.utils)
require(tmap)
require(tmaptools)

wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
laea <- CRS("+proj=laea +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
# setwd('C:/Users/lb968/Google Drive/research/nau/nasa_above/boreal_ndvi/')
setwd('C:/Users/Logan/Google Drive/research/nau/nasa_above/boreal_ndvi/')
source('code/fun_stack_trend.R')

# load spatial data
boreal.shp <- readOGR(dsn = 'data/gis_data/wwf_boreal_biome_laea.shp')
land.shp <-  readOGR(dsn = 'data/gis_data/land_45n_laea.shp')
ecoreg.shp <- readOGR(dsn = 'data/gis_data/wwf_boreal_ecoregs_laea.shp')

#-----------------------------------------------------
# CREATE BACKGROUND MAP
#-----------------------------------------------------
# background map
bg.map <- tm_shape(land.shp) + tm_polygons(col='gray80', border.col='gray60') + 
  tm_scale_bar(position = c("right", "bottom")) # + tm_graticules(ticks=F, alpha = 0.5, lwd = 0.5, col = 'blue', labels.size = 0)

# boreal domain
bor.map <- tm_shape(boreal.shp) + tm_borders(col = 'black', lty = 'solid', alpha = 1, lwd = 1.5) 

# ecoregions
eco.map <- tm_shape(ecoreg.shp) + tm_polygons('ECO_NAME')

ecoreg.map <- bg.map + eco.map + bor.map + 
  tm_layout(legend.outside = T, legend.outside.position = 'right', 
            legend.text.size = 2, legend.title.size = 2)

jpeg('figures/boreal_ecoreg_map.jpeg', 6, 6, res=400, units='in')
ecoreg.map
dev.off()
