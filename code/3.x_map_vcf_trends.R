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

vcf.tree.avg <- stack('data/gis_data/avhrr_vcf/avhrr_vcf_tree_mean_1982to2016_5km_laea.tif')
vcf.shortveg.avg <- stack('data/gis_data/avhrr_vcf/avhrr_vcf_shortveg_mean_1982to2016_5km_laea.tif')
vcf.barren.avg <- stack('data/gis_data/avhrr_vcf/avhrr_vcf_barren_mean_1982to2016_5km_laea.tif')

vcf.tree.trnd.gte1982 <- stack('data/gis_data/avhrr_vcf/avhrr_vcf_tree_trend_1982to2016_5km_laea.tif')
vcf.shortveg.trnd.gte1982 <- stack('data/gis_data/avhrr_vcf/avhrr_vcf_shortveg_trend_1982to2016_5km_laea.tif')
vcf.barren.trnd.gte1982 <- stack('data/gis_data/avhrr_vcf/avhrr_vcf_barren_trend_1982to2016_5km_laea.tif')

vcf.tree.trnd.gte2001 <- stack('data/gis_data/avhrr_vcf/avhrr_vcf_tree_trend_2001to2016_5km_laea.tif')
vcf.shortveg.trnd.gte2001 <- stack('data/gis_data/avhrr_vcf/avhrr_vcf_shortveg_trend_2001to2016_5km_laea.tif')
vcf.barren.trnd.gte2001 <- stack('data/gis_data/avhrr_vcf/avhrr_vcf_barren_trend_2001to2016_5km_laea.tif')

#-----------------------------------------------------
# CREATE BACKGROUND MAP
#-----------------------------------------------------
# background map
bg.map <- tm_shape(land.shp) + tm_polygons(col='gray80', border.col='gray60') + 
  tm_scale_bar(position = c("right", "bottom")) # + tm_graticules(ticks=F, alpha = 0.5, lwd = 0.5, col = 'blue', labels.size = 0)

# boreal domain
bor.map <- tm_shape(boreal.shp) + tm_borders(col = 'black', lty = 'solid', alpha = 1, lwd = 2) 

# ecoregions
eco.map <- tm_shape(ecoreg.shp) + tm_borders(col = 'black', lty = 1, alpha = 1, lwd = 1)

#--------------------------------------------------------------------------------------------------------
# PLOT MEAN TREE, SHORT VEG, AND BARREN COVER FROM 1982 TO 2016
#--------------------------------------------------------------------------------------------------------
# palette_explorer()

# mean tree cover 
tree.avg.cols  <- get_brewer_pal("BuGn", n = 9)
tree.avg.breaks <- seq(0,90,10)

tree.avg.map <- bg.map + tm_shape(vcf.tree.avg) + 
  tm_raster(style = "cont", palette = tree.avg.cols, breaks = tree.avg.breaks, 
            title = 'Tree Cover (%)', legend.show=T, legend.reverse=T) + 
  tm_layout(legend.outside = T, legend.outside.position = 'right', 
            legend.text.size = 2, legend.title.size = 2) + 
  eco.map + bor.map
  
jpeg('figures/avhrr_tree_cover_mean_1982to2016.jpg', 8, 8, units = 'in', res = 400)
tree.avg.map
dev.off()

# mean short veg cover 
shortveg.avg.cols  <- get_brewer_pal("BuGn", n = 9)
shortveg.avg.breaks <- seq(0,90,10)

shortveg.avg.map <- bg.map + tm_shape(vcf.shortveg.avg) + 
  tm_raster(style = "cont", palette = shortveg.avg.cols, breaks = shortveg.avg.breaks, 
            title = 'Short Veg. Cover (%)', legend.show=T, legend.reverse=T) + 
  tm_layout(legend.outside = T, legend.outside.position = 'right', 
            legend.text.size = 2, legend.title.size = 2) + 
  eco.map + bor.map

jpeg('figures/avhrr_shortveg_cover_mean_1982to2016.jpg', 8, 8, units = 'in', res = 400)
shortveg.avg.map
dev.off()

# mean short veg cover 
barren.avg.cols  <- get_brewer_pal("YlOrBr", n = 7)
barren.avg.breaks <- c(seq(0,25,5), 100)

barren.avg.map <- bg.map + tm_shape(vcf.barren.avg) + 
  tm_raster(style = "cont", palette = barren.avg.cols, breaks = barren.avg.breaks, 
            title = 'Barren Cover (%)', legend.show=T, legend.reverse=T) + 
  tm_layout(legend.outside = T, legend.outside.position = 'right', 
            legend.text.size = 2, legend.title.size = 2) + 
  eco.map + bor.map

jpeg('figures/avhrr_barren_cover_mean_1982to2016.jpg', 8, 8, units = 'in', res = 400)
barren.avg.map
dev.off()


# combine tree and short veg cover into one map 
vcf.avg.maps <- tmap_arrange(tree.avg.map, shortveg.avg.map)

jpeg('figures/avhrr_vcf_mean_1982to2016.jpg', 12, 4, units = 'in', res = 400)
vcf.avg.maps
dev.off()

#--------------------------------------------------------------------------------------------------------
# PLOT MANN-KENDALL TRENDS IN TREE COVER FROM 1982/2001 TO 2016
#--------------------------------------------------------------------------------------------------------
# palette_explorer()
tree.trnd.cols  <- get_brewer_pal(palette = "RdBu", n = 7)
tree.trnd.breaks <- c(-100,-50,-25,-5,0,5,25,50,100)

# tree cover change from 2001 to 2016
tree.sig.mask <- vcf.tree.trnd.gte2001[[4]] <= 0.1
tree.sig.mask[tree.sig.mask == 1] <- NA

tree.trnd.gte2001.map <- bg.map + tm_shape(vcf.tree.trnd.gte2001[[2]]) + 
  tm_raster(style = "cont", palette = tree.trnd.cols, breaks = tree.trnd.breaks, 
            title = expression('Tree Cover Change (%)'), legend.show=T, legend.reverse=T) + 
  tm_shape(tree.sig.mask) + tm_raster(col = 'layer', palette = 'white', legend.show=F) +
  tm_layout(legend.outside = T, legend.outside.position = 'right', legend.text.size = 2, legend.title.size = 2) + 
  eco.map + bor.map

jpeg('figures/avhrr_tree_cover_change_2001to2016.jpg', 8, 8, units = 'in', res = 400)
tree.trnd.gte2001.map
dev.off()

# tree cover change from 1982 to 2016
tree.sig.mask <- vcf.tree.trnd.gte1982[[4]] <= 0.1
tree.sig.mask[tree.sig.mask == 1] <- NA

tree.trnd.gte1982.map <- bg.map + tm_shape(vcf.tree.trnd.gte1982[[2]]) + 
  tm_raster(style = "cont", palette = tree.trnd.cols, breaks = tree.trnd.breaks, 
            title = expression('Tree Cover Change (%)'), legend.show=T, legend.reverse=T) + 
  tm_shape(tree.sig.mask) + tm_raster(col = 'layer', palette = 'white', legend.show=F) +
  tm_layout(legend.outside = T, legend.outside.position = 'right', legend.text.size = 2, legend.title.size = 2) + 
  eco.map + bor.map

jpeg('figures/avhrr_tree_cover_change_1982to2016.jpg', 8, 8, units = 'in', res = 400)
tree.trnd.gte1982.map
dev.off()

#--------------------------------------------------------------------------------------------------------
# PLOT MANN-KENDALL TRENDS IN SHORT VEGETATION COVER FROM 1982/2001 TO 2016
#--------------------------------------------------------------------------------------------------------
# shortveg.trnd.cols  <- get_brewer_pal(palette = "BrBG", n = 7)
shortveg.trnd.cols  <- get_brewer_pal(palette = "RdBu", n = 7)
shortveg.trnd.breaks <- c(-100,-50,-25,-5,0,5,25,50,100)

# shortveg cover change from 2001 to 2016
shortveg.sig.mask <- vcf.shortveg.trnd.gte2001[[4]] <= 0.1
shortveg.sig.mask[shortveg.sig.mask == 1] <- NA

shortveg.trnd.gte2001.map <- bg.map + tm_shape(vcf.shortveg.trnd.gte2001[[2]]) + 
  tm_raster(style = "cont", palette = shortveg.trnd.cols, breaks = shortveg.trnd.breaks, 
            title = expression('Short Veg. Cover Change (%)'), legend.show=T, legend.reverse=T) + 
  tm_shape(shortveg.sig.mask) + tm_raster(col = 'layer', palette = 'white', legend.show=F) +
  tm_layout(legend.outside = T, legend.outside.position = 'right', legend.text.size = 2, legend.title.size = 2) + 
  eco.map + bor.map

jpeg('figures/avhrr_shortveg_cover_change_2001to2016.jpg', 8, 8, units = 'in', res = 400)
shortveg.trnd.gte2001.map
dev.off()

# shortveg cover change from 1982 to 2016
shortveg.sig.mask <- vcf.shortveg.trnd.gte1982[[4]] <= 0.1
shortveg.sig.mask[shortveg.sig.mask == 1] <- NA

shortveg.trnd.gte1982.map <- bg.map + tm_shape(vcf.shortveg.trnd.gte1982[[2]]) + 
  tm_raster(style = "cont", palette = shortveg.trnd.cols, breaks = shortveg.trnd.breaks, 
            title = expression('Short Veg. Cover Change (%)'), legend.show=T, legend.reverse=T) + 
  tm_shape(shortveg.sig.mask) + tm_raster(col = 'layer', palette = 'white', legend.show=F) +
  tm_layout(legend.outside = T, legend.outside.position = 'right', legend.text.size = 2, legend.title.size = 2) + 
  eco.map + bor.map

jpeg('figures/avhrr_shortveg_cover_change_1982to2016.jpg', 8, 8, units = 'in', res = 400)
shortveg.trnd.gte1982.map
dev.off()

#--------------------------------------------------------------------------------------------------------
# 
#--------------------------------------------------------------------------------------------------------