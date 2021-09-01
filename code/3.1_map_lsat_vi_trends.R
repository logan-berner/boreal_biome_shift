# THIS R SCRIPT CREATES A MULTI-PANEL FIGURE SUMMARIZING VEGETATION GREENING AND BROWING W/IN LAND COVER TYPES ACROSS THE BOREAL FOREST BIOME.
# AUTHOR: LOGAN BERNER, NAU
# DATE: 2021-04-28
rm(list=ls())
require(data.table)
require(dplyr)
require(ggplot2)
require(ggpubr)
require(viridis)
require(tidyr)
require(sf)
require(raster)
require(rgdal)
require(broom) # for tidy()
require(ggspatial)
require(R.utils)
require(RColorBrewer)

setwd('/projects/arctic/users/lberner/boreal_biome_shift/')

raster2spdf <- function(r){
  df <- as.data.frame(as(r, "SpatialPixelsDataFrame"))
  colnames(df) <- c("value", "x", "y")
  df
}

# # AGGREGATE TO COARSER RESOLUTION FOR MAPPING ==============================================================================
# # load data sets at 300 m resolution
# aoi.r <- raster('data/gis_data/boreal_sampling_frame_300m_laea.tif')
# green.pcnt.1985.r <- raster('output/lsat_vi_gs_ecounit_trends_frac/lsat_vi_gs_boreal_ecounit_greening_pcnt_sites_q500_1985to2019_300m_laea.tif')
# green.pcnt.2000.r <- raster('output/lsat_vi_gs_ecounit_trends_frac/lsat_vi_gs_boreal_ecounit_greening_pcnt_sites_q500_2000to2019_300m_laea.tif')
# brown.pcnt.1985.r <- raster('output/lsat_vi_gs_ecounit_trends_frac/lsat_vi_gs_boreal_ecounit_browning_pcnt_sites_q500_1985to2019_300m_laea.tif')
# brown.pcnt.2000.r <- raster('output/lsat_vi_gs_ecounit_trends_frac/lsat_vi_gs_boreal_ecounit_browning_pcnt_sites_q500_2000to2019_300m_laea.tif')
# median.pcnt.1985.r <- raster('output/lsat_vi_gs_ecounit_trends_median/lsat_vi_gs_boreal_ecounit_total_change_pcnt_med_q500_1985to2019_300m_laea.tif')
# median.pcnt.2000.r <- raster('output/lsat_vi_gs_ecounit_trends_median/lsat_vi_gs_boreal_ecounit_total_change_pcnt_med_q500_2000to2019_300m_laea.tif')
# r.list <- list(green.pcnt.1985.r, green.pcnt.2000.r, brown.pcnt.1985.r, brown.pcnt.2000.r, median.pcnt.1985.r, median.pcnt.2000.r)
# 
# # create mask at 1500 m resolution
# aoi.sum.1500m.r <- raster::aggregate(aoi.r, fact = c(5,5), fun = sum, na.rm=T)
# aoi.pcnt.1500m.r <- aoi.sum.1500m.r / 25 * 100
# aoi.abv.50pcnt.1500m.r <- aoi.pcnt.1500m.r >= 50
# writeRaster(aoi.abv.50pcnt.1500m.r, 'data/gis_data/boreal_sampling_frame_50pcnt_threshold_1500m_laea.tif')
# 
# # aggregate trend layers to coarser resolution and mask
# mkdirs('output/lsat_vs_gs_ecounit_data_for_trend_maps')
# for (i in 1:length(r.list)){
#   r <- r.list[[i]]
#   r <- raster::aggregate(r, fact = 5, fun = 'mean', na.rm=T)
#   r <- mask(r, aoi.abv.50pcnt.1500m.r)
#   outname <- paste0('output/lsat_vs_gs_ecounit_data_for_trend_maps/',gsub('300m','1500m',names(r)),'.tif')
#   writeRaster(r, outname, overwrite=T)
#   print(i)
# }

# LOAD FILES =================================================================================================================
boreal.shp <- readOGR(dsn = 'data/gis_data/wwf_boreal_biome_laea.shp')
land.45n.shp <-  readOGR(dsn = 'data/gis_data/land_45n_laea.shp')
r.files <- list.files('output/lsat_vs_gs_ecounit_data_for_trend_maps/', full.names = T)
trend.stk <- stack(r.files)
aoi.r <- raster('data/gis_data/boreal_sampling_frame_50pcnt_threshold_1500m_laea.tif')
 names(trend.stk)

# SPATIAL DATA PREP =================================================================================================================
boreal.tidy <- tidy(boreal.shp)
land.45n.tidy <- tidy(land.45n.shp)
aoi.sdf <- raster2spdf(aoi.r)
trend.sdf <- raster2spdf(trend.stk)
colnames(trend.sdf) <- c('brn.1985','brn.2000','grn.1985','grn.2000','med.1985','med.2000','x','y')

# adjust ranges
pcnt.upr.lim <- 50
pcnt.lwr.lim <- 10
pcnt.mid.lim <- (pcnt.upr.lim-pcnt.lwr.lim)/2+pcnt.lwr.lim

med.upr.lim <- 5
med.lwr.lim <- -5

trend.sdf$grn.1985[trend.sdf$grn.1985 > pcnt.upr.lim] <- pcnt.upr.lim 
trend.sdf$grn.2000[trend.sdf$grn.2000 > pcnt.upr.lim] <- pcnt.upr.lim 
trend.sdf$brn.1985[trend.sdf$brn.1985 > pcnt.upr.lim] <- pcnt.upr.lim 
trend.sdf$brn.2000[trend.sdf$brn.2000 > pcnt.upr.lim] <- pcnt.upr.lim 

trend.sdf$grn.1985[trend.sdf$grn.1985 < pcnt.lwr.lim] <- pcnt.lwr.lim 
trend.sdf$grn.2000[trend.sdf$grn.2000 < pcnt.lwr.lim] <- pcnt.lwr.lim 
trend.sdf$brn.1985[trend.sdf$brn.1985 < pcnt.lwr.lim] <- pcnt.lwr.lim 
trend.sdf$brn.2000[trend.sdf$brn.2000 < pcnt.lwr.lim] <- pcnt.lwr.lim 

trend.sdf$med.1985[trend.sdf$med.1985 > med.upr.lim] <- med.upr.lim
trend.sdf$med.2000[trend.sdf$med.2000 > med.upr.lim] <- med.upr.lim 

trend.sdf$med.1985[trend.sdf$med.1985 < med.lwr.lim] <- med.lwr.lim
trend.sdf$med.2000[trend.sdf$med.2000 < med.lwr.lim] <- med.lwr.lim


# BASE MAP =================================================================================================

base.map <- ggplot() +  
  geom_polygon(data=land.45n.tidy, aes(x = long, y = lat, group = group), color = "gray10", size = 0.1, fill = "gray80") +
  geom_polygon(data=boreal.tidy, aes(x = long, y = lat, group = group), color = NA, size = 0.1, fill = "white") +
  coord_equal() + theme_void() + guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)) +
  theme(legend.position="bottom", legend.box="horizontal", legend.key.width=unit(1.25, "cm"),
        legend.text=element_text(size=12), legend.title=element_text(size=14),
        plot.margin = unit(c(0,0,0,0), "cm")) 
  
# base.map


# TREND MAPS ===========================================================================================

# colors
green.cols <- c('seashell2','springgreen4','royalblue4')
brown.cols <- c('seashell2','orange4','violetred4')
trnd.cols <- c('violetred4','orange4','seashell2','springgreen4','royalblue4')

# greening --------------
grn.1985.map <- base.map + geom_raster(data=aoi.sdf, aes(x=x, y=y, fill=value), fill = 'black', alpha=1) +
  geom_raster(data=trend.sdf, aes(x=x, y=y, fill=grn.1985), alpha=1) + 
  geom_polygon(data=boreal.tidy, aes(x = long, y = lat, group = group), color = "black", size = 0.2, fill = NA) +
  scale_fill_gradientn(colours = green.cols, breaks = c(pcnt.lwr.lim, pcnt.mid.lim, pcnt.upr.lim),
                       labels = c(paste0('<', pcnt.lwr.lim),pcnt.mid.lim,paste0('>', pcnt.upr.lim)), 
                       limits = c(pcnt.lwr.lim,pcnt.upr.lim)) +
  labs(fill = 'Greening prevalence\n(% of sample sites)')
# grn.1985.map

grn.2000.map <- base.map + geom_raster(data=trend.sdf, aes(x=x, y=y, fill=grn.2000), alpha=1) + 
  geom_polygon(data=boreal.tidy, aes(x = long, y = lat, group = group), color = "black", size = 0.2, fill = NA) +
  scale_fill_gradientn(colours = green.cols, breaks = c(pcnt.lwr.lim, pcnt.mid.lim, pcnt.upr.lim),  
                       labels = c(paste0('<', pcnt.lwr.lim),pcnt.mid.lim,paste0('>', pcnt.upr.lim)), 
                       limits = c(pcnt.lwr.lim,pcnt.upr.lim)) +
  labs(fill = 'Greening prevalence\n(% of sample sites)')
# grn.2000.map


# browning ---------------
brn.1985.map <- base.map + geom_raster(data=aoi.sdf, aes(x=x, y=y, fill=value), fill = 'black', alpha=1) +
  geom_raster(data=trend.sdf, aes(x=x, y=y, fill=brn.1985), alpha=1) + 
  geom_polygon(data=boreal.tidy, aes(x = long, y = lat, group = group), color = "black", size = 0.2, fill = NA) +
  scale_fill_gradientn(colours = brown.cols, breaks = c(pcnt.lwr.lim, pcnt.mid.lim, pcnt.upr.lim),
                       labels = c(paste0('<', pcnt.lwr.lim),pcnt.mid.lim,paste0('>', pcnt.upr.lim)), 
                       limits = c(pcnt.lwr.lim,pcnt.upr.lim)) +
  labs(fill = 'Browning prevalence\n(% of sample sites)')
# brn.1985.map

brn.2000.map <- base.map + geom_raster(data=trend.sdf, aes(x=x, y=y, fill=brn.2000), alpha=1) + 
  geom_polygon(data=boreal.tidy, aes(x = long, y = lat, group = group), color = "black", size = 0.2, fill = NA) +
  scale_fill_gradientn(colours = brown.cols, breaks = c(pcnt.lwr.lim, pcnt.mid.lim, pcnt.upr.lim),
                       labels = c(paste0('<', pcnt.lwr.lim),pcnt.mid.lim,paste0('>', pcnt.upr.lim)), 
                       limits = c(pcnt.lwr.lim,pcnt.upr.lim)) +
  labs(fill = 'Browning prevalence\n(% of sample sites)')
# brn.2000.map


# median change ----------- 
med.1985.map <- base.map + geom_raster(data=aoi.sdf, aes(x=x, y=y, fill=value), fill = 'black', alpha=1) +
  geom_raster(data=trend.sdf, aes(x=x, y=y, fill=med.1985), alpha=1) + 
  geom_polygon(data=boreal.tidy, aes(x = long, y = lat, group = group), color = "black", size = 0.2, fill = NA) +
  scale_fill_gradientn(colours = trnd.cols, breaks = c(-5,0,5), labels = c('<-5','0','>5'), limits = c(-5,5)) +
  labs(fill = 'Greenness trend\n(median % change)')
# med.1985.map

med.2000.map <- base.map + geom_raster(data=trend.sdf, aes(x=x, y=y, fill=med.2000), alpha=1) + 
  geom_polygon(data=boreal.tidy, aes(x = long, y = lat, group = group), color = "black", size = 0.2, fill = NA) +
  scale_fill_gradientn(colours = trnd.cols, breaks = c(-5,0,5), labels = c('<-5','0','>5'), limits = c(-5,5)) +
  labs(fill = 'Greenness trend\n(median % change)')
# med.2000.map


# COMBINE PANELS =============================================================================================
grn.maps <- ggarrange(grn.1985.map, grn.2000.map, labels=c('a','d'), align = 'v', ncol = 1, nrow = 2, label.x = 0.20, label.y = 0.98, common.legend = T, legend = 'bottom')
brn.maps <- ggarrange(brn.1985.map, brn.2000.map, labels=c('b','e'), align = 'v', ncol = 1, nrow = 2, label.x = 0.20, label.y = 0.98, common.legend = T, legend = 'bottom')
med.maps <- ggarrange(med.1985.map, med.2000.map, labels=c('c','f'), align = 'v', ncol = 1, nrow = 2, label.x = 0.20, label.y = 0.98, common.legend = T, legend = 'bottom')
combo.maps <- ggarrange(grn.maps, brn.maps, med.maps, ncol = 3)
ggsave('figures/paper/fig_2_trend_maps.jpg', plot = combo.maps, width = 10, height = 7, units = 'in', dpi = 400)

# END SCRIPT =====================================================================================================  