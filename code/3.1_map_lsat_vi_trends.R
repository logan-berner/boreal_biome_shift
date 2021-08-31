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

setwd('/projects/arctic/users/lberner/boreal_biome_shift/')

raster2spdf <- function(r){
  df <- as.data.frame(as(r, "SpatialPixelsDataFrame"))
  colnames(df) <- c("value", "x", "y")
  df
}

# AGGREGATE TO COARSER RESOLUTION FOR MAPPING ==============================================================================
# load data sets at 300 m resolution
aoi.r <- raster('data/gis_data/boreal_sampling_frame_300m_laea.tif')
green.pcnt.1985.r <- raster('output/lsat_vi_gs_ecounit_trends_frac/lsat_vi_gs_boreal_ecounit_greening_pcnt_sites_q500_1985to2019_300m_laea.tif')
green.pcnt.2000.r <- raster('output/lsat_vi_gs_ecounit_trends_frac/lsat_vi_gs_boreal_ecounit_greening_pcnt_sites_q500_2000to2019_300m_laea.tif')
brown.pcnt.1985.r <- raster('output/lsat_vi_gs_ecounit_trends_frac/lsat_vi_gs_boreal_ecounit_browning_pcnt_sites_q500_1985to2019_300m_laea.tif')
brown.pcnt.2000.r <- raster('output/lsat_vi_gs_ecounit_trends_frac/lsat_vi_gs_boreal_ecounit_browning_pcnt_sites_q500_2000to2019_300m_laea.tif')
median.pcnt.1985.r <- raster('output/lsat_vi_gs_ecounit_trends_median/lsat_vi_gs_boreal_ecounit_total_change_pcnt_med_q500_1985to2019_300m_laea.tif')
median.pcnt.2000.r <- raster('output/lsat_vi_gs_ecounit_trends_median/lsat_vi_gs_boreal_ecounit_total_change_pcnt_med_q500_2000to2019_300m_laea.tif')
r.list <- list(green.pcnt.1985.r, green.pcnt.2000.r, brown.pcnt.1985.r, brown.pcnt.2000.r, median.pcnt.1985.r, median.pcnt.2000.r)

# create mask at 1500 m resolution
aoi.sum.1500m.r <- raster::aggregate(aoi.r, fact = c(5,5), fun = sum, na.rm=T)
aoi.pcnt.1500m.r <- aoi.sum.1500m.r / 25 * 100
aoi.abv.50pcnt.1500m.r <- aoi.pcnt.1500m.r >= 50
writeRaster(aoi.abv.50pcnt.1500m.r, 'data/gis_data/boreal_sampling_frame_50pcnt_threshold_1500m_laea.tif')

# aggregate trend layers to coarser resolution and mask
mkdirs('output/lsat_vs_gs_ecounit_data_for_trend_maps')
for (i in 1:length(r.list)){
  r <- r.list[[i]]
  r <- raster::aggregate(r, fact = 5, fun = 'mean', na.rm=T)
  r <- mask(r, aoi.abv.50pcnt.1500m.r)
  outname <- paste0('output/lsat_vs_gs_ecounit_data_for_trend_maps/',gsub('300m','1500m',names(r)))
  writeRaster(r, outname, overwrite=T)
  print(i)
}

# LOAD FILES =================================================================================================================
boreal.shp <- readOGR(dsn = 'data/gis_data/wwf_boreal_biome_laea.shp')
land.45n.shp <-  readOGR(dsn = 'data/gis_data/land_45n_laea.shp')
green.pcnt.1985.r <- raster('output/lsat_vi_gs_ecounit_trends_frac/lsat_vi_gs_boreal_ecounit_greening_pcnt_sites_q500_1985to2019_300m_laea.tif')
green.pcnt.2000.r <- raster('output/lsat_vi_gs_ecounit_trends_frac/lsat_vi_gs_boreal_ecounit_greening_pcnt_sites_q500_2000to2019_300m_laea.tif')
brown.pcnt.1985.r <- raster('output/lsat_vi_gs_ecounit_trends_frac/lsat_vi_gs_boreal_ecounit_browning_pcnt_sites_q500_1985to2019_300m_laea.tif')
brown.pcnt.2000.r <- raster('output/lsat_vi_gs_ecounit_trends_frac/lsat_vi_gs_boreal_ecounit_browning_pcnt_sites_q500_2000to2019_300m_laea.tif')


# GENERAL DATA PREP =================================================================================================================
boreal.tidy <- tidy(boreal.shp)
land.45n.tidy <- tidy(land.45n.shp)

green.pcnt.1985.sdf <- raster2spdf(green.pcnt.1985.1500m.r)
fivenum(green.pcnt.1985.sdf$value)

# ADJUST RANGE =============================================================================================
green.pcnt.1985.sdf$value[green.pcnt.1985.sdf$value >= 50] <- 50

# FIGURE COLORS ===========================================================================================
trnd.cols <- c('saddlebrown','springgreen3')
n.cols <- length(levels(landcov.frac.trnds.dt$landcov.name))
lc.col.df <- data.frame(lc.class = levels(landcov.frac.trnds.smry.dt$landcov.name), col = viridis::turbo(n.cols))
fwrite(lc.col.df, 'data/gis_data/landcov/landcov_colors.csv')

# LAND COVER MAP =================================================================================================
green.cols <- c('tan','yellow','green1','green4')

green.pcnt.1985.map <- ggplot() +  
  geom_polygon(data=land.45n.tidy, aes(x = long, y = lat, group = group), color = "gray10", size = 0.1, fill = "gray80") +
  geom_polygon(data=boreal.tidy, aes(x = long, y = lat, group = group), color = NA, size = 0.1, fill = "white") +
  geom_raster(data=green.pcnt.1985.sdf, aes(x=x, y=y, fill=value), alpha=1) + 
  geom_polygon(data=boreal.tidy, aes(x = long, y = lat, group = group), color = "black", size = 0.2, fill = NA) +
  scale_fill_gradientn(colours = green.cols, breaks = c(0,25,50), labels = c('0','25','>50'), limits = c(0,50)) +
  labs(fill = 'Greening prevalence\n(% of sample sites)') + coord_equal() + theme_void() +
  theme(legend.position="bottom", legend.box="horizontal", legend.key.width=unit(1.5, "cm"),
        legend.text=element_text(size=12), legend.title=element_text(size=14)) + 
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))

green.pcnt.1985.map


# COMBINE PANELS =============================================================================================
fig.col1 <- ggarrange(lc.map, fig.b, labels=c('a','c'), align = 'v', ncol = 1, nrow = 2, label.x = 0.91, label.y = 0.98)
fig.col2 <- ggarrange(fig.a, fig.c, labels=c('b','d'), ncol = 1, nrow = 2, label.x = 0.91, label.y = 0.98)
fig.combo <- ggarrange(fig.col1, fig.col2, ncol = 2)
fig.combo
ggsave('figures/paper/fig_3_landcover.jpg', width = 9, height = 6, units = 'in', dpi = 400)


# END PLOT =====================================================================================================