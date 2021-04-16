#-------------------------------------------------------------------------------------------------------------------
# CREATE MAPS SHOWING CLIMATIC TRENDS ACROSS THE BOREAL BIOME
# AUTHOR: LOGAN BERNER, NAU
# DATE: 2021-01-07
#-------------------------------------------------------------------------------------------------------------------
rm(list=ls())
require(sf)
require(ggplot2)
require(raster)
require(rgdal)
require(broom) # for tidy()
require(ggspatial)
require(ggpubr)
# require(dplyr)
# require(spData)
# require(spDataLarge)
# require(tmap)
# require(tmaptools)
# require(leaflet)
# require(mapview)
# require(shiny)
# require(data.table)

setwd('C:/Users/Logan/Google Drive/research/nau/nasa_above/boreal_biome_shift/')

# LOAD SPATIAL DATA ===================================================================================================
boreal.shp <- readOGR(dsn = 'data/gis_data/wwf_boreal_biome_laea.shp')
land.45n.shp <-  readOGR(dsn = 'data/gis_data/land_45n_laea.shp')

def.trnd.r <- raster('data/gis_data/terra_clim/wateryear_trends/terraclim_boreal_def_trend_1959to2017_wy_sum_mm_laea_4km.tif')
ppt.trnd.r <- raster('data/gis_data/terra_clim/wateryear_trends/terraclim_boreal_ppt_trend_1959to2017_wy_sum_mm_laea_4km.tif')


# CONVERT RASTERS TO SPATIAL PIXEL DF FOR USE WITH GGPLOT =============================================================
# shapefiles must be converted to tidy()
boreal.tidy <- tidy(boreal.shp)
land.45n.tidy <- tidy(land.45n.shp)

# rasters must be converted to spatial pixel data frames. 
def.trnd.r <- mask(def.trnd.r, boreal.shp)
def.trnd.r[is.na(def.trnd.r)] <- -999
def.trnd.spdf <- as(def.trnd.r, "SpatialPixelsDataFrame")
def.trnd.df <- as.data.frame(def.trnd.spdf)
colnames(def.trnd.df) <- c("value", "x", "y")
def.trnd.df <- subset(def.trnd.df, value != -999)

# BACKGROUND MAP ======================================================================================================
bg.map <- ggplot(land.45n.tidy, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", size = 0.1, fill = "lightgrey") +
  xlab("Longitude") + ylab("Latitude") +
  coord_equal() + theme_minimal() + 
  annotation_scale(location = "bl", width_hint = 0.5)


# CLIMATE TREND MAPS =================================================================================================
def.trnd.fig <- ggplot() +  
  geom_polygon(data=land.45n.tidy, aes(x = long, y = lat, group = group), color = "black", size = 0.1, fill = "gray90") +
  geom_raster(data=def.trnd.df, aes(x=x, y=y, fill=value), alpha=1) + 
  geom_polygon(data=boreal.tidy, aes(x = long, y = lat, group = group), color = "gray40", size = 0.1, fill = NA) +
  scale_fill_gradient2(low = "darkblue", mid = "ghostwhite", high = "darkred", midpoint = 0.0, 
                       na.value = "grey50", guide = "colourbar") +
  labs(fill = expression('Deficit trend (mm yr'^-1*')')) + coord_equal() + theme_void() +
  theme(legend.position="bottom", legend.box="horizontal", legend.key.width=unit(2, "cm")) + 
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))

ggsave('figures/dev_def_trend.jpg', width = 5, height = 5, units = 'in', dpi = 400)


# multi-panel climate trend figures
ggarrange(def.trnd.fig, def.trnd.fig, labels = c("(a)", "(b)"), label.x = 0.15, label.y = 0.95, ncol = 2, nrow = 1)
ggsave('figures/dev_def_trend.jpg', width = 9, height = 5, units = 'in', dpi = 400)
