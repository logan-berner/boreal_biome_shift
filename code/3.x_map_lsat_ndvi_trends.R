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

setwd('C:/Users/Logan/Google Drive/research/nau/nasa_above/boreal_biome_shift/')

# CUSTORM FUNCTIONS ===================================================================================================
raster2spdf <- function(r){
  # r <- mask(r, boreal.shp)
  # r[is.na(r)] <- -999
  df <- as.data.frame(as(r, "SpatialPixelsDataFrame"))
  colnames(df) <- c("value", "x", "y")
  # df <- subset(df, value != -999)
  df
}

# LOAD SPATIAL DATA ===================================================================================================
boreal.shp <- readOGR(dsn = 'data/gis_data/wwf_boreal_biome_laea.shp')
land.45n.shp <-  readOGR(dsn = 'data/gis_data/land_45n_laea.shp')

ndvi.grn.1985.r <- raster('data/gis_data/ndvi_trends/boreal_lsat_ndvi_pcnt_greening_1985to2017.tif')
ndvi.brn.1985.r <- raster('data/gis_data/ndvi_trends/boreal_lsat_ndvi_pcnt_browning_1985to2017.tif')
ndvi.grn.2000.r <- raster('data/gis_data/ndvi_trends/boreal_lsat_ndvi_pcnt_greening_2000to2017.tif')
ndvi.brn.2000.r <- raster('data/gis_data/ndvi_trends/boreal_lsat_ndvi_pcnt_browning_2000to2017.tif')


# REDUCE RESOLUTION FOR PLOTTING (OTHERWISE ERRORS OUT FROM TOO LARGE FILES) ==========================================
ndvi.grn.1985.r <- raster::aggregate(ndvi.grn.1985.r, fact = c(10,10), fun = 'mean', na.rm=T)
ndvi.brn.1985.r <- raster::aggregate(ndvi.brn.1985.r, fact = c(10,10), fun = 'mean', na.rm=T)
ndvi.grn.2000.r <- raster::aggregate(ndvi.grn.2000.r, fact = c(10,10), fun = 'mean', na.rm=T)
ndvi.brn.2000.r <- raster::aggregate(ndvi.brn.2000.r, fact = c(10,10), fun = 'mean', na.rm=T)


# CONVERT RASTERS TO SPATIAL PIXEL DF FOR USE WITH GGPLOT =============================================================
# shapefiles must be converted to tidy()
boreal.tidy <- tidy(boreal.shp)
land.45n.tidy <- tidy(land.45n.shp)

# rasters must be converted to spatial pixel data frames. 
ndvi.grn.1985.df <- raster2spdf(ndvi.grn.1985.r)
ndvi.brn.1985.df <- raster2spdf(ndvi.brn.1985.r)
ndvi.grn.2000.df <- raster2spdf(ndvi.grn.2000.r)
ndvi.brn.2000.df <- raster2spdf(ndvi.brn.2000.r)


# BACKGROUND MAP ======================================================================================================
bg.map <- ggplot(land.45n.tidy, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", size = 0.1, fill = "lightgrey") +
  xlab("Longitude") + ylab("Latitude") +
  coord_equal() + theme_minimal() + 
  annotation_scale(location = "bl", width_hint = 0.5)


# CLIMATE TREND MAPS =================================================================================================

# % greening 1985-2017 -----------------------
ndvi.grn.1985.fig <- ggplot() +  
  geom_polygon(data=land.45n.tidy, aes(x = long, y = lat, group = group), color = "gray50", size = 0.1, fill = "gray30") +
  geom_polygon(data=boreal.tidy, aes(x = long, y = lat, group = group), color = NA, size = 0.1, fill = "black") +
  geom_raster(data=ndvi.grn.1985.df, aes(x=x, y=y, fill=value), alpha=1) + 
  geom_polygon(data=boreal.tidy, aes(x = long, y = lat, group = group), color = "black", size = 0.1, fill = NA) +
  scale_fill_gradient2(low = "white", mid = "green4", high = 'darkgreen', midpoint = 60, na.value = NA, guide = "colourbar") +
  labs(fill = 'Positive NDVI trend 1985-2017 (% of sampling sites)') + coord_equal() + theme_void() +
  theme(legend.position="bottom", legend.box="horizontal", legend.key.width=unit(2, "cm")) + 
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))

# ndvi.grn.1985.fig

ggsave('figures/lsat_ndvi_ecounit_trends_pcnt_greening_1985to2017.jpg', width = 5, height = 5, units = 'in', dpi = 400)

# % browning 1985-2017 -----------------------
ndvi.brn.1985.fig <- ggplot() +  
  geom_polygon(data=land.45n.tidy, aes(x = long, y = lat, group = group), color = "gray50", size = 0.1, fill = "gray30") +
  geom_polygon(data=boreal.tidy, aes(x = long, y = lat, group = group), color = NA, size = 0.1, fill = "black") +
  geom_raster(data=ndvi.brn.1985.df, aes(x=x, y=y, fill=value), alpha=1) + 
  geom_polygon(data=boreal.tidy, aes(x = long, y = lat, group = group), color = "black", size = 0.1, fill = NA) +
  scale_fill_gradient2(low = "white", mid = "firebrick4", high = 'darkred', midpoint = 60, na.value = NA, guide = "colourbar") +
  labs(fill = 'Negative NDVI trend 1985-2017 (% of sampling sites)') + coord_equal() + theme_void() +
  theme(legend.position="bottom", legend.box="horizontal", legend.key.width=unit(2, "cm")) + 
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))

# ndvi.brn.1985.fig  

ggsave('figures/lsat_ndvi_ecounit_trends_pcnt_browning_1985to2017.jpg', width = 5, height = 5, units = 'in', dpi = 400)

# multi-panel trend figures --------
figs.1985 <- ggarrange(ndvi.grn.1985.fig, ndvi.brn.1985.fig,
                  labels = c("(a)", "(b)"), label.x = 0.15, label.y = 0.95, ncol = 2, nrow = 1)
ggsave('figures/lsat_ndvi_ecounit_trends_pcnts_1985to2017_2panel.jpg', width = 9, height = 5, units = 'in', dpi = 400)


# % greening 2000-2017 -----------------------
ndvi.grn.2000.fig <- ggplot() +  
  geom_polygon(data=land.45n.tidy, aes(x = long, y = lat, group = group), color = "gray50", size = 0.1, fill = "gray30") +
  geom_polygon(data=boreal.tidy, aes(x = long, y = lat, group = group), color = NA, size = 0.1, fill = "black") +
  geom_raster(data=ndvi.grn.2000.df, aes(x=x, y=y, fill=value), alpha=1) + 
  geom_polygon(data=boreal.tidy, aes(x = long, y = lat, group = group), color = "black", size = 0.1, fill = NA) +
  scale_fill_gradient2(low = "white", mid = "green4", high = 'darkgreen', midpoint = 60, na.value = NA, guide = "colourbar") +
  labs(fill = 'Positive NDVI trend 2000-2017 (% of sampling sites)') + coord_equal() + theme_void() +
  theme(legend.position="bottom", legend.box="horizontal", legend.key.width=unit(2, "cm")) + 
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))

# ndvi.grn.2000.fig
ggsave('figures/lsat_ndvi_ecounit_trends_pcnt_greening_2000to2017.jpg', width = 5, height = 5, units = 'in', dpi = 400)

# % browning 2000-2017 -----------------------
ndvi.brn.2000.fig <- ggplot() +  
  geom_polygon(data=land.45n.tidy, aes(x = long, y = lat, group = group), color = "gray50", size = 0.1, fill = "gray30") +
  geom_polygon(data=boreal.tidy, aes(x = long, y = lat, group = group), color = NA, size = 0.1, fill = "black") +
  geom_raster(data=ndvi.brn.2000.df, aes(x=x, y=y, fill=value), alpha=1) + 
  geom_polygon(data=boreal.tidy, aes(x = long, y = lat, group = group), color = "black", size = 0.1, fill = NA) +
  scale_fill_gradient2(low = "white", mid = "firebrick4", high = 'darkred', midpoint = 60, na.value = NA, guide = "colourbar") +
  labs(fill = 'Negative NDVI trend 2000-2017 (% of sampling sites)') + coord_equal() + theme_void() +
  theme(legend.position="bottom", legend.box="horizontal", legend.key.width=unit(2, "cm")) + 
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))

# ndvi.brn.2000.fig  

ggsave('figures/lsat_ndvi_ecounit_trends_pcnt_browning_2000to2017.jpg', width = 5, height = 5, units = 'in', dpi = 400)


# multi-panel trend figures --------
figs.2000 <- ggarrange(ndvi.grn.2000.fig, ndvi.brn.2000.fig,
                       labels = c("(a)", "(b)"), label.x = 0.15, label.y = 0.95, ncol = 2, nrow = 1)
ggsave('figures/lsat_ndvi_ecounit_trends_pcnts_2000to2017_2panel.jpg', width = 9, height = 5, units = 'in', dpi = 400)


# multi-panel climate trend figures
figs <- ggarrange(ndvi.grn.1985.fig, ndvi.brn.1985.fig, ndvi.grn.2000.fig, ndvi.brn.2000.fig, 
                  labels = c("(a)", "(b)", "(c)", "(d)"), label.x = 0.15, label.y = 0.95, ncol = 2, nrow = 2)
ggsave('figures/lsat_ndvi_ecounit_trends_pcnts_4panel.jpg', width = 9, height = 9, units = 'in', dpi = 400)
