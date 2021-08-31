# This R script takes Landsat sampling sites from the boreal biome and extracts geospatial info for the sites
# Date: 2021-04-20

rm(list=ls())
require(data.table)
require(maptools)
require(raster)
require(rgdal)
require(caret)
require(dplyr)

wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
laea <- CRS("+proj=laea +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

setwd('/projects/arctic/users/lberner/boreal_biome_shift/')

# LOAD DATA SETS =====================================================================================
lsat.ts.dt <- fread('data/lsat_samples/boreal_lsat_clean_data_100k_sites_1985to2016.csv')

## load shapefiles
domain.shp <- readOGR(dsn = 'data/gis_data/wwf_boreal_biome_laea.shp')
ecoreg.shp <- readOGR(dsn = 'data/gis_data/wwf_boreal_ecoregs_laea.shp')

## load land cover rasters
lc.r <- raster('data/gis_data/landcov/esa_cci_landcov_consol_classes_2018_300m_laea.tif')
ecounit.r <- raster('data/gis_data/ecological_land_unit_boreal_300m_laea.tif')
treecov.r <- raster('data/gis_data/landcov/modis_treecov_median_2017to2019_boreal_laea_300m.tif')
treecov.gfcc.r <- raster('data/gis_data/landcov/gfcc_boreal_tree_cover_2015_30m_laea.tif')

## load permafrost rasters
pf.magt.r <- raster('data/gis_data/soil/esa_globpermafrost_pf_magt_avg_degCx10_2000to2016_boreal_1km_laea.tif')

## load topo rasters
elev.r <- raster('data/gis_data/topo/gmted2010_boreal_elevation_m_300m_laea.tif')
slope.r <- raster('data/gis_data/topo/gmted2010_boreal_slope_deg_300m_laea.tif')
southness.r <- raster('data/gis_data/topo/gmted2010_boreal_southness_300m_laea.tif')
westness.r <- raster('data/gis_data/topo/gmted2010_boreal_westness_300m_laea.tif')

## soil nitrogen files
soil.n.files.df <- data.frame(file = list.files('data/gis_data/soil/', pattern = 'soil', full.names = T),
                              col.name = list.files('data/gis_data/soil/', pattern = 'soil'))

soil.n.files.df$col.name <- substr(soil.n.files.df$col.name, 21,39) # fix a few column names
soil.n.files.df$col.name <- gsub('100to200cm_g','100to200cm_gkg', soil.n.files.df$col.name)  # fix a few column names
soil.n.files.df$col.name <- gsub('60to100cm_gk','60to100cm_gkg', soil.n.files.df$col.name)  # fix a few column names
soil.n.files.df$col.name <- gsub('_','.',soil.n.files.df$col.name)

## clim norms 
swi.avg.r <- raster('data/gis_data/terra_clim/swi_norms/terraclim_boreal_swi_mean_2000to2019_Cx100_laea_4km.tif')
def.avg.r <- raster('data/gis_data/terra_clim/wateryear_norms/terraclim_boreal_def_mean_2000to2019_wy_sum_mm_laea_4km.tif')
soil.avg.r <- raster('data/gis_data/terra_clim/seasonal_norms/terraclim_boreal_soil_mean_2000to2019_jja_mean_mm_laea_4km.tif')
vpd.avg.r <- raster('data/gis_data/terra_clim/seasonal_norms/terraclim_boreal_vpd_mean_2000to2019_jja_max_kPax100_laea_4km.tif')

## clim norms 
swi.anom.r <- raster('data/gis_data/terra_clim/swi_anoms/terraclim_boreal_swi_anom_2000to2019_maxzscore_laea_4km.tif')
def.anom.r <- raster('data/gis_data/terra_clim/wateryear_anoms/terraclim_boreal_def_anom_2000to2019_wy_sum_maxzscore_laea_4km.tif')
soil.anom.r <- raster('data/gis_data/terra_clim/seasonal_anoms/terraclim_boreal_soil_anom_2000to2019_jja_mean_maxzscore_laea_4km.tif')
vpd.anom.r <-  raster('data/gis_data/terra_clim/seasonal_anoms/terraclim_boreal_vpd_anom_2000to2019_jja_max_maxzscore_laea_4km.tif')

## clim trends
swi.change.r <- raster('data/gis_data/terra_clim/swi_trends/terraclim_boreal_swi_trend_2000to2019_Cx100_laea_4km.tif', layer = 2)
def.change.r <- raster('data/gis_data/terra_clim/wateryear_trends/terraclim_boreal_def_trend_2000to2019_wy_sum_mm_laea_4km.tif', layer = 2)
soil.change.r <- raster('data/gis_data/terra_clim/seasonal_trends/terraclim_boreal_soil_trend_2000to2019_jja_mean_mm_laea_4km.tif', layer = 2)
vpd.change.r <-  raster('data/gis_data/terra_clim/seasonal_trends/terraclim_boreal_vpd_trend_2000to2019_jja_max_kPax100_laea_4km.tif', layer = 2)

# SPATIALIZE SITES ===========================================================================
site.dt <- lsat.ts.dt[, .(lat = mean(latitude, na.rm=T), lon = mean(longitude, na.rm=T)), by = site]
pts.wgs84 <- SpatialPoints(coords = site.dt[,c(3,2)], proj4string = wgs84)
pts.laea <- spTransform(pts.wgs84, CRSobj = laea)

## double check alignment
plot(domain.shp, col = 'black')
points(pts.laea, col = 'red', pch = '*', cex = 0.1)


# EXTRACT GEOSPATIAL DATA FOR SITES =====================================================================

## ecoregion
site.ecoreg <- data.table(site = site.dt$site, over(pts.laea, ecoreg.shp))
site.ecoreg <- site.ecoreg[, c('site','ECO_NAME','ECO_NUM'), with=FALSE] # grab select cols
colnames(site.ecoreg) <- gsub("_", ".", tolower(colnames(site.ecoreg)))
site.dt <- site.dt[site.ecoreg, on = 'site']

## esa land cover
lc.key <- read.csv('data/gis_data/landcov/ESACCI-LC-Legend.csv', header = T)
site.lc <- data.table(site = site.dt$site, landcov.code = raster::extract(lc.r, pts.laea))
site.lc$landcov.name <- lc.key$label[match(site.lc$landcov.code, lc.key$rcls.val)]
site.dt <- site.dt[site.lc, on = 'site']

## ecounit
site.dt$ecounit <- raster::extract(ecounit.r, pts.laea)

## tree cover 
site.dt$treecov <- raster::extract(treecov.r, pts.laea)
# site.dt$treecov.gfcc <- raster::extract(treecov.gfcc.r, pts.laea)

## permafrost
site.dt$magt.degC <- raster::extract(pf.magt.r, pts.laea)/10

## topo
site.dt$elev.m <- raster::extract(elev.r, pts.laea)
site.dt$slope.deg <- raster::extract(slope.r, pts.laea)
site.dt$southness <- raster::extract(southness.r, pts.laea)
site.dt$westness <- raster::extract(westness.r, pts.laea)

## extract soil nitrogen
for (j in 1:nrow(soil.n.files.df)){
  r <- raster(as.character(soil.n.files.df$file[j]))
  site.dt[, soil.n.files.df$col.name[j] := raster::extract(r, pts.laea)]
  print(j/nrow(soil.n.files.df))
}


# climate norms
site.dt[, swi.avg.degC := raster::extract(swi.avg.r, pts.laea) / 100]
site.dt[, def.avg.cm := raster::extract(def.avg.r, pts.laea) / 10]
site.dt[, soil.avg.cm := raster::extract(soil.avg.r, pts.laea) / 10]
site.dt[, vpd.avg.kPa := raster::extract(vpd.avg.r, pts.laea) / 100]


## extract climate anomalies
site.dt[, swi.anom.zscore := raster::extract(swi.anom.r, pts.laea)]
site.dt[, def.anom.zscore := raster::extract(def.anom.r, pts.laea)]
site.dt[, soil.anom.zscore := raster::extract(soil.anom.r, pts.laea)]
site.dt[, vpd.anom.zscore := raster::extract(vpd.anom.r, pts.laea)]


## extract climate trends
site.dt[, swi.change.degC := raster::extract(swi.change.r, pts.laea) / 100]
site.dt[, def.change.cm := raster::extract(def.change.r, pts.laea) / 10]
site.dt[, soil.change.cm := raster::extract(soil.change.r, pts.laea) / 10]
site.dt[, vpd.change.kPa := raster::extract(vpd.change.r, pts.laea) / 100]


# REMOVE NUMERICAL VARIABLES WITH NEAR ZERO VARIANCE =======================================

# set characters to factors and separate factor and numeric columns
site.dt <- site.dt %>% mutate_if(is.character,as.factor) %>% as.data.table()
site.fac.dt <- site.dt[, sapply(site.dt, is.numeric)==F, with=F]
site.num.dt <- site.dt[, sapply(site.dt, is.numeric), with=F]
cors.df <- cor(site.num.dt, use = 'pair', method = 'pearson')

# remove deep soil nitrogen layers that are highly correlated with soil nitrogen at 30 - 60 cm depth (r=0.83-0.93)
site.dt <- site.dt[, c('soil.n.30to60cm.gkg', 'soil.n.60to100cm.gkg', 'soil.n.100to200cm.gkg') := NULL]

# WRITE OUT FILE =========================================================================
fwrite(site.dt, 'output/boreal_sample_site_climate_and_landcover.csv')
# site.dt <- fread('output/boreal_sample_site_climate_and_landcover.csv')
print("All done!!")

# END SCRIPT==============================================================================