# This R script takes Landsat sampling sites from the boreal biome and extracts geospatial info for the sites
# Date: 2021-04-20

rm(list=ls())
require(data.table)
require(maptools)
require(raster)
require(rgdal)

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

## load permafrost rasters
pf.prob.r <- raster('data/gis_data/permafrost/esa_globpermafrost_pf_prob_avg_2000to2016_boreal_1km_laea.tif')
pf.magt.r <- raster('data/gis_data/permafrost/esa_globpermafrost_pf_magt_avg_degCx10_2000to2016_boreal_1km_laea.tif')

## load topo rasters
elev.r <- raster('data/gis_data/topo/gmted2010_boreal_elevation_m_300m_laea.tif')
slope.r <- raster('data/gis_data/topo/gmted2010_boreal_slope_deg_300m_laea.tif')
southness.r <- raster('data/gis_data/topo/gmted2010_boreal_southness_300m_laea.tif')
westness.r <- raster('data/gis_data/topo/gmted2010_boreal_westness_300m_laea.tif')

## clim norm files
clim.norm.files.df <- data.frame(file = c(list.files('data/gis_data/terra_clim/seasonal_norms', pattern = glob2rx('*.tif'), full.names = T),
                                          list.files('data/gis_data/terra_clim/wateryear_norms', pattern = glob2rx('*.tif'), full.names = T)),
                                 col.name = c(list.files('data/gis_data/terra_clim/seasonal_norms', pattern = glob2rx('*.tif'), full.names = F),
                                          list.files('data/gis_data/terra_clim/wateryear_norms', pattern = glob2rx('*.tif'), full.names = F)))

clim.norm.files.df$col.name <- gsub('_','.', gsub('_laea_4km.tif','',gsub('terraclim_boreal_','', clim.norm.files.df$col.name)))

## clim anomaly files
clim.anom.files.df <- data.frame(file = c(list.files('data/gis_data/terra_clim/seasonal_anoms', pattern = glob2rx('*.tif'), full.names = T),
                                          list.files('data/gis_data/terra_clim/wateryear_anoms', pattern = glob2rx('*.tif'), full.names = T)),
                                 col.name = c(list.files('data/gis_data/terra_clim/seasonal_anoms', pattern = glob2rx('*.tif'), full.names = F),
                                              list.files('data/gis_data/terra_clim/wateryear_anoms', pattern = glob2rx('*.tif'), full.names = F)))

clim.anom.files.df$col.name <- gsub('_','.', gsub('_laea_4km.tif','',gsub('terraclim_boreal_','', clim.anom.files.df$col.name)))

## clim trend files
clim.trend.files.df <- data.frame(file = c(list.files('data/gis_data/terra_clim/seasonal_trends', pattern = glob2rx('*.tif'), full.names = T),
                                          list.files('data/gis_data/terra_clim/wateryear_trends', pattern = glob2rx('*.tif'), full.names = T)),
                                 col.name = c(list.files('data/gis_data/terra_clim/seasonal_trends', pattern = glob2rx('*.tif'), full.names = F),
                                              list.files('data/gis_data/terra_clim/wateryear_trends', pattern = glob2rx('*.tif'), full.names = F)))
clim.trend.files.df$col.name <- gsub('_','.', gsub('_laea_4km.tif','',gsub('terraclim_boreal_','', clim.trend.files.df$col.name)))
clim.trend.files.df$col.name.int <- gsub('to2019','',gsub('trend.','', clim.trend.files.df$col.name))


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

## modis tree cover 
site.dt$treecov <- raster::extract(treecov.r, pts.laea)

## permafrost
site.dt$pf.prob <- raster::extract(pf.prob.r, pts.laea)/100
site.dt$magt.degC <- raster::extract(pf.magt.r, pts.laea)/10

## topo
site.dt$elev.m <- raster::extract(elev.r, pts.laea)
site.dt$slope.deg <- raster::extract(slope.r, pts.laea)
site.dt$southness <- raster::extract(southness.r, pts.laea)
site.dt$westness <- raster::extract(westness.r, pts.laea)

## extract climate norms
for (j in 1:nrow(clim.norm.files.df)){
  r <- raster(as.character(clim.norm.files.df$file[j]))
  site.dt[, clim.norm.files.df$col.name[j] := raster::extract(r, pts.laea)]
  print(j/nrow(clim.norm.files.df))
}

## extract climate anomalies
for (j in 1:nrow(clim.anom.files.df)){
  r <- raster(as.character(clim.anom.files.df$file[j]))
  site.dt[, clim.anom.files.df$col.name[j] := raster::extract(r, pts.laea)]
  print(j/nrow(clim.anom.files.df))
}

## extract climate trends
for (j in 1:nrow(clim.trend.files.df)){
  chng.r <- raster(as.character(clim.trend.files.df$file[j]), layer = 2) # total change
  int.r <- raster(as.character(clim.trend.files.df$file[j]), layer = 3) # intercept of regression
  site.dt[, clim.trend.files.df$col.name[j] := raster::extract(chng.r, pts.laea)]
  site.dt[, clim.trend.files.df$col.name.int[j] := raster::extract(int.r, pts.laea)]
  print(j/nrow(clim.trend.files.df))
}


# WRITE OUT FILE =========================================================================
fwrite(site.dt, 'output/boreal_sample_site_climate_and_landcover.csv')

print("All done!!")
# END SCRIPT==============================================================================