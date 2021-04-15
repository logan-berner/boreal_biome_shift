#-------------------------------------------------------------------------------------------------------------------
# SITES ON THE NORTH SLOPE OF ALASKA. 
# AUTHOR: LOGAN BERNER, NAU
# DATE: 2017-07-03
#-------------------------------------------------------------------------------------------------------------------
rm(list=ls())
require(dplyr)
require(tidyr)
require(data.table)
require(reshape2)
require(maptools)
require(raster)
require(sp)
require(R.utils)
require(tmap)

tmp.dir <- 'C:/tmp/R_tmp'
tempfile(tmpdir=tmp.dir)
rasterOptions(tmpdir=tmp.dir)

# setwd('C:/Users/lb968/Google Drive/research/nau/nsf_arctic/arctic_greening/')
setwd('C:/Users/Logan/Google Drive/research/nau/nasa_above/boreal_ndvi/')

# LOAD DATA SETS ====================================================================================
ndvi.trnd.site <- fread('output/boreal_site_lsat_ndvi_trends.csv', fill=T)

blank.r <- raster('data/gis_data/boreal_domain_grid_allna_laea_300m.tif')
ecounit.r <- raster('data/gis_data/ecological_land_unit_boreal_aoi_300m_laea.tif')
ecoreg.r <- raster('data/gis_data/wwf_boreal_ecoregs_laea_300m.tif')


# CONSOLIDATE TREND CATEGORIES =======================================================================
ndvi.trnd.site$trend.cat[ndvi.trnd.site$trend.cat == 'greening.sig.p5'] <- 'positive'
ndvi.trnd.site$trend.cat[ndvi.trnd.site$trend.cat == 'greening.sig.p10'] <- 'positive'
ndvi.trnd.site$trend.cat[ndvi.trnd.site$trend.cat == 'browning.sig.p5'] <- 'negative'
ndvi.trnd.site$trend.cat[ndvi.trnd.site$trend.cat == 'browning.sig.p10'] <- 'negative'
ndvi.trnd.site$trend.cat[ndvi.trnd.site$trend.cat == 'insig'] <- 'no trend'


# COMPUTE PERCENT OF SITES IN EACH ECOUNIT WITH TREND CATEGORY =======================================
# convert grouping variables to factors so that each combination gets a 'complete' summary (i.e., don't drop combos with 0 sites)
ndvi.trnd.site$trend.cat <- factor(ndvi.trnd.site$trend.cat)
ndvi.trnd.site$eco.num <- factor(ndvi.trnd.site$eco.num)
ndvi.trnd.site$ecounit <- factor(ndvi.trnd.site$ecounit)
ndvi.trnd.site$period <- factor(ndvi.trnd.site$period)

ndvi.trnd.ecounit.dt <- ndvi.trnd.site %>%
  filter(trend.cat != 'NA.NA', eco.num != '', ecounit != '') %>%
  group_by(period, eco.num, ecounit, trend.cat) %>% complete() %>% 
  summarise(n.sites = n()) %>% 
  group_by(period, eco.num, ecounit) %>%
  mutate(n.sites.eco = sum(n.sites), pcnt.sites = round(n.sites / n.sites.eco * 100,0)) %>%
  arrange(period, eco.num, ecounit, trend.cat) %>% filter(is.na(pcnt.sites) == F)

ndvi.trnd.ecounit.dt <- data.table(ndvi.trnd.ecounit.dt)

# change 'type' for later join of data tables 
ndvi.trnd.ecounit.dt$trend.cat <- as.character(ndvi.trnd.ecounit.dt$trend.cat)
ndvi.trnd.ecounit.dt$eco.num <- as.integer(as.character(ndvi.trnd.ecounit.dt$eco.num))
ndvi.trnd.ecounit.dt$ecounit <- as.integer(as.character(ndvi.trnd.ecounit.dt$ecounit))
ndvi.trnd.ecounit.dt$period <- as.character(ndvi.trnd.ecounit.dt$period)

ndvi.trnd.ecounit.dt
str(ndvi.trnd.ecounit.dt)

# CREATE DATA TABLE WITH ECOUNIT AND ECOREGION FOR EACH GRIDCELL =========================================
boreal.dt <- data.table(pxl.id = 1:ncell(blank.r), eco.num = values(ecoreg.r), ecounit = values(ecounit.r))
boreal.dt <- na.omit(boreal.dt)


# SPATIALIZE TRENDS FROM 1985 TO 2017 ====================================================================
# add ndvi trend summaries to each grid cell as data table
boreal.trends.1985.dt <- ndvi.trnd.ecounit.dt[period == '1985-2017']
boreal.trends.1985.dt <- boreal.dt[boreal.trends.1985.dt, on = c('eco.num','ecounit'), allow.cartesian=TRUE]

# number of sample sites
boreal.n.1985.r <- blank.r
boreal.n.1985.r[boreal.trends.1985.dt[trend.cat == 'positive', pxl.id]] <- boreal.trends.1985.dt[trend.cat == 'positive', n.sites.eco]
writeRaster(boreal.n.1985.r, 'data/gis_data/ndvi_trends/boreal_lsat_ndvi_pcnt_greening_1985to2017.tif', datatype='INT2U', overwrite=T)
rm(boreal.n.1985.r)

# greening
boreal.grn.1985.r <- blank.r
boreal.grn.1985.r[boreal.trends.1985.dt[trend.cat == 'positive', pxl.id]] <- boreal.trends.1985.dt[trend.cat == 'positive', pcnt.sites]
plot(boreal.grn.1985.r)
writeRaster(boreal.grn.1985.r, 'data/gis_data/ndvi_trends/boreal_lsat_ndvi_pcnt_greening_1985to2017.tif', datatype='INT1U', overwrite=T)
rm(boreal.grn.1985.r)

# browning
boreal.brn.1985.r <- blank.r
boreal.brn.1985.r[boreal.trends.1985.dt[trend.cat == 'negative', pxl.id]] <- boreal.trends.1985.dt[trend.cat == 'negative', pcnt.sites]
writeRaster(boreal.brn.1985.r, 'data/gis_data/ndvi_trends/boreal_lsat_ndvi_pcnt_browning_1985to2017.tif', datatype='INT1U', overwrite=T)
rm(boreal.brn.1985.r)

# no trend
boreal.notrend.1985.r <- blank.r
boreal.notrend.1985.r[boreal.trends.1985.dt[trend.cat == 'no trend', pxl.id]] <- boreal.trends.1985.dt[trend.cat == 'no trend', pcnt.sites]
writeRaster(boreal.notrend.1985.r, 'data/gis_data/ndvi_trends/boreal_lsat_ndvi_pcnt_notrend_1985to2017.tif', datatype='INT1U', overwrite=T)
rm(boreal.notrend.1985.r)


# SPATIALIZE TRENDS FROM 2000 TO 2017 ====================================================================
# add ndvi trend summaries to each grid cell as data table
boreal.trends.2000.dt <- ndvi.trnd.ecounit.dt[period == '2000-2017']
boreal.trends.2000.dt <- boreal.dt[boreal.trends.2000.dt, on = c('eco.num','ecounit'), allow.cartesian=TRUE]

# number of sample sites
boreal.n.2000.r <- blank.r
boreal.n.2000.r[boreal.trends.2000.dt[trend.cat == 'positive', pxl.id]] <- boreal.trends.2000.dt[trend.cat == 'positive', n.sites.eco]
writeRaster(boreal.n.2000.r, 'data/gis_data/ndvi_trends/boreal_lsat_ndvi_pcnt_greening_2000to2017.tif', datatype='INT2U', overwrite=T)
rm(boreal.n.2000.r)

# greening
boreal.grn.2000.r <- blank.r
boreal.grn.2000.r[boreal.trends.2000.dt[trend.cat == 'positive', pxl.id]] <- boreal.trends.2000.dt[trend.cat == 'positive', pcnt.sites]
plot(boreal.grn.2000.r)
writeRaster(boreal.grn.2000.r, 'data/gis_data/ndvi_trends/boreal_lsat_ndvi_pcnt_greening_2000to2017.tif', datatype='INT1U', overwrite=T)
rm(boreal.grn.2000.r)

# browning
boreal.brn.2000.r <- blank.r
boreal.brn.2000.r[boreal.trends.2000.dt[trend.cat == 'negative', pxl.id]] <- boreal.trends.2000.dt[trend.cat == 'negative', pcnt.sites]
writeRaster(boreal.brn.2000.r, 'data/gis_data/ndvi_trends/boreal_lsat_ndvi_pcnt_browning_2000to2017.tif', datatype='INT1U', overwrite=T)
rm(boreal.brn.2000.r)

# no trend
boreal.notrend.2000.r <- blank.r
boreal.notrend.2000.r[boreal.trends.2000.dt[trend.cat == 'no trend', pxl.id]] <- boreal.trends.2000.dt[trend.cat == 'no trend', pcnt.sites]
writeRaster(boreal.notrend.2000.r, 'data/gis_data/ndvi_trends/boreal_lsat_ndvi_pcnt_notrend_2000to2017.tif', datatype='INT1U', overwrite=T)
rm(boreal.notrend.2000.r)

# END SCRIPT ====================================================================