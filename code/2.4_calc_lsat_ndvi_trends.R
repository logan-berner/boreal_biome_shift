# ABOUT THIS SCRIPT  ==============================================================================================================
# This R script computes temporal trends in NDVI time series for sites and zones in the Arctic. 
# AUTHOR: LOGAN BERNER, NAU
# DATE: 2020-02-21

# SET UP WORKSPACE ==============================================================================================================
rm(list=ls())
.libPaths(c(.libPaths(), "~/R/", '/home/lb968/.conda/envs/rstudio_env/lib/R/library'))
library(dplyr)
require(tidyr)
library(data.table)
library(reshape2)
library(zyp)
library(maptools)
library(raster)
library(rgdal)
library(sp)
library(R.utils)

args <- commandArgs(TRUE)
# i = as.numeric(args[1])
i = 1

setwd('/projects/arctic/users/lberner/boreal_biome_shift/')

# WRAPPER FUNCTIONS FOR COMUPTING AND SUMMARIZING TRENDS IN NDVI ACROSS SITES ==================================================
calc.trends <- function(x,y){
  xx <- zyp.yuepilon(y,x) ## note the order of x and y are switched in this call!!!
  return(data.table(slope=round(xx['trend'],5), int=round(xx['intercept'],5), tau=round(xx['tau'],3), pval=round(xx['sig'],4)))
}


# READ IN LANDSAT VI TIME SERIES  ==============================================================================================================
print('start loading data...')  

site.dt <- fread('output/boreal_sample_site_climate_and_landcover.csv')

ndvi.ts.files <- list.files('output/ndvi_timeseries/', full.names = T)
ndvi.ts <- fread(ndvi.ts.files[i], fill=T)
setnames(ndvi.ts, 'ndvi.avg','ndvi')
length(unique(ndvi.ts$site))

# reformat i for sorting
if (i < 10){
  i <- paste0('000',i)
} else if (i < 100){
  i <- paste0('00',i)
} else if (i < 1000){
  i <- paste0('0',i)
}

# subset sites with obs from 1985 - 2016
ndvi.ts.gte1985 <- ndvi.ts[first.yr <= 1986]
ndvi.ts.gte1985$period <- '1985-2017'
ndvi.ts.gte1985$year.rsc <- ndvi.ts.gte1985$year - 1985
length(unique(ndvi.ts.gte1985$site))

# subset sites with obs from 2000 - 2016
ndvi.ts.gte2000 <- ndvi.ts[year >= 2000][first.yr <= 2001]
ndvi.ts.gte2000$period <- '2000-2017'
ndvi.ts.gte2000$year.rsc <- ndvi.ts.gte2000$year - 2000
length(unique(ndvi.ts.gte2000$site))

# combine sites for both time periods
ndvi.ts.periods <- data.table(rbind(ndvi.ts.gte1985, ndvi.ts.gte2000))

# add site charatceristis to NDVI time series data
ndvi.ts.periods <- site.dt[ndvi.ts.periods, on = 'site']


# COMPUTE MANN-KENDALL TREND FOR EACH SITE ===========================================================================================
print('start computing NDVI trends for each site...')  
ndvi.trnd.site <- ndvi.ts.periods %>% group_by(period, site) %>% do(out=calc.trends(x=.$year.rsc, y=.$ndvi)) %>% unnest(cols=c(out)) %>% data.table()

# categorize trends 
ndvi.trnd.site <- ndvi.trnd.site[is.na(slope)==F]
ndvi.trnd.site[period == '1985-2017', n.yrs := length(1985:2017)][period == '2000-2017', n.yrs := length(2000:2017)]
ndvi.trnd.site[, total.change := slope * n.yrs]
ndvi.trnd.site[, total.change.pcnt := total.change / int * 100] # total percent increase relative to 1984 
ndvi.trnd.site[, sig :=  cut(pval, c(-Inf, 0.0500, 0.1000, Inf), c('sig.p5','sig.p10','insig'))]
ndvi.trnd.site[, slope.cat := cut(slope, c(-Inf, 0, Inf), c('browning','greening'))]
ndvi.trnd.site[, trend.cat := paste(slope.cat, sig, sep = '.')]
ndvi.trnd.site[trend.cat == 'greening.insig' | trend.cat == 'browning.insig', trend.cat := 'insig']

# add spatial details to trends
ndvi.trnd.site <- site.dt[ndvi.trnd.site, on = 'site']

# write out trend for each site during each period
mkdirs('output/lsat_site_trends/mc_reps')
ndvi.trnd.site$rep <- i
fwrite(ndvi.trnd.site, paste0('output/lsat_site_trends/mc_reps/Landsat_NDVIja_boreal_site_trends_rep_',i,'.csv'))

print('finished site trends...')  

# COMPUTE ANNUAL MEAN NDVI AND LINEAR TREND IN MEAN NDVI FOR EACH ECOUNIT ====================================
print('start computing average NDVI trends for each ecounit')  
lsat.ndvi.ecounit.ts <- ndvi.ts.periods %>% group_by(period, ecounit, year, year.rsc) %>%
  summarise(ndvi.avg=mean(ndvi, na.rm = T), ndvi.sd=sd(ndvi, na.rm = T), n.sites = n())

lsat.ndvi.ecounit.trends <- lsat.ndvi.ecounit.ts %>% group_by(ecounit, period) %>% 
  do(out=calc.trends(x=.$year.rsc, y=.$ndvi.avg)) %>% unnest(cols=c(out)) %>% data.table()

lsat.ndvi.ecounit.trends[period=='1985-2017', n.yrs := 33]
lsat.ndvi.ecounit.trends[period=='2000-2017', n.yrs := 18]

lsat.ndvi.ecounit.trends[, delta.ndvi := slope * n.yrs]
lsat.ndvi.ecounit.trends[, delta.ndvi.pcnt := as.numeric(delta.ndvi / int * 100), by = c('ecounit')]

nrow(lsat.ndvi.ecounit.trends[pval <= 0.10 & slope > 0]) / nrow(lsat.ndvi.ecounit.trends)

# hist(lsat.ndvi.ecounit.trends$delta.ndvi, 200)
# hist(lsat.ndvi.ecounit.trends$delta.ndvi.pcnt, 200)
# abline(v=0, col='blue')
# lsat.ndvi.ecounit.trends[pval <= 0.10]

# COMPUTE % OF SAMPLING SITES IN EACH ECOUNIT THAT GREENED AND BROWNED ==============================================================
ndvi.trnd.site.cat.pcnt.by.ecounit <- ndvi.trnd.site[ , .(n.sites = .N), by=c('period','ecounit','trend.cat')]
ndvi.trnd.site.cat.pcnt.by.ecounit <- ndvi.trnd.site.cat.pcnt.by.ecounit[, n.sites.ecounit := sum(n.sites), by=c('period','ecounit')]
ndvi.trnd.site.cat.pcnt.by.ecounit <- ndvi.trnd.site.cat.pcnt.by.ecounit[, pcnt.sites := round(n.sites / n.sites.ecounit * 100)]
ndvi.trnd.site.cat.pcnt.by.ecounit <- ndvi.trnd.site.cat.pcnt.by.ecounit[, trend.cat := factor(trend.cat, 
                                                                                               levels = c('browning.sig.p5','browning.sig.p10','insig','greening.sig.p10','greening.sig.p5'),
                                                                                               labels = c('browning','browning','no trend','greening','greening'))]

ndvi.trnd.site.cat.pcnt.by.ecounit <- ndvi.trnd.site.cat.pcnt.by.ecounit[order(ecounit, trend.cat)]
ndvi.trnd.site.cat.pcnt.by.ecounit <- ndvi.trnd.site.cat.pcnt.by.ecounit[n.sites.ecounit >= 10]
setorder(ndvi.trnd.site.cat.pcnt.by.ecounit, period, ecounit, trend.cat)
         
ndvi.trnd.site.cat.pcnt.by.ecounit.wide <- dcast.data.table(data = ndvi.trnd.site.cat.pcnt.by.ecounit, formula = ecounit ~ trend.cat + period, value.var = 'pcnt.sites')

ndvi.trnd.site.cat.pcnt.by.ecounit.wide[is.na(ndvi.trnd.site.cat.pcnt.by.ecounit.wide)] <- 0

#-------------------------------------------------------------------------------------------------------------------
# SPATIALIZE TRENDS
#-------------------------------------------------------------------------------------------------------------------
# arctic.r[arctic.r == 0] <- 1
mkdirs('data/gis_data')
arctic.r[] <- NA
arctic.pxl.dt <- data.table(cellid = 1:ncell(arctic.r), ecounit = values(ecounit.r))
arctic.pxl.dt <- na.omit(arctic.pxl.dt)

# ECOUNIT PERCENTAGE GREENING / BROWNING  -----------------------------------------------------------
ecounit.pcnt.dt <- arctic.pxl.dt[ndvi.trnd.site.cat.pcnt.by.ecounit.wide, on = 'ecounit']

# % sites greening
greening.pcnt.r <- arctic.r
greening.pcnt.r[ecounit.pcnt.dt$cellid] <- round(ecounit.pcnt.dt$greening)
writeRaster(greening.pcnt.r, 'data/gis_data/arctic_lsat_ndvi_ecounit_pcnt_greening_2000to2020.tif', datatype = 'INT1U', overwrite=T)
rm(greening.pcnt.r)

# % sites browning 
browning.pcnt.r <- arctic.r
browning.pcnt.r[ecounit.pcnt.dt$cellid] <- round(ecounit.pcnt.dt$browning)
writeRaster(browning.pcnt.r, 'data/gis_data/arctic_lsat_ndvi_ecounit_pcnt_browning_2000to2020.tif', datatype = 'INT1U', overwrite=T)
rm(browning.pcnt.r)


# ECOUNIT AVERAGE CHANGE -----------------------------------------------------------
arctic.trends.2000.dt <- arctic.pxl.dt[lsat.ndvi.ecounit.trends, on = 'ecounit']
arctic.trends.2000.dt <- na.omit(arctic.trends.2000.dt)
rm(arctic.pxl.dt)

# pval
pval.2000.slp.r <- arctic.r
pval.2000.slp.r[arctic.trends.2000.dt$cellid] <- round(arctic.trends.2000.dt$pval*1000)
writeRaster(pval.2000.slp.r, 'data/gis_data/arctic_lsat_ndvi_ecounit_trend_pval_x1000_2000to2020.tif', datatype = 'INT2U', overwrite=T)
rm(pval.2000.slp.r)

# slope
trends.2000.slp.r <- arctic.r
trends.2000.slp.r[arctic.trends.2000.dt$cellid] <- round(arctic.trends.2000.dt$slope*10000)
writeRaster(trends.2000.slp.r, 'data/gis_data/arctic_lsat_ndvi_ecounit_trend_slope_x10000_2000to2020.tif', datatype = 'INT2S', overwrite=T)
rm(trends.2000.slp.r)

# NDVI change
trends.2000.chng.r <- arctic.r
trends.2000.chng.r[arctic.trends.2000.dt$cellid] <- round(arctic.trends.2000.dt$delta.ndvi*10000)
writeRaster(trends.2000.chng.r, 'data/gis_data/arctic_lsat_ndvi_ecounit_total_change_ndvi_x10000_2000to2020.tif', datatype = 'INT2S', overwrite=T)
rm(trends.2000.chng.r)

# NDVI change as pcnt
trends.2000.chng.pcnt.r <- arctic.r
trends.2000.chng.pcnt.r[arctic.trends.2000.dt$cellid] <- round(arctic.trends.2000.dt$delta.ndvi.pcnt*100)
writeRaster(trends.2000.chng.pcnt.r, 'data/gis_data/arctic_lsat_ndvi_ecounit_pcnt_change_ndvi_x100_2000to2020.tif', datatype = 'INT2S', overwrite=T)
rm(trends.2000.chng.pcnt.r)


# END SCRIPT ==================================================================================================




# # COMPUTE PROPORTION OF SITES IN EACH BIOCLIMATIC ZONE THAT HAVE SPECIFIC TREND CATEGORIES ===============================================
# 
# # zones
# ndvi.trnd.site.cat.pcnt.by.zone <- ndvi.trnd.site[trend.cat != 'NA.NA'][lat.zone != ''][, .(n.sites = .N), by=c('period', 'lat.zone', 'trend.cat')]
# ndvi.trnd.site.cat.pcnt.by.zone <- ndvi.trnd.site.cat.pcnt.by.zone[, n.sites.zone := sum(n.sites), by = c('period','lat.zone')]
# ndvi.trnd.site.cat.pcnt.by.zone <- ndvi.trnd.site.cat.pcnt.by.zone[, pcnt.sites := n.sites / n.sites.zone * 100]
# ndvi.trnd.site.cat.pcnt.by.zone <- ndvi.trnd.site.cat.pcnt.by.zone[, trend.cat := factor(trend.cat, 
#                                                                                          levels = c('browning.sig.p5','browning.sig.p10','insig','greening.sig.p10','greening.sig.p5'),
#                                                                                          labels = c('browning (p<0.05)','browning (p<0.10)','no trend','greening (p<0.10)','greening (p<0.05)'))]
# 
# ndvi.trnd.site.cat.pcnt.by.zone <- ndvi.trnd.site.cat.pcnt.by.zone[order(period,lat.zone,trend.cat)]
# ndvi.trnd.site.cat.pcnt.by.zone <- ndvi.trnd.site.cat.pcnt.by.zone[, pcnt.position := cumsum(pcnt.sites)-pcnt.sites/2, by = c('period','lat.zone')]
# ndvi.trnd.site.cat.pcnt.by.zone <- ndvi.trnd.site.cat.pcnt.by.zone[, cnt.position := cumsum(n.sites)-n.sites/2, by = c('period','lat.zone')]
# 
# # biome
# ndvi.trnd.site.cat.pcnt.by.biome <- ndvi.trnd.site[trend.cat != 'NA.NA'][lat.zone != ''][, .(n.sites = .N), by=c('period', 'trend.cat')]
# ndvi.trnd.site.cat.pcnt.by.biome <- ndvi.trnd.site.cat.pcnt.by.biome[, n.sites.zone := sum(n.sites), by = c('period')]
# ndvi.trnd.site.cat.pcnt.by.biome <- ndvi.trnd.site.cat.pcnt.by.biome[, pcnt.sites := n.sites / n.sites.zone * 100]
# ndvi.trnd.site.cat.pcnt.by.biome <- ndvi.trnd.site.cat.pcnt.by.biome[, trend.cat := factor(trend.cat, 
#                                                                                            levels = c('browning.sig.p5','browning.sig.p10','insig','greening.sig.p10','greening.sig.p5'),
#                                                                                            labels = c('browning (p<0.05)','browning (p<0.10)','no trend','greening (p<0.10)','greening (p<0.05)'))]
# 
# ndvi.trnd.site.cat.pcnt.by.biome <- ndvi.trnd.site.cat.pcnt.by.biome[order(period,trend.cat)]
# ndvi.trnd.site.cat.pcnt.by.biome <- ndvi.trnd.site.cat.pcnt.by.biome[, pcnt.position := cumsum(pcnt.sites)-pcnt.sites/2, by = c('period')]
# ndvi.trnd.site.cat.pcnt.by.biome <- ndvi.trnd.site.cat.pcnt.by.biome[, cnt.position := cumsum(n.sites)-n.sites/2, by = c('period')]
# ndvi.trnd.site.cat.pcnt.by.biome <- ndvi.trnd.site.cat.pcnt.by.biome[, lat.zone := 'Arctic']
# 
# #combine zones and biome
# ndvi.trnd.site.cat.pcnt.by.zone <- rbind(ndvi.trnd.site.cat.pcnt.by.zone, ndvi.trnd.site.cat.pcnt.by.biome)
# 
# # add MC rep
# ndvi.trnd.site.cat.pcnt.by.zone$rep <- i
# 
# # write out
# mkdirs('output/lsat_zonal_freq_trends/mc_reps')
# fwrite(ndvi.trnd.site.cat.pcnt.by.zone, paste0('output/lsat_zonal_freq_trends/mc_reps/Landsat_NDVImax_tundra_zone_pcnt_trend_category_rep_',i,'.csv'))
# 
# print('finished freq of site trends...')  


# # GRID SITES - LEVEL TRENDS ================================================================================================
# # spatalize sites and write out as shapefile
# wgs84 <- CRS("+proj=longlat +datum=WGS84")
# laea <- CRS("+proj=laea +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
# 
# ndvi.trnd.site.pts.wgs <- SpatialPoints(coords = cbind(ndvi.trnd.site$longitude, ndvi.trnd.site$latitude), proj4string = wgs84)
# ndvi.trnd.site.pts.laea <- spTransform(ndvi.trnd.site.pts.wgs, CRSobj = laea)
# 
# arctic.zones.r <- raster('data/gis_data/arctic_zones/arctic_oroarctic_lat_zones_laea_50km.tif')
# 
# arctic.tmplt.r <- arctic.zones.r
# arctic.tmplt.r[arctic.tmplt.r >= 0] <- NA
# 
# arctic.pxl.id.r <- arctic.zones.r
# arctic.pxl.id.r[] <- 1:ncell(arctic.zones.r) 
# 
# site.pxl.id <- data.table(site = ndvi.trnd.site$site, pxl.id = raster::extract(arctic.pxl.id.r, ndvi.trnd.site.pts.laea))
# 
# ndvi.trnd.site$pxl.id <- site.pxl.id$pxl.id[match(site.pxl.id$site, ndvi.trnd.site$site)]
# 
# 
# # NUMBER OF SITES
# ndvi.site.cnt.pxl <- ndvi.trnd.site[, .(n.sites = .N), by=c('period', 'pxl.id')]
# 
# ndvi.site.cnt.pxl.gte1985 <- ndvi.site.cnt.pxl[period == '1985-2016']
# ndvi.site.cnt.pxl.gte2000 <- ndvi.site.cnt.pxl[period == '2000-2016']
# 
# arctic.site.cnt.gte1985.r <- arctic.tmplt.r
# arctic.site.cnt.gte1985.r[ndvi.site.cnt.pxl.gte1985$pxl.id] <- ndvi.site.cnt.pxl.gte1985$n.sites
# 
# arctic.site.cnt.gte2000.r <- arctic.tmplt.r
# arctic.site.cnt.gte2000.r[ndvi.site.cnt.pxl.gte2000$pxl.id] <- ndvi.site.cnt.pxl.gte2000$n.sites
# 
# # GREENING
# ndvi.trnd.grn.pxl <- ndvi.trnd.site[, n.sites := .N, by=c('period', 'pxl.id')]
# ndvi.trnd.grn.pxl <- ndvi.trnd.site[pval <= 0.10 & slope.cat == 'greening', .(n.sites = first(n.sites), n.sites.grn = .N), by=c('period', 'pxl.id')]
# ndvi.trnd.grn.pxl <- ndvi.trnd.grn.pxl[, pcnt.sites.grn := n.sites.grn / n.sites * 100]
# 
# ndvi.trnd.grn.pxl.gte1985 <- ndvi.trnd.grn.pxl[period == '1985-2016']
# ndvi.trnd.grn.pxl.gte2000 <- ndvi.trnd.grn.pxl[period == '2000-2016']
# 
# arctic.grn.gte1985.r <- arctic.tmplt.r
# arctic.grn.gte1985.r[arctic.site.cnt.gte1985.r > 0] <- 0
# arctic.grn.gte1985.r[ndvi.trnd.grn.pxl.gte1985$pxl.id] <- ndvi.trnd.grn.pxl.gte1985$pcnt.sites.grn 
# 
# arctic.grn.gte2000.r <- arctic.tmplt.r
# arctic.grn.gte2000.r[arctic.site.cnt.gte2000.r > 0] <- 0
# arctic.grn.gte2000.r[ndvi.trnd.grn.pxl.gte2000$pxl.id] <- ndvi.trnd.grn.pxl.gte2000$pcnt.sites.grn 
# 
# 
# # BROWNING
# ndvi.trnd.brn.pxl <- ndvi.trnd.site[, n.sites := .N, by=c('period', 'pxl.id')]
# ndvi.trnd.brn.pxl <- ndvi.trnd.site[pval <= 0.10 & slope.cat == 'browning', .(n.sites = first(n.sites), n.sites.brn = .N), by=c('period', 'pxl.id')]
# ndvi.trnd.brn.pxl <- ndvi.trnd.brn.pxl[, pcnt.sites.brn := n.sites.brn / n.sites * 100]
# 
# ndvi.trnd.brn.pxl.gte1985 <- ndvi.trnd.brn.pxl[period == '1985-2016']
# ndvi.trnd.brn.pxl.gte2000 <- ndvi.trnd.brn.pxl[period == '2000-2016']
# 
# arctic.brn.gte1985.r <- arctic.tmplt.r
# arctic.brn.gte1985.r[arctic.site.cnt.gte1985.r > 0] <- 0
# arctic.brn.gte1985.r[ndvi.trnd.brn.pxl.gte1985$pxl.id] <- ndvi.trnd.brn.pxl.gte1985$pcnt.sites.brn 
# 
# arctic.brn.gte2000.r <- arctic.tmplt.r
# arctic.brn.gte2000.r[arctic.site.cnt.gte2000.r > 0] <- 0
# arctic.brn.gte2000.r[ndvi.trnd.brn.pxl.gte2000$pxl.id] <- ndvi.trnd.brn.pxl.gte2000$pcnt.sites.brn 
# 
# # plot(arctic.brn.gte1985.r)
# # plot(arctic.brn.gte2000.r)
# 
# # WRITE OUT RASTERS
# mkdirs('output/lsat_gridded_trends/mc_reps')
# writeRaster(arctic.site.cnt.gte1985.r, paste0('output/lsat_gridded_trends/mc_reps/arctic_lsat_ndvi_site_cnt_1985to2016_rep_',i,'.tif'), overwrite=T)
# writeRaster(arctic.site.cnt.gte2000.r, paste0('output/lsat_gridded_trends/mc_reps/arctic_lsat_ndvi_site_cnt_2000to2016_rep_',i,'.tif'), overwrite=T)
# 
# writeRaster(arctic.grn.gte1985.r, paste0('output/lsat_gridded_trends/mc_reps/arctic_lsat_ndvi_pcnt_greening_p10_1985to2016_rep_',i,'.tif'), overwrite=T)
# writeRaster(arctic.grn.gte2000.r, paste0('output/lsat_gridded_trends/mc_reps/arctic_lsat_ndvi_pcnt_greening_p10_2000to2016_rep_',i,'.tif'), overwrite=T)
# 
# writeRaster(arctic.brn.gte1985.r, paste0('output/lsat_gridded_trends/mc_reps/arctic_lsat_ndvi_pcnt_browning_p10_1985to2016_rep_',i,'.tif'), overwrite=T)
# writeRaster(arctic.brn.gte2000.r, paste0('output/lsat_gridded_trends/mc_reps/arctic_lsat_ndvi_pcnt_browning_p10_2000to2016_rep_',i,'.tif'), overwrite=T)
# 
# print('finished gridded trends... All done!!')  

# # COMPUTE TRENDS IN BIOME-WIDE AND SUBZONE-WIDE MEAN LANDSAT NDVI ====================================================================
# ndvi.ts.periods <- data.table(ndvi.ts.periods)
# ndvi.ts.periods <- ndvi.ts.periods[, ':='(ndvi.avg = mean(ndvi, na.rm=T), ndvi.sd = sd(ndvi, na.rm=T)), by = c('period', 'site')]
# ndvi.ts.periods <- ndvi.ts.periods[, ndvi.anom := ndvi - ndvi.avg]
# ndvi.ts.periods <- ndvi.ts.periods[, ndvi.zanom := ndvi.anom / ndvi.sd]
# 
# ndvi.ts.biome.avg <- ndvi.ts.periods[, .(ndvi.avg=mean(ndvi, na.rm = T), ndvi.sd=sd(ndvi, na.rm = T) , n.sites = .N,
#                                          ndvi.anom.avg=mean(ndvi.anom, na.rm = T), ndvi.anom.sd=sd(ndvi.anom, na.rm = T),
#                                          ndvi.zanom.avg=mean(ndvi.zanom, na.rm = T), ndvi.zanom.sd=sd(ndvi.zanom, na.rm = T),
#                                          lat.zone = 'Arctic'), 
#                                      by = c('period', 'year')]
# 
# ndvi.ts.zone.avg <- ndvi.ts.periods[lat.zone != '', .(ndvi.avg=mean(ndvi, na.rm = T), ndvi.sd=sd(ndvi, na.rm = T) , n.sites = .N,
#                                                       ndvi.anom.avg=mean(ndvi.anom, na.rm = T), ndvi.anom.sd=sd(ndvi.anom, na.rm = T),
#                                                       ndvi.zanom.avg=mean(ndvi.zanom, na.rm = T), ndvi.zanom.sd=sd(ndvi.zanom, na.rm = T)),
#                                     by = c('period', 'lat.zone', 'year')]
# 
# # add 'biome' as a sub-zone
# ndvi.ts.zone.avg <- rbind(ndvi.ts.zone.avg, ndvi.ts.biome.avg) 
# ndvi.ts.zone.avg <- ndvi.ts.zone.avg[order(period,lat.zone,year)]
# 
# # rescale years
# ndvi.ts.zone.avg <- ndvi.ts.zone.avg[, year.rsc := (seq_along(year)-1), by=c('period', 'lat.zone')]
# 
# # compute number of sites for each subzone at each time step
# ndvi.ts.zone.nsites <- ndvi.ts.zone.avg[, .(n.sites.min = min(n.sites), n.sites.max = max(n.sites)), by=c('period', 'lat.zone')]
# 
# # trend 
# ndvi.trnd.zone.anom.smry <- ndvi.ts.zone.avg %>% group_by(period, lat.zone) %>% do(out=calc.trends(x=.$year.rsc, y=.$ndvi.anom.avg)) %>% unnest(cols=c(out)) %>% data.table()
# ndvi.trnd.zone.anom.smry <- ndvi.trnd.zone.anom.smry[, n.yrs :=  c(rep(32,4), rep(17,4))]
# ndvi.trnd.zone.anom.smry <- ndvi.ts.zone.nsites[ndvi.trnd.zone.anom.smry, on = c('period','lat.zone')]
# 
# ndvi.trnd.zone.avg.smry <-  ndvi.ts.zone.avg %>% group_by(period, lat.zone) %>% do(out=calc.trends(x=.$year.rsc, y=.$ndvi.avg)) %>% unnest(cols=c(out)) %>% data.table()
# ndvi.trnd.zone.avg.smry <- ndvi.trnd.zone.avg.smry[, n.yrs :=  c(rep(32,4), rep(17,4))]
# ndvi.trnd.zone.avg.smry <- ndvi.trnd.zone.avg.smry[, total.change := slope * n.yrs][, total.change.pcnt := total.change / int * 100]
# ndvi.trnd.zone.avg.smry <- ndvi.ts.zone.nsites[ndvi.trnd.zone.avg.smry, on = c('period','lat.zone')]
# 
# ndvi.trnd.zone.avg.smry <- ndvi.trnd.zone.avg.smry[lat.zone != '', lat.zone := factor(lat.zone, levels = c('High Arctic','Low Arctic','Oro Arctic','Arctic'))]
# ndvi.trnd.zone.avg.smry <- ndvi.trnd.zone.avg.smry[order(period, lat.zone)]
# ndvi.trnd.zone.avg.smry
# 
# ndvi.trnd.zone.avg.smry$rep <- i
# 
# # write out 
# mkdirs('output/lsat_zonal_avg_trends/mc_reps')
# mkdirs('output/lsat_zonal_avg_anom_trends/mc_reps')
# 
# fwrite(ndvi.trnd.zone.avg.smry, paste0('output/lsat_zonal_avg_trends/mc_reps/Landsat_NDVImax_mean_tundra_zone_trends_rep_',i,'.csv'))
# fwrite(ndvi.trnd.zone.anom.smry, paste0('output/lsat_zonal_avg_anom_trends/mc_reps/Landsat_NDVImax_mean_anom_tundra_zone_trends_rep_',i,'.csv'))
# 
# print('finished biome scale trends...')  
# 
# # END SCRIPT ================================================================================================================