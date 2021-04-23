# ABOUT THIS SCRIPT  ==============================================================================================================
# This R script computes temporal trends in vi time series for sites and zones in the boreal forest biome. 
# AUTHOR: LOGAN BERNER, NAU
# DATE: 2021-04-19

# SET UP WORKSPACE ==============================================================================================================
rm(list=ls())
.libPaths(c(.libPaths(), "~/R/", "/home/lb968/R/4.0.2/"))
require(data.table)
require(lsatTS)
library(raster)
library(R.utils)

args <- commandArgs(TRUE)
# i = as.numeric(args[1])
i = 1

setwd('/projects/arctic/users/lberner/boreal_biome_shift/')


# READ IN LANDSAT VI TIME SERIES  ==============================================================================================================
print('Loading data...')  

site.dt <- fread('output/boreal_sample_site_climate_and_landcover.csv')
lsat.vi.dt <- fread(list.files('output/lsat_vi_gs_site_timeseries/', full.names = T)[i])

# lsat.vi.dt <- lsat.vi.dt[sample(1:nrow(lsat.vi.dt), nrow(lsat.vi.dt)*0.25)]

boreal.r <- raster('data/gis_data/wwf_boreal_biome_laea_300m.tif')
ecounit.r <- raster('data/gis_data/ecological_land_unit_boreal_aoi_300m_laea.tif')     

# reformat i for sorting
if (i < 10){
  i <- paste0('000',i)
} else if (i < 100){
  i <- paste0('00',i)
} else if (i < 1000){
  i <- paste0('0',i)
}


# COMPUTE MANN-KENDALL TREND FOR EACH SITE ===========================================================================================
print('Computing vi trend for each site...')  

lsat.vi.site.trnd.1985.dt <- lsat_calc_trend(lsat.vi.dt, vi = 'vi.max', yrs = 1985:2019, yr.tolerance = 1, nyr.min.frac = 0.666, sig = 0.10)
lsat.vi.site.trnd.2000.dt <- lsat_calc_trend(lsat.vi.dt, vi = 'vi.max', yrs = 2000:2019, yr.tolerance = 1, nyr.min.frac = 0.666, sig = 0.10)
lsat.vi.site.trnd.dt <- rbind(lsat.vi.site.trnd.1985.dt, lsat.vi.site.trnd.2000.dt)

# specify which vi was used
lsat.vi.site.trnd.dt[, vi.name := unique(lsat.vi.dt$vi.name)]

# write out trend for each site during each period
mkdirs('output/lsat_vi_gs_site_trends/mc_reps')
lsat.vi.site.trnd.dt$rep <- i
fwrite(lsat.vi.site.trnd.dt, paste0('output/lsat_vi_gs_site_trends/mc_reps/boreal_lsat_vi_gs_site_trends_rep_',i,'.csv'))


# COMPUTE % OF SAMPLING SITES IN EACH ECOUNIT THAT GREENED AND BROWNED ==============================================================
print('Computing ecounit fractional trends...')  
mkdirs('output/lsat_vi_gs_ecounit_trends_frac/mc_reps_tabular')

# add ecounit to trends
lsat.vi.site.trnd.dt$ecounit <- site.dt$ecounit[match(lsat.vi.site.trnd.dt$site, site.dt$site)]
rm(site.dt)

# compute fractions ---------------------------------
lsat.vi.ecounit.frac.trnd.dt <- lsat.vi.site.trnd.dt[ , .(n.sites = .N), by=c('trend.period','ecounit','trend.cat')]
lsat.vi.ecounit.frac.trnd.dt <- lsat.vi.ecounit.frac.trnd.dt[, n.sites.ecounit := sum(n.sites), by=c('trend.period','ecounit')]
lsat.vi.ecounit.frac.trnd.dt <- lsat.vi.ecounit.frac.trnd.dt[, pcnt.sites := round(n.sites / n.sites.ecounit * 100)]
lsat.vi.ecounit.frac.trnd.dt <- lsat.vi.ecounit.frac.trnd.dt[, trend.cat := factor(trend.cat, levels = c('browning','no_trend','greening'))]
lsat.vi.ecounit.frac.trnd.dt <- lsat.vi.ecounit.frac.trnd.dt[order(ecounit, trend.cat)]
lsat.vi.ecounit.frac.trnd.dt <- lsat.vi.ecounit.frac.trnd.dt[n.sites.ecounit >= 10]
setorder(lsat.vi.ecounit.frac.trnd.dt, trend.period, ecounit, trend.cat)

# spatialize trend fractions ------------------------
mkdirs('output/lsat_vi_gs_ecounit_trends_frac/mc_reps_gridded')

boreal.r[] <- NA
boreal.pxl.dt <- data.table(cellid = 1:ncell(boreal.r), ecounit = values(ecounit.r))
boreal.pxl.dt <- na.omit(boreal.pxl.dt)

trend.periods <- c('1985to2019','2000to2019')
# j = trend.periods[1]

for (j in trend.periods){
  
  # n sites per ecounit ---------
  print(paste0('starting to grid nsites ', j))
  lsat.vi.ecounit.nsites.dt <- dcast.data.table(data = lsat.vi.ecounit.frac.trnd.dt[trend.period == j], formula = ecounit ~ trend.period, value.var = 'n.sites.ecounit', fun.aggregate = mean)
  setnames(lsat.vi.ecounit.nsites.dt, j, 'n.sites')
  lsat.vi.ecounit.nsites.dt[is.na(lsat.vi.ecounit.nsites.dt)] <- 0
  
  ecounit.nsites.dt <- boreal.pxl.dt[lsat.vi.ecounit.nsites.dt, on = 'ecounit']
  ecounit.nsites.dt <- na.omit(ecounit.nsites.dt)
  
  nsites.r <- boreal.r
  nsites.r[ecounit.nsites.dt$cellid] <- ecounit.nsites.dt$n.sites
  writeRaster(nsites.r, paste0('output/lsat_vi_gs_ecounit_trends_frac/mc_reps_gridded/boreal_lsat_vi_gs_ecounit_nsites_',j,'_300m_laea_rep_',i,'.tif'), datatype = 'INT2U', overwrite=T)
  rm(list=c('nsites.r','ecounit.nsites.dt', 'lsat.vi.ecounit.nsites.dt'))

    
  # percent of sites that greenned and browned per ecounit ------------
  
  # cast wide and join grid cell data table
  lsat.vi.ecounit.frac.trnd.wide.dt <- dcast.data.table(data = lsat.vi.ecounit.frac.trnd.dt[trend.period == j], formula = ecounit ~ trend.cat, value.var = 'pcnt.sites')
  lsat.vi.ecounit.frac.trnd.wide.dt[is.na(lsat.vi.ecounit.frac.trnd.wide.dt)] <- 0
  
  ecounit.pcnt.dt <- boreal.pxl.dt[lsat.vi.ecounit.frac.trnd.wide.dt, on = 'ecounit']
  ecounit.pcnt.dt <- na.omit(ecounit.pcnt.dt)
  rm(lsat.vi.ecounit.frac.trnd.wide.dt)
  
  ### % sites greening
  print(paste0('starting to grid greening sites ', j))
  greening.pcnt.r <- boreal.r
  greening.pcnt.r[ecounit.pcnt.dt$cellid] <- ecounit.pcnt.dt$greening
  writeRaster(greening.pcnt.r, paste0('output/lsat_vi_gs_ecounit_trends_frac/mc_reps_gridded/boreal_lsat_vi_gs_ecounit_pcnt_greening_',j,'_300m_laea_rep_',i,'.tif'), datatype = 'INT1U', overwrite=T)
  rm(greening.pcnt.r)
  
  ### % sites browning
  print(paste0('starting to grid browning sites ', j))
  browning.pcnt.r <- boreal.r
  browning.pcnt.r[ecounit.pcnt.dt$cellid] <- ecounit.pcnt.dt$browning
  writeRaster(browning.pcnt.r, paste0('output/lsat_vi_gs_ecounit_trends_frac/mc_reps_gridded/boreal_lsat_vi_gs_ecounit_pcnt_browning_',j,'_300m_laea_rep_',i,'.tif'), datatype = 'INT1U', overwrite=T)
  rm(list=c('browning.pcnt.r','ecounit.pcnt.dt'))
  
  print(paste0('finished gridding ', j))
}

# 
# 
# # ECOUNIT AVERAGE CHANGE -----------------------------------------------------------
# boreal.trends.2000.dt <- boreal.pxl.dt[lsat.vi.ecounit.trends, on = 'ecounit']
# boreal.trends.2000.dt <- na.omit(boreal.trends.2000.dt)
# rm(boreal.pxl.dt)
# 
# # pval
# pval.2000.slp.r <- boreal.r
# pval.2000.slp.r[boreal.trends.2000.dt$cellid] <- round(boreal.trends.2000.dt$pval*1000)
# writeRaster(pval.2000.slp.r, 'data/gis_data/boreal_lsat_vi_ecounit_trend_pval_x1000_2000to2020.tif', datatype = 'INT2U', overwrite=T)
# rm(pval.2000.slp.r)
# 
# # slope
# trends.2000.slp.r <- boreal.r
# trends.2000.slp.r[boreal.trends.2000.dt$cellid] <- round(boreal.trends.2000.dt$slope*10000)
# writeRaster(trends.2000.slp.r, 'data/gis_data/boreal_lsat_vi_ecounit_trend_slope_x10000_2000to2020.tif', datatype = 'INT2S', overwrite=T)
# rm(trends.2000.slp.r)
# 
# # vi change
# trends.2000.chng.r <- boreal.r
# trends.2000.chng.r[boreal.trends.2000.dt$cellid] <- round(boreal.trends.2000.dt$delta.vi*10000)
# writeRaster(trends.2000.chng.r, 'data/gis_data/boreal_lsat_vi_ecounit_total_change_vi_x10000_2000to2020.tif', datatype = 'INT2S', overwrite=T)
# rm(trends.2000.chng.r)
# 
# # vi change as pcnt
# trends.2000.chng.pcnt.r <- boreal.r
# trends.2000.chng.pcnt.r[boreal.trends.2000.dt$cellid] <- round(boreal.trends.2000.dt$delta.vi.pcnt*100)
# writeRaster(trends.2000.chng.pcnt.r, 'data/gis_data/boreal_lsat_vi_ecounit_pcnt_change_vi_x100_2000to2020.tif', datatype = 'INT2S', overwrite=T)
# rm(trends.2000.chng.pcnt.r)


# END SCRIPT ==================================================================================================




# # COMPUTE PROPORTION OF SITES IN EACH BIOCLIMATIC ZONE THAT HAVE SPECIFIC TREND CATEGORIES ===============================================
# 
# # zones
# vi.trnd.site.cat.pcnt.by.zone <- vi.trnd.site[trend.cat != 'NA.NA'][lat.zone != ''][, .(n.sites = .N), by=c('period', 'lat.zone', 'trend.cat')]
# vi.trnd.site.cat.pcnt.by.zone <- vi.trnd.site.cat.pcnt.by.zone[, n.sites.zone := sum(n.sites), by = c('period','lat.zone')]
# vi.trnd.site.cat.pcnt.by.zone <- vi.trnd.site.cat.pcnt.by.zone[, pcnt.sites := n.sites / n.sites.zone * 100]
# vi.trnd.site.cat.pcnt.by.zone <- vi.trnd.site.cat.pcnt.by.zone[, trend.cat := factor(trend.cat, 
#                                                                                          levels = c('browning.sig.p5','browning.sig.p10','insig','greening.sig.p10','greening.sig.p5'),
#                                                                                          labels = c('browning (p<0.05)','browning (p<0.10)','no trend','greening (p<0.10)','greening (p<0.05)'))]
# 
# vi.trnd.site.cat.pcnt.by.zone <- vi.trnd.site.cat.pcnt.by.zone[order(period,lat.zone,trend.cat)]
# vi.trnd.site.cat.pcnt.by.zone <- vi.trnd.site.cat.pcnt.by.zone[, pcnt.position := cumsum(pcnt.sites)-pcnt.sites/2, by = c('period','lat.zone')]
# vi.trnd.site.cat.pcnt.by.zone <- vi.trnd.site.cat.pcnt.by.zone[, cnt.position := cumsum(n.sites)-n.sites/2, by = c('period','lat.zone')]
# 
# # biome
# vi.trnd.site.cat.pcnt.by.biome <- vi.trnd.site[trend.cat != 'NA.NA'][lat.zone != ''][, .(n.sites = .N), by=c('period', 'trend.cat')]
# vi.trnd.site.cat.pcnt.by.biome <- vi.trnd.site.cat.pcnt.by.biome[, n.sites.zone := sum(n.sites), by = c('period')]
# vi.trnd.site.cat.pcnt.by.biome <- vi.trnd.site.cat.pcnt.by.biome[, pcnt.sites := n.sites / n.sites.zone * 100]
# vi.trnd.site.cat.pcnt.by.biome <- vi.trnd.site.cat.pcnt.by.biome[, trend.cat := factor(trend.cat, 
#                                                                                            levels = c('browning.sig.p5','browning.sig.p10','insig','greening.sig.p10','greening.sig.p5'),
#                                                                                            labels = c('browning (p<0.05)','browning (p<0.10)','no trend','greening (p<0.10)','greening (p<0.05)'))]
# 
# vi.trnd.site.cat.pcnt.by.biome <- vi.trnd.site.cat.pcnt.by.biome[order(period,trend.cat)]
# vi.trnd.site.cat.pcnt.by.biome <- vi.trnd.site.cat.pcnt.by.biome[, pcnt.position := cumsum(pcnt.sites)-pcnt.sites/2, by = c('period')]
# vi.trnd.site.cat.pcnt.by.biome <- vi.trnd.site.cat.pcnt.by.biome[, cnt.position := cumsum(n.sites)-n.sites/2, by = c('period')]
# vi.trnd.site.cat.pcnt.by.biome <- vi.trnd.site.cat.pcnt.by.biome[, lat.zone := 'boreal']
# 
# #combine zones and biome
# vi.trnd.site.cat.pcnt.by.zone <- rbind(vi.trnd.site.cat.pcnt.by.zone, vi.trnd.site.cat.pcnt.by.biome)
# 
# # add MC rep
# vi.trnd.site.cat.pcnt.by.zone$rep <- i
# 
# # write out
# mkdirs('output/lsat_zonal_freq_trends/mc_reps')
# fwrite(vi.trnd.site.cat.pcnt.by.zone, paste0('output/lsat_zonal_freq_trends/mc_reps/Landsat_vimax_tundra_zone_pcnt_trend_category_rep_',i,'.csv'))
# 
# print('finished freq of site trends...')  


# # GRID SITES - LEVEL TRENDS ================================================================================================
# # spatalize sites and write out as shapefile
# wgs84 <- CRS("+proj=longlat +datum=WGS84")
# laea <- CRS("+proj=laea +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
# 
# vi.trnd.site.pts.wgs <- SpatialPoints(coords = cbind(vi.trnd.site$longitude, vi.trnd.site$latitude), proj4string = wgs84)
# vi.trnd.site.pts.laea <- spTransform(vi.trnd.site.pts.wgs, CRSobj = laea)
# 
# boreal.zones.r <- raster('data/gis_data/boreal_zones/boreal_oroboreal_lat_zones_laea_50km.tif')
# 
# boreal.tmplt.r <- boreal.zones.r
# boreal.tmplt.r[boreal.tmplt.r >= 0] <- NA
# 
# boreal.pxl.id.r <- boreal.zones.r
# boreal.pxl.id.r[] <- 1:ncell(boreal.zones.r) 
# 
# site.pxl.id <- data.table(site = vi.trnd.site$site, pxl.id = raster::extract(boreal.pxl.id.r, vi.trnd.site.pts.laea))
# 
# vi.trnd.site$pxl.id <- site.pxl.id$pxl.id[match(site.pxl.id$site, vi.trnd.site$site)]
# 
# 
# # NUMBER OF SITES
# vi.site.cnt.pxl <- vi.trnd.site[, .(n.sites = .N), by=c('period', 'pxl.id')]
# 
# vi.site.cnt.pxl.gte1985 <- vi.site.cnt.pxl[period == '1985-2016']
# vi.site.cnt.pxl.gte2000 <- vi.site.cnt.pxl[period == '2000-2016']
# 
# boreal.site.cnt.gte1985.r <- boreal.tmplt.r
# boreal.site.cnt.gte1985.r[vi.site.cnt.pxl.gte1985$pxl.id] <- vi.site.cnt.pxl.gte1985$n.sites
# 
# boreal.site.cnt.gte2000.r <- boreal.tmplt.r
# boreal.site.cnt.gte2000.r[vi.site.cnt.pxl.gte2000$pxl.id] <- vi.site.cnt.pxl.gte2000$n.sites
# 
# # GREENING
# vi.trnd.grn.pxl <- vi.trnd.site[, n.sites := .N, by=c('period', 'pxl.id')]
# vi.trnd.grn.pxl <- vi.trnd.site[pval <= 0.10 & slope.cat == 'greening', .(n.sites = first(n.sites), n.sites.grn = .N), by=c('period', 'pxl.id')]
# vi.trnd.grn.pxl <- vi.trnd.grn.pxl[, pcnt.sites.grn := n.sites.grn / n.sites * 100]
# 
# vi.trnd.grn.pxl.gte1985 <- vi.trnd.grn.pxl[period == '1985-2016']
# vi.trnd.grn.pxl.gte2000 <- vi.trnd.grn.pxl[period == '2000-2016']
# 
# boreal.grn.gte1985.r <- boreal.tmplt.r
# boreal.grn.gte1985.r[boreal.site.cnt.gte1985.r > 0] <- 0
# boreal.grn.gte1985.r[vi.trnd.grn.pxl.gte1985$pxl.id] <- vi.trnd.grn.pxl.gte1985$pcnt.sites.grn 
# 
# boreal.grn.gte2000.r <- boreal.tmplt.r
# boreal.grn.gte2000.r[boreal.site.cnt.gte2000.r > 0] <- 0
# boreal.grn.gte2000.r[vi.trnd.grn.pxl.gte2000$pxl.id] <- vi.trnd.grn.pxl.gte2000$pcnt.sites.grn 
# 
# 
# # BROWNING
# vi.trnd.brn.pxl <- vi.trnd.site[, n.sites := .N, by=c('period', 'pxl.id')]
# vi.trnd.brn.pxl <- vi.trnd.site[pval <= 0.10 & slope.cat == 'browning', .(n.sites = first(n.sites), n.sites.brn = .N), by=c('period', 'pxl.id')]
# vi.trnd.brn.pxl <- vi.trnd.brn.pxl[, pcnt.sites.brn := n.sites.brn / n.sites * 100]
# 
# vi.trnd.brn.pxl.gte1985 <- vi.trnd.brn.pxl[period == '1985-2016']
# vi.trnd.brn.pxl.gte2000 <- vi.trnd.brn.pxl[period == '2000-2016']
# 
# boreal.brn.gte1985.r <- boreal.tmplt.r
# boreal.brn.gte1985.r[boreal.site.cnt.gte1985.r > 0] <- 0
# boreal.brn.gte1985.r[vi.trnd.brn.pxl.gte1985$pxl.id] <- vi.trnd.brn.pxl.gte1985$pcnt.sites.brn 
# 
# boreal.brn.gte2000.r <- boreal.tmplt.r
# boreal.brn.gte2000.r[boreal.site.cnt.gte2000.r > 0] <- 0
# boreal.brn.gte2000.r[vi.trnd.brn.pxl.gte2000$pxl.id] <- vi.trnd.brn.pxl.gte2000$pcnt.sites.brn 
# 
# # plot(boreal.brn.gte1985.r)
# # plot(boreal.brn.gte2000.r)
# 
# # WRITE OUT RASTERS
# mkdirs('output/lsat_gridded_trends/mc_reps')
# writeRaster(boreal.site.cnt.gte1985.r, paste0('output/lsat_gridded_trends/mc_reps/boreal_lsat_vi_site_cnt_1985to2016_rep_',i,'.tif'), overwrite=T)
# writeRaster(boreal.site.cnt.gte2000.r, paste0('output/lsat_gridded_trends/mc_reps/boreal_lsat_vi_site_cnt_2000to2016_rep_',i,'.tif'), overwrite=T)
# 
# writeRaster(boreal.grn.gte1985.r, paste0('output/lsat_gridded_trends/mc_reps/boreal_lsat_vi_pcnt_greening_p10_1985to2016_rep_',i,'.tif'), overwrite=T)
# writeRaster(boreal.grn.gte2000.r, paste0('output/lsat_gridded_trends/mc_reps/boreal_lsat_vi_pcnt_greening_p10_2000to2016_rep_',i,'.tif'), overwrite=T)
# 
# writeRaster(boreal.brn.gte1985.r, paste0('output/lsat_gridded_trends/mc_reps/boreal_lsat_vi_pcnt_browning_p10_1985to2016_rep_',i,'.tif'), overwrite=T)
# writeRaster(boreal.brn.gte2000.r, paste0('output/lsat_gridded_trends/mc_reps/boreal_lsat_vi_pcnt_browning_p10_2000to2016_rep_',i,'.tif'), overwrite=T)
# 
# print('finished gridded trends... All done!!')  



# lsat.vi.ecounit.frac.trnd.wide.dt <- dcast.data.table(data = lsat.vi.ecounit.frac.trnd.dt, formula = ecounit ~ trend.cat + trend.period, value.var = 'pcnt.sites')
# lsat.vi.ecounit.frac.trnd.wide.dt[is.na(lsat.vi.ecounit.frac.trnd.wide.dt)] <- 0
# 
# lsat.vi.ecounit.nsites.dt <- dcast.data.table(data = lsat.vi.ecounit.frac.trnd.dt, formula = ecounit ~ trend.period, value.var = 'n.sites.ecounit', fun.aggregate = mean)
# lsat.vi.ecounit.nsites.dt[is.na(lsat.vi.ecounit.nsites.dt)] <- 0
# 
# # spatialize trend fractions -------------------------------------------------
# mkdirs('output/lsat_vi_gs_ecounit_trends_frac/mc_reps_gridded')
# 
# boreal.r[] <- NA
# boreal.pxl.dt <- data.table(cellid = 1:ncell(boreal.r), ecounit = values(ecounit.r))
# boreal.pxl.dt <- na.omit(boreal.pxl.dt)
# 
# ecounit.pcnt.dt <- boreal.pxl.dt[lsat.vi.ecounit.frac.trnd.wide.dt, on = 'ecounit']
# ecounit.pcnt.dt <- na.omit(ecounit.pcnt.dt)
# 
# 
# ## 1985 - 2019 ----------------
# 
# ### n sites
# ecounit.nsites.dt <- boreal.pxl.dt[lsat.vi.ecounit.nsites.dt, on = 'ecounit']
# ecounit.nsites.dt <- na.omit(ecounit.nsites.dt)
# 
# nsites.1985.r <- boreal.r
# nsites.1985.r[ecounit.pcnt.dt$cellid] <- ecounit.nsites.dt$'1985to2019'
# writeRaster(nsites.1985.r, paste0('output/lsat_vi_gs_ecounit_trends_frac/mc_reps_gridded/boreal_lsat_vi_gs_ecounit_nsites_1985to2019_300m_laea_rep_',i,'.tif'), datatype = 'INT1U', overwrite=T)
# rm(nsites.1985.r)
# 
# ### % sites greening
# greening.pcnt.1985.r <- boreal.r
# greening.pcnt.1985.r[ecounit.pcnt.dt$cellid] <- ecounit.pcnt.dt$greening_1985to2019
# writeRaster(greening.pcnt.1985.r, paste0('output/lsat_vi_gs_ecounit_trends_frac/mc_reps_gridded/boreal_lsat_vi_gs_ecounit_pcnt_greening_1985to2019_300m_laea_rep_',i,'.tif'), datatype = 'INT1U', overwrite=T)
# rm(greening.pcnt.1985.r)
# 
# ### % sites browning
# browning.pcnt.1985.r <- boreal.r
# browning.pcnt.1985.r[ecounit.pcnt.dt$cellid] <- ecounit.pcnt.dt$browning_1985to2019
# writeRaster(browning.pcnt.1985.r, paste0('output/lsat_vi_gs_ecounit_trends_frac/mc_reps_gridded/boreal_lsat_vi_gs_ecounit_pcnt_browning_1985to2019_300m_laea_rep_',i,'.tif'), datatype = 'INT1U', overwrite=T)
# rm(browning.pcnt.1985.r)
# 
# 
# 
# # ECOUNIT AVERAGE CHANGE -----------------------------------------------------------
# boreal.trends.2000.dt <- boreal.pxl.dt[lsat.vi.ecounit.trends, on = 'ecounit']
# boreal.trends.2000.dt <- na.omit(boreal.trends.2000.dt)
# rm(boreal.pxl.dt)
# 
# # pval
# pval.2000.slp.r <- boreal.r
# pval.2000.slp.r[boreal.trends.2000.dt$cellid] <- round(boreal.trends.2000.dt$pval*1000)
# writeRaster(pval.2000.slp.r, 'data/gis_data/boreal_lsat_vi_ecounit_trend_pval_x1000_2000to2020.tif', datatype = 'INT2U', overwrite=T)
# rm(pval.2000.slp.r)
# 
# # slope
# trends.2000.slp.r <- boreal.r
# trends.2000.slp.r[boreal.trends.2000.dt$cellid] <- round(boreal.trends.2000.dt$slope*10000)
# writeRaster(trends.2000.slp.r, 'data/gis_data/boreal_lsat_vi_ecounit_trend_slope_x10000_2000to2020.tif', datatype = 'INT2S', overwrite=T)
# rm(trends.2000.slp.r)
# 
# # vi change
# trends.2000.chng.r <- boreal.r
# trends.2000.chng.r[boreal.trends.2000.dt$cellid] <- round(boreal.trends.2000.dt$delta.vi*10000)
# writeRaster(trends.2000.chng.r, 'data/gis_data/boreal_lsat_vi_ecounit_total_change_vi_x10000_2000to2020.tif', datatype = 'INT2S', overwrite=T)
# rm(trends.2000.chng.r)
# 
# # vi change as pcnt
# trends.2000.chng.pcnt.r <- boreal.r
# trends.2000.chng.pcnt.r[boreal.trends.2000.dt$cellid] <- round(boreal.trends.2000.dt$delta.vi.pcnt*100)
# writeRaster(trends.2000.chng.pcnt.r, 'data/gis_data/boreal_lsat_vi_ecounit_pcnt_change_vi_x100_2000to2020.tif', datatype = 'INT2S', overwrite=T)
# rm(trends.2000.chng.pcnt.r)
# 
