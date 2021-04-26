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
i = 100

setwd('/projects/arctic/users/lberner/boreal_biome_shift/')


# READ IN LANDSAT VI TIME SERIES  ==============================================================================================================
print('Loading data...')  

site.dt <- fread('output/boreal_sample_site_climate_and_landcover.csv')
lsat.vi.dt <- fread(list.files('output/lsat_vi_gs_site_timeseries/', full.names = T)[i])

boreal.r <- raster('data/gis_data/wwf_boreal_biome_laea_300m.tif')
ecounit.r <- raster('data/gis_data/ecological_land_unit_boreal_aoi_300m_laea.tif')     

vi.rep <- unique(lsat.vi.dt$vi.name)

# reformat i for sorting
if (i < 10){
  i <- paste0('000',i)
} else if (i < 100){
  i <- paste0('00',i)
} else if (i < 1000){
  i <- paste0('0',i)
}


# SITE: COMPUTE MANN-KENDALL TREND ===========================================================================================
print('Computing vi trend for each site...')  
mkdirs('output/lsat_vi_gs_site_trends/mc_reps')

lsat.vi.site.trnd.1985.dt <- lsat_calc_trend(lsat.vi.dt, vi = 'vi.max', yrs = 1985:2019, yr.tolerance = 1, nyr.min.frac = 0.666, sig = 0.10)
lsat.vi.site.trnd.2000.dt <- lsat_calc_trend(lsat.vi.dt, vi = 'vi.max', yrs = 2000:2019, yr.tolerance = 1, nyr.min.frac = 0.666, sig = 0.10)
lsat.vi.site.trnd.dt <- rbind(lsat.vi.site.trnd.1985.dt, lsat.vi.site.trnd.2000.dt)

lsat.vi.site.trnd.dt$rep <- i
fwrite(lsat.vi.site.trnd.dt, paste0('output/lsat_vi_gs_site_trends/mc_reps/lsat_vi_gs_boreal_site_trends_rep_',i,'.csv'))


# BIOME: COMPUTE % OF SAMPLING SITES THAT GREENED AND BROWNED ==============================================================
print('Computing biome fractional trends...')  
mkdirs('output/lsat_vi_gs_biome_trends_frac/mc_reps_tabular')

lsat.vi.biome.frac.trnd.dt <- lsat.vi.site.trnd.dt[ , .(n.sites = .N), by=c('trend.period','trend.cat')]
lsat.vi.biome.frac.trnd.dt <- lsat.vi.biome.frac.trnd.dt[, n.sites.biome := sum(n.sites), by=c('trend.period')]
lsat.vi.biome.frac.trnd.dt <- lsat.vi.biome.frac.trnd.dt[, pcnt.sites := round(n.sites / n.sites.biome * 100, 1)]
lsat.vi.biome.frac.trnd.dt <- lsat.vi.biome.frac.trnd.dt[, trend.cat := factor(trend.cat, levels = c('browning','no_trend','greening'))]
setorder(lsat.vi.biome.frac.trnd.dt, trend.period, trend.cat)

lsat.vi.biome.frac.trnd.dt[, vi.name := vi.rep]
lsat.vi.biome.frac.trnd.dt$rep <- i
fwrite(lsat.vi.biome.frac.trnd.dt, paste0('output/lsat_vi_gs_biome_trends_frac/mc_reps_tabular/lsat_vi_gs_boreal_biome_frac_trends_rep_',i,'.csv'))


# LANDCOVER: COMPUTE % OF SAMPLING SITES IN EACH LANDCOVER THAT GREENED AND BROWNED ==============================================================
print('Computing landcover fractional trends...')  
mkdirs('output/lsat_vi_gs_landcov_trends_frac/mc_reps_tabular')

lsat.vi.site.trnd.dt$landcov.name <- site.dt$landcov.name[match(lsat.vi.site.trnd.dt$site, site.dt$site)]

lsat.vi.landcov.frac.trnd.dt <- lsat.vi.site.trnd.dt[ , .(n.sites = .N), by=c('trend.period','landcov.name','trend.cat')]
lsat.vi.landcov.frac.trnd.dt <- lsat.vi.landcov.frac.trnd.dt[, n.sites.landcov := sum(n.sites), by=c('trend.period','landcov.name')]
lsat.vi.landcov.frac.trnd.dt <- lsat.vi.landcov.frac.trnd.dt[, pcnt.sites := round(n.sites / n.sites.landcov * 100, 1)]
lsat.vi.landcov.frac.trnd.dt <- lsat.vi.landcov.frac.trnd.dt[, trend.cat := factor(trend.cat, levels = c('browning','no_trend','greening'))]
lsat.vi.landcov.frac.trnd.dt <- lsat.vi.landcov.frac.trnd.dt[order(landcov.name, trend.cat)]
lsat.vi.landcov.frac.trnd.dt <- lsat.vi.landcov.frac.trnd.dt[n.sites.landcov >= 10]
setorder(lsat.vi.landcov.frac.trnd.dt, trend.period, landcov.name, trend.cat)

lsat.vi.landcov.frac.trnd.dt[, vi.name := vi.rep]
lsat.vi.landcov.frac.trnd.dt$rep <- i
fwrite(lsat.vi.landcov.frac.trnd.dt, paste0('output/lsat_vi_gs_landcov_trends_frac/mc_reps_tabular/lsat_vi_gs_boreal_landcov_frac_trends_rep_',i,'.csv'))


# ECOUNIT: COMPUTE % OF SAMPLING SITES IN EACH ECOUNIT THAT GREENED AND BROWNED ==============================================================
print('Computing ecounit fractional trends...')  
mkdirs('output/lsat_vi_gs_ecounit_trends_frac/mc_reps_tabular')

lsat.vi.site.trnd.dt$ecounit <- site.dt$ecounit[match(lsat.vi.site.trnd.dt$site, site.dt$site)]

lsat.vi.ecounit.frac.trnd.dt <- lsat.vi.site.trnd.dt[ , .(n.sites = .N), by=c('trend.period','ecounit','trend.cat')]
lsat.vi.ecounit.frac.trnd.dt <- lsat.vi.ecounit.frac.trnd.dt[, n.sites.ecounit := sum(n.sites), by=c('trend.period','ecounit')]
lsat.vi.ecounit.frac.trnd.dt <- lsat.vi.ecounit.frac.trnd.dt[, pcnt.sites := round(n.sites / n.sites.ecounit * 100, 1)]
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
  writeRaster(nsites.r, paste0('output/lsat_vi_gs_ecounit_trends_frac/mc_reps_gridded/lsat_vi_gs_boreal_ecounit_nsites_',j,'_300m_laea_rep_',i,'.tif'), datatype = 'INT2U', overwrite=T)
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
  writeRaster(greening.pcnt.r, paste0('output/lsat_vi_gs_ecounit_trends_frac/mc_reps_gridded/lsat_vi_gs_boreal_ecounit_pcnt_greening_',j,'_300m_laea_rep_',i,'.tif'), datatype = 'INT1U', overwrite=T)
  rm(greening.pcnt.r)
  
  ### % sites browning
  print(paste0('starting to grid browning sites ', j))
  browning.pcnt.r <- boreal.r
  browning.pcnt.r[ecounit.pcnt.dt$cellid] <- ecounit.pcnt.dt$browning
  writeRaster(browning.pcnt.r, paste0('output/lsat_vi_gs_ecounit_trends_frac/mc_reps_gridded/lsat_vi_gs_boreal_ecounit_pcnt_browning_',j,'_300m_laea_rep_',i,'.tif'), datatype = 'INT1U', overwrite=T)
  rm(list=c('browning.pcnt.r','ecounit.pcnt.dt'))
  
  print(paste0('finished gridding ', j))
}


# COMPUTE AVERAGE PERCENT CHANGE IN VEGETATION INDEX FOR EACH ECOUNIT ==============================================================

lsat.vi.ecounit.med.trnd.dt <- lsat.vi.site.trnd.dt[, .(total.change.pcnt.med = median(total.change.pcnt), n.sites = .N), by = c('ecounit','trend.period','vi.name','rep')]

ggplot(lsat.vi.ecounit.med.trnd.dt, aes(total.change.pcnt.med)) + facet_wrap(~trend.period) + geom_density() + xlim(x=c(-50,50))



ggplot(lsat.vi.site.trnd.dt, aes(total.change.pcnt)) + facet_wrap(~trend.period) + geom_density() + xlim(x=c(-50,50))

ggplot(lsat.vi.site.trnd.dt, aes(total.change.pcnt)) + facet_wrap(~trend.period) + geom_histogram() + xlim(x=c(-50,50))

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

print("All done!!")
# END SCRIPT ==================================================================================================
