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
i = as.numeric(args[1])
# i = 1

tmp.dir <- paste0('/scratch/lb968/lsat_vi_trend_',i)
tempfile(tmpdir=tmp.dir)
rasterOptions(tmpdir=tmp.dir)

setwd('/projects/arctic/users/lberner/boreal_biome_shift/')


# READ IN LANDSAT VI TIME SERIES  ==============================================================================================================
print('Loading data...')  

lsat.dt <- fread('data/lsat_samples/boreal_lsat_clean_data_100k_sites_1985to2016.csv')

site.dt <- fread('output/boreal_sample_site_climate_and_landcover.csv')
lsat.vi.dt <- fread(list.files('output/lsat_vi_gs_site_timeseries/', full.names = T)[i])

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

lsat.vi.site.trnd.dt[, vi.name := vi.rep]
lsat.vi.site.trnd.dt$rep <- i
fwrite(lsat.vi.site.trnd.dt, paste0('output/lsat_vi_gs_site_trends/mc_reps/lsat_vi_gs_boreal_site_trends_rep_',i,'.csv'))

lsat.vi.site.trnd.dt <- fread(paste0('output/lsat_vi_gs_site_trends/mc_reps/lsat_vi_gs_boreal_site_trends_rep_',i,'.csv'))

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
print('Computing trend prevalence by landcover...')
mkdirs('output/lsat_vi_gs_landcov_trends_frac/mc_reps_tabular')

lsat.vi.site.trnd.dt$landcov.name <- site.dt$landcov.name[match(lsat.vi.site.trnd.dt$site, site.dt$site)]

lsat.vi.landcov.frac.trnd.dt <- lsat.vi.site.trnd.dt[ , .(n.sites = .N), by=c('trend.period','landcov.name','trend.cat')]
lsat.vi.landcov.frac.trnd.dt <- lsat.vi.landcov.frac.trnd.dt[, n.sites.landcov := sum(n.sites), by=c('trend.period','landcov.name')]
lsat.vi.landcov.frac.trnd.dt <- lsat.vi.landcov.frac.trnd.dt[, pcnt.sites := round(n.sites / n.sites.landcov * 100, 1)]
lsat.vi.landcov.frac.trnd.dt <- lsat.vi.landcov.frac.trnd.dt[, trend.cat := factor(trend.cat, levels = c('browning','no_trend','greening'))]
lsat.vi.landcov.frac.trnd.dt <- lsat.vi.landcov.frac.trnd.dt[order(landcov.name, trend.cat)]
lsat.vi.landcov.frac.trnd.dt <- lsat.vi.landcov.frac.trnd.dt[n.sites.landcov >= 10 & is.na(trend.cat) == F & is.na(landcov.name) == F]
setorder(lsat.vi.landcov.frac.trnd.dt, trend.period, landcov.name, trend.cat)

lsat.vi.landcov.frac.trnd.dt[, vi.name := vi.rep]
lsat.vi.landcov.frac.trnd.dt$rep <- i
fwrite(lsat.vi.landcov.frac.trnd.dt, paste0('output/lsat_vi_gs_landcov_trends_frac/mc_reps_tabular/lsat_vi_gs_boreal_landcov_frac_trends_rep_',i,'.csv'))


# LANDCOVER: COMPUTE % OF SAMPLE SITES THAT GREENED AND BROWNED ACROSS LAND COVER TYPES (I.E., WHERE DID G/B MOSTLY OCCUR?) ==================
print('Computing trend occurrence within each landcover classes...')
mkdirs('output/lsat_vi_gs_landcov_trends/mc_reps_tabular')

lsat.vi.landcov.frac.all.trnd.dt <- lsat.vi.site.trnd.dt[ , .(n.sites.landcov.trnd = .N), by=c('trend.period','landcov.name','trend.cat')]
lsat.vi.landcov.frac.all.trnd.dt <- lsat.vi.site.trnd.dt[ , n.sites.landcov := sum(n.sites.landcov.trnd), by=c('trend.period','landcov.name','trend.cat')]
lsat.vi.landcov.frac.all.trnd.dt <- lsat.vi.landcov.frac.all.trnd.dt[trend.cat == 'greening', n.sites.trnd := sum(n.sites.landcov.trnd), by=c('trend.period')]
lsat.vi.landcov.frac.all.trnd.dt <- lsat.vi.landcov.frac.all.trnd.dt[trend.cat == 'browning', n.sites.trnd := sum(n.sites.landcov.trnd), by=c('trend.period')]
lsat.vi.landcov.frac.all.trnd.dt <- lsat.vi.landcov.frac.all.trnd.dt[trend.cat == 'no_trend', n.sites.trnd := sum(n.sites.landcov.trnd), by=c('trend.period')]
lsat.vi.landcov.frac.all.trnd.dt <- lsat.vi.landcov.frac.all.trnd.dt[trend.cat == 'greening', pcnt.sites := round(n.sites.landcov.trnd / n.sites.trnd * 100, 1)]
lsat.vi.landcov.frac.all.trnd.dt <- lsat.vi.landcov.frac.all.trnd.dt[trend.cat == 'browning', pcnt.sites := round(n.sites.landcov.trnd / n.sites.trnd * 100, 1)]
lsat.vi.landcov.frac.all.trnd.dt <- lsat.vi.landcov.frac.all.trnd.dt[trend.cat == 'no_trend', pcnt.sites := round(n.sites.landcov.trnd / n.sites.trnd * 100, 1)]
lsat.vi.landcov.frac.all.trnd.dt <- lsat.vi.landcov.frac.all.trnd.dt[, trend.cat := factor(trend.cat, levels = c('browning','no_trend','greening'))]
lsat.vi.landcov.frac.all.trnd.dt <- lsat.vi.landcov.frac.all.trnd.dt[order(landcov.name, trend.cat)]
lsat.vi.landcov.frac.all.trnd.dt <- lsat.vi.landcov.frac.all.trnd.dt[is.na(trend.cat) == F & is.na(landcov.name) == F]
setorder(lsat.vi.landcov.frac.all.trnd.dt, trend.period, landcov.name, trend.cat)

lsat.vi.landcov.frac.all.trnd.dt[, vi.name := vi.rep]
lsat.vi.landcov.frac.all.trnd.dt$rep <- i
fwrite(lsat.vi.landcov.frac.all.trnd.dt, paste0('output/lsat_vi_gs_landcov_trends/mc_reps_tabular/lsat_vi_gs_boreal_landcov_trends_rep_',i,'.csv'))



# ECOUNIT: COMPUTE % OF SAMPLING SITES IN EACH ECOUNIT THAT GREENED AND BROWNED ==============================================================
print('Computing ecounit fractional trends...')
mkdirs('output/lsat_vi_gs_ecounit_trends_frac/mc_reps_tabular')

lsat.vi.site.trnd.dt$ecounit <- site.dt$ecounit[match(lsat.vi.site.trnd.dt$site, site.dt$site)]

lsat.vi.ecounit.frac.trnd.dt <- lsat.vi.site.trnd.dt[ , .(n.sites = .N), by=c('trend.period','ecounit','trend.cat')]
lsat.vi.ecounit.frac.trnd.dt <- lsat.vi.ecounit.frac.trnd.dt[, n.sites.ecounit := sum(n.sites), by=c('trend.period','ecounit')]
lsat.vi.ecounit.frac.trnd.dt <- lsat.vi.ecounit.frac.trnd.dt[, pcnt.sites := round(n.sites / n.sites.ecounit * 100, 1)]
lsat.vi.ecounit.frac.trnd.dt <- lsat.vi.ecounit.frac.trnd.dt[, trend.cat := factor(trend.cat, levels = c('browning','no_trend','greening'))]
lsat.vi.ecounit.frac.trnd.dt <- lsat.vi.ecounit.frac.trnd.dt[order(ecounit, trend.cat)]
lsat.vi.ecounit.frac.trnd.dt <- lsat.vi.ecounit.frac.trnd.dt[n.sites.ecounit >= 10 & is.na(trend.cat) == F & is.na(ecounit) == F]
setorder(lsat.vi.ecounit.frac.trnd.dt, trend.period, ecounit, trend.cat)

lsat.vi.ecounit.frac.trnd.dt[, vi.name := vi.rep]
lsat.vi.ecounit.frac.trnd.dt$rep <- i
fwrite(lsat.vi.ecounit.frac.trnd.dt, paste0('output/lsat_vi_gs_ecounit_trends_frac/mc_reps_tabular/lsat_vi_gs_boreal_ecounit_frac_trends_rep_',i,'.csv'))


# ECOUNIT: COMPUTE MEDIAN PERCENT CHANGE IN VEGETATION INDEX FOR EACH ECOUNIT ==============================================================
mkdirs('output/lsat_vi_gs_ecounit_trends_median/mc_reps_tabular')

lsat.vi.ecounit.med.trnd.dt <- lsat.vi.site.trnd.dt[, .(total.change.pcnt.med = median(total.change.pcnt, na.rm=T), n.sites = .N), by = c('ecounit','trend.period','vi.name','rep')]
lsat.vi.ecounit.med.trnd.dt <- lsat.vi.ecounit.med.trnd.dt[n.sites >= 10 & is.na(ecounit) == F]
lsat.vi.ecounit.med.trnd.dt[, vi.name := vi.rep]
lsat.vi.ecounit.med.trnd.dt$rep <- i

fwrite(lsat.vi.ecounit.med.trnd.dt, paste0('output/lsat_vi_gs_ecounit_trends_median/mc_reps_tabular/lsat_vi_gs_boreal_ecounit_median_trends_rep_',i,'.csv'))


# CLEAN UP ======================================================================================================================
gc()
removeTmpFiles()
unlink(tmp.dir, recursive = T)
print("All done!!")

# END SCRIPT ====================================================================================================================