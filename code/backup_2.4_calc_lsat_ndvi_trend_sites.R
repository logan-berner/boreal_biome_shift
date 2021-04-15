# COMPUTE TRENDS IN LANDSAT NDVI FOR SITES ACROSS THE BOREAL DOMAIN USING MANN-KENDALL TESTS 
# AUTHOR: LOGAN BERNER, NAU
# DATE: 2020-12-01
rm(list=ls())
.libPaths(c(.libPaths(), "~/R/", '/home/lb968/.conda/envs/rstudio_env/lib/R/library'))
require(plyr)
require(dplyr)
require(data.table)
require(zyp)
require(R.utils)

args <- commandArgs(TRUE)
# i = as.numeric(args[1])
i=1

# reformat i for sorting
if (i < 10){
  i <- paste0('000',i)
} else if (i < 100){
  i <- paste0('00',i)
} else if (i < 1000){
  i <- paste0('0',i)
}

setwd('/projects/arctic/users/lberner/boreal_biome_shift/')

# READ IN AND SUBSET LANDSAT VI TIME SERIES ================================================================

# landsat time series
infile <- paste0('output/ndvi_timeseries/boreal_site_lsat_ja_vi_timeseries_rep_',i,'.csv')
site.ndvi.ts.dt <- fread(infile, fill=T)

# subset sites with obs from 1985 - 2017
site.ndvi.ts.dt.gte1985 <- site.ndvi.ts.dt[first.yr <= 1986]
site.ndvi.ts.dt.gte1985 <- site.ndvi.ts.dt.gte1985[, n.yr.obs := .N, by = 'site'][n.yr.obs >= 20]
site.ndvi.ts.dt.gte1985$period <- '1985-2017'
site.ndvi.ts.dt.gte1985$year.rsc <- site.ndvi.ts.dt.gte1985$year - 1985
length(unique(site.ndvi.ts.dt.gte1985$site))

# subset sites with obs from 2000 - 2017
site.ndvi.ts.dt.gte2000 <- site.ndvi.ts.dt[year >= 2000][first.yr <= 2000]
site.ndvi.ts.dt.gte2000 <- site.ndvi.ts.dt.gte2000[, n.yr.obs := .N, by = 'site'][n.yr.obs >= 13]
site.ndvi.ts.dt.gte2000$period <- '2000-2017'
site.ndvi.ts.dt.gte2000$year.rsc <- site.ndvi.ts.dt.gte2000$year - 2000
length(unique(site.ndvi.ts.dt.gte2000$site))

# combine sites for both time periods
site.ndvi.ts.dt.periods <- rbind(site.ndvi.ts.dt.gte1985, site.ndvi.ts.dt.gte2000)

# summarize ancillary landsat info on sites
site.smry.dt <- site.ndvi.ts.dt[, .(latitude = mean(latitude), longitude = mean(longitude), first.yr = first(first.yr), n.yr.obs = first(n.yr.obs), n.scenes = sum(n.scenes)), by = 'site']


# DEFINE FUNCTIONS FOR COMUPTING AND SUMMARIZING TRENDS IN NDVI ACROSS SITES =========================================
mk.site.models <- function(df){
  zyp.yuepilon(df$ndvi.avg, df$year.rsc)
}

mk.smry <- function(x){
  data.frame(slope=round(x['trend'],5), slope.lbound=round(x['lbound'],5), slope.ubound=round(x['ubound'],5),
             total.change=round(x['trendp'],5),int=round(x['intercept'],5), tau=round(x['tau'],3), pval=round(x['sig'],4))
}

# COMPUTE MANN-KENDALL TREND FOR EACH SITE (1985-2017 and 2000 - 2017) ====================================================
site.ndvi.trnd.mdl <- dlply(site.ndvi.ts.dt.periods, .(period, site), .fun=mk.site.models)
site.ndvi.trnd.dt <- ldply(site.ndvi.trnd.mdl, function(x) mk.smry(x))

# categorize trends 
site.ndvi.trnd.dt$total.change.pcnt <- site.ndvi.trnd.dt$total.change / site.ndvi.trnd.dt$int * 100 # total percent change relative to 1985 
site.ndvi.trnd.dt$sig <- cut(site.ndvi.trnd.dt$pval, c(-Inf, 0.0500, 0.1000, Inf), c('sig.p5','sig.p10','insig'))
site.ndvi.trnd.dt$slope.cat <- cut(site.ndvi.trnd.dt$slope, c(-Inf, 0, Inf), c('browning','greening'))
site.ndvi.trnd.dt$trend.cat <- paste(site.ndvi.trnd.dt$slope.cat, site.ndvi.trnd.dt$sig, sep = '.')
site.ndvi.trnd.dt$trend.cat[grep('insig', site.ndvi.trnd.dt$trend.cat)] <- 'insig'


# ADD LANDSAT MEADATA TO TRENDS ===============================================
site.ndvi.trnd.dt <- left_join(site.ndvi.trnd.dt, site.smry.dt, by = 'site')


# WRITE OUTPUT TO FILE====================================================
mkdirs('output/ndvi_trends/')
site.ndvi.trnd.dt$rep <- i
outfile <- paste0('output/ndvi_trends/boreal_site_lsat_ndvi_trends_rep_',i,'.csv')
fwrite(site.ndvi.trnd.dt, outfile)

# END SCRIPT ============================================================================================