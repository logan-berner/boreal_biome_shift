# This R script takes Landsat data downloaded from GEE and then post-processess the data to 
# estimate summer NDVI for 50,000 random sites in the boreal biome.
# Date: 2020-12-01
version
rm(list=ls())
# .libPaths(c(.libPaths(), "~/R/", '/home/lb968/R/3.6', '/home/lb968/.conda/envs/rstudio_env/lib/R/library'))
.libPaths(c(.libPaths(), "~/R/", "/home/lb968/R/4.0.2/"))
require(data.table)
require(R.utils)
require(ranger)
require(zoo)
require(ggplot2)
require(ggpubr)

setwd('/projects/arctic/users/lberner/boreal_biome_shift/')
source('/home/lb968/code/boreal_biome_shift/0.1_fun_lsat_tools.R')

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

# LOAD CLEANED DATA SET ============================================================================
lsat <- fread('data/lsat_samples/boreal_lsat_samples_cleaned.csv')


# RANDOMLY SUBSAMPLE SITES AND OBS WITHIN SITES ============================================================================
# frac.sample <- 0.9
# 
# # subset sites
# sites <- unique(lsat$site)
# n.sites <- length(sites)
# n.samples <- n.sites * frac.sample
# sample.sites <- sample(sites, n.samples)
# lsat <- lsat[lsat$site %in% sample.sites]
# 
# # subset obs w/in sites
# lsat <- lsat[, n.obs.site := .N, by = 'site'] # calc number of obs at each site
# lsat <- lsat[, n.obs.subset := n.obs.site * frac.sample] # calc number of obs at each site
# lsat <- lsat[, .SD[sample(.N, n.obs.subset)], by = 'site']


# PERMUTE REFLECTANCE AND CALC NDVI ================================================================
# sensor specific uncertanity from Markham and Helder (2012, RSE) and Markham et al. (2014; RS)
LT05.red.scalers <- runif(nrow(lsat[satellite == 'LT05']), -0.07, 0.07)
LT05.nir.scalers <- runif(nrow(lsat[satellite == 'LT05']), -0.07, 0.07)
LE07.red.scalers <- runif(nrow(lsat[satellite == 'LE07']), -0.05, 0.05)
LE07.nir.scalers <- runif(nrow(lsat[satellite == 'LE07']), -0.05, 0.05)
LC08.red.scalers <- runif(nrow(lsat[satellite == 'LC08']), -0.03, 0.03)
LC08.nir.scalers <- runif(nrow(lsat[satellite == 'LC08']), -0.03, 0.03)

lsat[satellite == 'LT05', nir := nir + nir * LT05.nir.scalers]
lsat[satellite == 'LT05', red := red + red * LT05.red.scalers]
lsat[satellite == 'LE07', nir := nir + nir * LE07.nir.scalers]
lsat[satellite == 'LE07', red := red + red * LE07.red.scalers]
lsat[satellite == 'LC08', nir := nir + nir * LC08.nir.scalers]
lsat[satellite == 'LC08', red := red + red * LC08.red.scalers]


# COMPUTE NDVI AND CROSS-CALIBRATE NDVI AMONG LANDSAT SENSORS =========================================================
print('starting calc and xcal ndvi...')
mkdirs('output/xcal_ndvi/')
mkdirs('output/xcal_evi2/')
lsat <- lsat_spec_index(lsat, 'ndvi')
lsat <- lsat_spec_index(lsat, 'evi2')

lsat <- lsat_xcal_rf(lsat, 'ndvi', doy.rng = 182:243, min.obs = 5, frac.eval = 0.25, outfile.prefix = paste0('ndvi_rep_',i), outdir = 'output/xcal_ndvi')
lsat <- lsat_xcal_rf(lsat, 'evi2', doy.rng = 182:243, min.obs = 5, frac.eval = 0.25, outfile.prefix = paste0('evi2_rep_',i), outdir = 'output/xcal_evi2')




# FILTER OUT BARREN SITES AND SITES WITH TOO FEW YEARS OF OBSERVATIONS  ================================================================ 
# subset barren sites
lsat <- lsat[, ndvi.xcal.avg := mean(ndvi.xcal, na.rm = T), by = site]
lsat.barren <- lsat[ndvi.xcal.avg < 0.10]
# fwrite(lsat.barren, 'output/tundra_barren_sites.csv')

length(unique(lsat.barren$site)) # number of barren sites
lsat <- lsat[ndvi.xcal.avg >= 0.10][ndvi.xcal >= 0.10]
length(unique(lsat$site)) # after excluding barren

# identify sites with too short a record 
lsat <- lsat[, n.yrs := length(unique(year)), by = site]
lsat.shortrecords <- lsat[n.yrs < 10]
lsat <- lsat[n.yrs >= 10]
length(unique(lsat$site)) # after excluding short records

# identify sites with too few observations
lsat <- lsat[, n.obs := .N, by = site]
lsat <- lsat[n.obs > 30]
length(unique(lsat$site)) # after excluding short records


# ESTIMATE ANNUAL MAXIMUM SUMMER NDVI ================================================================ 
print('starting pheno...')
# apply phenological correction
mkdirs('output/pheno_curves')
mkdirs('output/pheno_timeseries')

# derive phenological curves for each site
rep.spar <- 0.7 + runif(1,0.-0.02,0.02)
spl.outfile <- paste0('output/pheno_curves/pheno_curves_rep_',i,'.csv')
lsat.pheno <- lsat_pheno(lsat, 'ndvi.xcal', window.yrs = 17, window.min.obs = 20, spar = rep.spar, spl.fit.outfile = spl.outfile)
fwrite(lsat.pheno, paste0('output/pheno_timeseries/tundra_landsat_ndvi_pheno_corrected_timeseries_rep_',i,'.csv'))

# compute max NDVI
lsat.max <- lsat_pheno_max(lsat.pheno, vi = 'ndvi.xcal', min.frac.of.max = 0.75)

# evaluate estimates of max NDVI
lsat.max.eval <- lsat_pheno_max_eval(lsat.pheno, vi = 'ndvi.xcal', min.frac.of.max = 0.75, min.obs = 11, reps = 10, 
                                     outdir = 'output/pheno_max_eval/', outfile.suffix = paste0('rep_',i))

# rename some of the NDVI colunms
colnames(lsat.max) <- gsub('.pred','',gsub('.pred.min','.lower',gsub('.pred.max','.upper',gsub('.xcal','',colnames(lsat.max)))))

# DETERMINE OBS DENSITY ==============================================================================================================
# determine first year of obs and number of years with obs
lsat.max <- lsat.max[, first.yr := min(year, na.rm=T), by = site]
lsat.max <- lsat.max[, n.yr.obs := length(unique(year)), by = site]
length(unique(lsat.max$site))

# convert implicit NA to explicit NA (i.e., ensure all site x year combinations)
full.fac <- lsat.max %>% expand(site, year = 1982:2016)
full.fac <- data.table(full.fac)
lsat.max <- lsat.max[full.fac, on = c('site','year')]

length(unique(lsat.max$site))










# COMPUTE MEAN SUMMER VEG INDICES =====================================================
# compute mean NDVI  
lsat.avg <- lsat[, .(latitude = mean(latitude, na.rm=T), longitude = mean(longitude, na.rm=T),
                     ndvi.avg = mean(ndvi.xcal, na.rm=T), ndvi.sd = sd(ndvi.xcal, na.rm=T), n.scenes = .N), 
                 by = c('site','year')]

lsat.avg <- setorder(lsat.avg, site,year)


# DETERMINE FIRST YEAR OF OBS AND NUMBER OF YEARS WITH OBS ===================================================
lsat.avg[, first.yr := min(year, na.rm=T), by = site]
lsat.avg[, n.yr.obs := length(unique(year)), by = site]
length(unique(lsat.avg$site))


# WRITE OUT ANNUAL SUMMER NDVI TIME SERIES FOR THE SITES ========================================================
mkdirs('output/ndvi_timeseries')
fwrite(lsat.avg, paste0('output/ndvi_timeseries/boreal_site_lsat_ja_vi_timeseries_rep_',i,'.csv'))

# END SCRIPT ========================================================================