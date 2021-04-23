# This R script takes Landsat data downloaded from GEE and then post-processess the data to 
# estimate annual NDVImax for 100,000 random sites in the boreal biome.
# Date: 2021-04-16
version
rm(list=ls())
.libPaths(c(.libPaths(), "~/R/", "/home/lb968/R/4.0.2/"))
require(data.table)
require(R.utils)
require(lsatTS)

setwd('/projects/arctic/users/lberner/boreal_biome_shift/')

args <- commandArgs(TRUE)
i = as.numeric(args[1])
# i=1

# reformat i for sorting
if (i < 10){
  i <- paste0('000',i)
} else if (i < 100){
  i <- paste0('00',i)
} else if (i < 1000){
  i <- paste0('0',i)
}

# LOAD CLEANED DATA SET ============================================================================
lsat.dt <- fread('data/lsat_samples/boreal_lsat_clean_data_100k_sites_1985to2016.csv')
dim(lsat.dt)

# RANDOMLY SUBSAMPLE OBS WITHIN SITES ============================================================================
frac.sample <- 0.9
lsat.dt <- lsat.dt[, n.obs.site := .N, by = site] # calc number of obs at each site
lsat.dt <- lsat.dt[, n.obs.subset := n.obs.site * frac.sample] # calc number of obs at each site
lsat.dt <- lsat.dt[, .SD[sample(.N, n.obs.subset)], by = site]
lsat.dt <- lsat.dt[, c('n.obs.site','n.obs.subset') := NULL]
dim(lsat.dt)


# PERMUTE REFLECTANCE ================================================================
# sensor specific uncertanity from Markham and Helder (2012, RSE) and Markham et al. (2014; RS)
LT05.red.scalers <- runif(nrow(lsat.dt[satellite == 'LT05']), -0.07, 0.07)
LT05.nir.scalers <- runif(nrow(lsat.dt[satellite == 'LT05']), -0.07, 0.07)
LE07.red.scalers <- runif(nrow(lsat.dt[satellite == 'LE07']), -0.05, 0.05)
LE07.nir.scalers <- runif(nrow(lsat.dt[satellite == 'LE07']), -0.05, 0.05)
LC08.red.scalers <- runif(nrow(lsat.dt[satellite == 'LC08']), -0.03, 0.03)
LC08.nir.scalers <- runif(nrow(lsat.dt[satellite == 'LC08']), -0.03, 0.03)

lsat.dt[satellite == 'LT05', nir := nir + nir * LT05.nir.scalers]
lsat.dt[satellite == 'LT05', red := red + red * LT05.red.scalers]
lsat.dt[satellite == 'LE07', nir := nir + nir * LE07.nir.scalers]
lsat.dt[satellite == 'LE07', red := red + red * LE07.red.scalers]
lsat.dt[satellite == 'LC08', nir := nir + nir * LC08.nir.scalers]
lsat.dt[satellite == 'LC08', red := red + red * LC08.red.scalers]


# COMPUTE VEGETATION INDEX AND CROSS-CALIBRATE IT AMONG LANDSAT SENSORS ====================
print('Calculating and cross calibrating vegetation indices...')

vi.all <- c('ndvi','evi2','nirv','kndvi')
vi.rep <- sample(vi.all, 1)

lsat.dt <- lsat_calc_spec_index(lsat.dt, vi.rep)

lsat.dt <- lsat_calibrate_rf(lsat.dt, band = vi.rep, doy.rng = 152:243, min.obs = 5, frac.train = 0.75, 
                             outfile.id = paste0(vi.rep, '_rep_',i), outdir = 'output/xcal/', overwrite.col = T)


# ESTIMATE ANNUAL MAXIMUM SUMMER VEGETATION INDEX ================================================================ 
print('Estimating growing season characteristics...')

# derive phenological curves for each site
spar.rep <- runif(1, 0.68, 0.72)
win.yrs.rep <- sample(c(5,7,9), 1)
win.min.obs.rep <- sample(10:15, 1)

lsat.pheno.dt <- lsat_fit_phenological_curves(lsat.dt, vi.rep, window.yrs = win.yrs.rep, window.min.obs = win.min.obs.rep, spar = spar.rep, spl.fit.outfile = F)

# compute annual max VI
min.frac.of.max.rep <- runif(1, 0.65, 0.75)
lsat.gs.dt <- lsat_summarize_growing_seasons(lsat.pheno.dt, vi = vi.rep, min.frac.of.max = min.frac.of.max.rep)

# rename columns to 'vi' 
lsat.gs.dt[, vi.name := vi.rep]
colnames(lsat.gs.dt) <- gsub(vi.rep, 'vi', colnames(lsat.gs.dt))

# determine first year and number of years with observations 
lsat.gs.dt <- lsat.gs.dt[, first.yr := min(year, na.rm=T), by = site]
lsat.gs.dt <- lsat.gs.dt[, n.yr.obs := length(unique(year)), by = site]

# write out time series of annual growing season vegetation indices 
mkdirs('output/lsat_vi_gs_timeseries')
lsat.gs.dt[, rep := i]
fwrite(lsat.gs.dt, paste0('output/lsat_vi_gs_site_timeseries/boreal_lsat_vi_gs_site_timeseries_rep_',i,'.csv'))


print('All done!')
# END SCRIPT ========================================================================