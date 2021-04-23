# This R script takes Landsat data downloaded from GEE and then post-processess the data to 
# estimate annual NDVImax for 100,000 random sites in the boreal biome.
# Date: 2021-04-16
rm(list=ls())
.libPaths(c(.libPaths(), "~/R/", "/home/lb968/R/4.0.2/"))
require(data.table)

setwd('/projects/arctic/users/lberner/boreal_biome_shift/')

# combine data files 
data.files <- list.files('data/lsat_samples/cleaned/', pattern = 'cleaned.csv', full.names = T)
data.dt <- do.call("rbind", lapply(data.files, fread))
length(unique(data.dt$site))
fwrite(data.dt, 'data/lsat_samples/boreal_lsat_clean_data_100k_sites_1985to2016.csv')

# summarize number of multi-band measurements across all sampling sites
smry.files <- list.files('data/lsat_samples/cleaned/', pattern = 'summary.csv', full.names = T)
smry.dt <- do.call("rbind", lapply(smry.files, fread))
obs.dt <- smry.dt[, .(n.sites.all = sum(n.sites.all), n.sites.clear = sum(n.sites.clear), n.obs.all = sum(n.obs.all), n.obs.clear = sum(n.obs.clear))]
fwrite(obs.dt, 'output/boreal_lsat_sample_site_observation_summary.csv')

