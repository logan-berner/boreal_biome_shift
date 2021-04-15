#-------------------------------------------------------------------------------------------------------------------
# SUMMARIZE TRENDS IN LANDSAT NDVI BY LAND COVER TYPE
# AUTHOR: LOGAN BERNER, NAU
# DATE: 2020-02-11
#-------------------------------------------------------------------------------------------------------------------
rm(list=ls())
require(plyr)
require(dplyr)
require(data.table)
require(lattice)
require(reshape2)
require(R.utils)

# args <- commandArgs(TRUE)
# # i = as.numeric(args[1])
# i=1

setwd('/projects/above_gedi/lberner/boreal_ndvi')

infiles <- list.files('output/ndvi_trends/', full.names = T)[1:100]
n.files <- length(infiles)

lc.smry.mc.rep.lst <- list()

for (i in 1:n.files){
  # read in data file for a MC rep
  site.trnd.mc.rep.dt <- fread(infiles[i])
  
  # COMPUTE PROPORTION OF SITES IN EACH LAND COVER TYPE HAVE SPECIFIC TREND CATEGORIES (both 1984-2017 and 2000 - 2017 ) ==========================
  site.ndvi.trnd.cat.pcnt.by.lc <- site.trnd.mc.rep.dt %>% filter(trend.cat != 'NA.NA', landcov.name != '') %>% group_by(period, landcov.name, trend.cat, rep) %>% 
    tally(name = 'n.sites') %>% group_by(period, landcov.name) %>% mutate(rep = i, n.sites.lc = sum(n.sites),
                                                                           pcnt.sites = n.sites / n.sites.lc * 100,
                                                                           trend.cat = factor(trend.cat,
                                                                                              levels = c('browning.sig.p5','browning.sig.p10','insig','greening.sig.p10','greening.sig.p5'),
                                                                                              labels = c('browning (p<0.05)','browning (p<0.10)','no trend','greening (p<0.10)','greening (p<0.05'))) %>%
    arrange(period, landcov.name, trend.cat) %>% group_by(period, landcov.name) %>%
    mutate(pcnt.position = cumsum(pcnt.sites)-pcnt.sites/2, cnt.position = cumsum(n.sites)-n.sites/2)
  
  # biome
  site.ndvi.trnd.cat.pcnt.by.biome <- site.trnd.mc.rep.dt %>% filter(trend.cat != 'NA.NA') %>% group_by(period, trend.cat, rep) %>% tally(name='n.sites') %>%
    group_by(period) %>% mutate(landcov.name = 'All', n.sites.lc = sum(n.sites),
                                pcnt.sites = n.sites / n.sites.lc * 100,
                                trend.cat = factor(trend.cat,
                                                   levels = c('browning.sig.p5','browning.sig.p10','insig','greening.sig.p10','greening.sig.p5'),
                                                   labels = c('browning (p<0.05)','browning (p<0.10)','no trend','greening (p<0.10)','greening (p<0.05'))) %>%
    arrange(period, trend.cat) %>% group_by(period) %>%
    mutate(pcnt.position = cumsum(pcnt.sites)-pcnt.sites/2, cnt.position = cumsum(n.sites)-n.sites/2)
  
  site.ndvi.trnd.cat.pcnt.by.biome <- dplyr::select(site.ndvi.trnd.cat.pcnt.by.biome, period, everything())
  
  #combine lcs and biome
  site.ndvi.trnd.cat.pcnt.by.lc <- rbind(site.ndvi.trnd.cat.pcnt.by.lc, site.ndvi.trnd.cat.pcnt.by.biome)
  
  
  print(i/n.files)
}


fwrite(site.ndvi.trnd.cat.pcnt.by.lc, 'output/boreal_site_lsat_ndvi_trend_pcnt_by_landcov.csv', sep = ',', row.names = F, col.names = T)