# This R script takes Landsat data downloaded from GEE and then post-processess the data to 
# estimate summer NDVI for 10,000 sites randomly located in the boreal biome.
# Date: 2019-06-20
rm(list=ls())
require(tidyr)
require(dplyr)
require(lattice)
require(latticeExtra)
require(data.table)

# setwd('C:/Users/lb968/Google Drive/research/nau/nasa_above/boreal_ndvi/')
setwd('C:/Users/Logan/Google Drive/research/nau/nasa_above/boreal_ndvi/')

lsat.avg <- fread('output/boreal_site_lsat_ja_vi_timeseries.csv')

#--------------------------------------------------------------------------------------------------------
# MEAN LANDSAT NDVI VS VCF TREE COVER
#--------------------------------------------------------------------------------------------------------
eco.avg <- lsat.avg[year > 2000 & year < 2016,.(ndvi.avg = mean(ndvi.avg, na.rm=T), nirv.avg = mean(nirv.avg, na.rm=T),
                                                tree.avg = mean(vcf.tree, na.rm=T), shortveg.avg = mean(vcf.shortveg, na.rm=T)), 
                    by = 'eco.name']


ndvi.vs.treecov.fig <- xyplot(ndvi.avg ~ tree.avg, eco.avg, 
                              xlab = "Mean Ecoreg AVHRR VCF Tree Cover (%)", 
                              ylab='Mean Ecoreg. Landsat Summer NDVI', pch=16)
nirv.vs.treecov.fig <- xyplot(nirv.avg ~ tree.avg, eco.avg, 
                              xlab = "Mean Ecoreg AVHRR VCF Tree Cover (%)", 
                              ylab='Mean Ecoreg. Landsat Summer NIRV', pch=16)

ndvi.vs.shortveg.fig <- xyplot(ndvi.avg ~ shortveg.avg, eco.avg,
                               xlab = "Mean Ecoreg AVHRR VCF Short Veg. Cover (%)", 
                               ylab='Mean Ecoreg. Landsat Summer NDVI', pch=16)

nirv.vs.shortveg.fig <- xyplot(nirv.avg ~ shortveg.avg, eco.avg,
                               xlab = "Mean Ecoreg AVHRR VCF Short Veg. Cover (%)", 
                               ylab='Mean Ecoreg. Landsat Summer NIRV', pch=16)


#jpeg('figures/ecoreg_lsat_ndvi_vs_avhrr_treecov_mean2000to2016.jpg', 10, 10, units = 'in', res=400)
jpeg('figures/ecoreg_lsat_vi_vs_avhrr_vcf_mean2000to2016.jpg', 8, 8, units = 'in', res=400)
print(ndvi.vs.treecov.fig, position = c(0, 0, 0.5, 0.5), more=T)
print(ndvi.vs.shortveg.fig, position = c(0.5, 0, 1, 0.5), more=T)
print(nirv.vs.treecov.fig, position = c(0, 0.5, 0.5, 1), more=T)
print(nirv.vs.shortveg.fig, position = c(0.5, 0.5, 1, 1), more=T)
dev.off()

cor.test(eco.avg$ndvi.avg, eco.avg$tree.avg, method = 'pearson')
cor.test(eco.avg$nirv.avg, eco.avg$tree.avg, method = 'pearson')

cor.test(eco.avg$ndvi.avg, eco.avg$shortveg.avg, method = 'pearson')
cor.test(eco.avg$nirv.avg, eco.avg$shortveg.avg, method = 'pearson')
