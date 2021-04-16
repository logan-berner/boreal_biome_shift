# This R script takes Landsat data downloaded from GEE and then post-processess the data to 
# estimate summer NDVI for 10,000 sites randomly located in the boreal biome.
# Date: 2019-06-20
rm(list=ls())
require(tidyr)
require(dplyr)
require(lattice)
require(latticeExtra)
require(data.table)

setwd('C:/Users/lb968/Google Drive/research/nau/nasa_above/boreal_ndvi/')
# setwd('C:/Users/Logan/Google Drive/research/nau/nasa_above/boreal_ndvi/')

site.ts.dt <- fread('output/boreal_site_lsat_ja_vi_timeseries.csv')

#--------------------------------------------------------------------------------------------------------
# ANNUAL MEAN LANDSAT NDVI AND VCF TREE COVER FOR THE BOREAL BIOME
#--------------------------------------------------------------------------------------------------------
biome.yrly.gte1985.dt <- site.ts.dt[first.yr <= 1986 & year >= 1985, .(ndvi.med = median(ndvi.avg, na.rm=T), tree.med = median(vcf.tree, na.rm=T)), 
                                    by = 'year'][order(year)]

ndvi.biome.yrly.gte1985.fig <- xyplot(ndvi.med ~ year , biome.yrly.gte1985.dt, type='b',
                                    ylab = 'Median July-Aug Landsat NDVI', xlab = 'Year', auto.key=T, scales = list(y=list(relation='free')))

tree.biome.yrly.gte1985.fig <- xyplot(tree.med ~ year, biome.yrly.gte1985.dt, type='b',
                                    ylab = 'Median AVHRR Tree Cover (%)', xlab = 'Year', auto.key=T, scales = list(y=list(relation='free')))

biome.yrly.gte2000.dt <- site.ts.dt[year >= 2000, .(ndvi.med = median(ndvi.avg, na.rm=T), tree.med = median(vcf.tree, na.rm=T)), 
                                    by = 'year'][order(year)]

ndvi.biome.yrly.gte2000.fig <- xyplot(ndvi.med ~ year , biome.yrly.gte2000.dt, type='b',
                                      ylab = 'Median July-Aug Landsat NDVI', xlab = 'Year', auto.key=T, scales = list(y=list(relation='free')))

tree.biome.yrly.gte2000.fig <- xyplot(tree.med ~ year, biome.yrly.gte2000.dt, type='b',
                                      ylab = 'Median AVHRR Tree Cover (%)', xlab = 'Year', auto.key=T, scales = list(y=list(relation='free')))

ndvi.biome.yrly.gte1985.fig
tree.biome.yrly.gte1985.fig

ndvi.biome.yrly.gte2000.fig
tree.biome.yrly.gte2000.fig


jpeg('figures/lsat_ndvi_biome_timeseries_1985to2016.jpg', 5, 5, units = 'in', res = 400)
ndvi.biome.yrly.gte1985.fig
dev.off()

jpeg('figures/avhrr_vcf_tree_biome_timeseries_1985to2016.jpg', 5, 5, units = 'in', res = 400)
tree.biome.yrly.gte1985.fig
dev.off()




#--------------------------------------------------------------------------------------------------------
# ANNUAL MEAN LANDSAT NDVI AND VCF TREE COVER BY ECOREGION
#--------------------------------------------------------------------------------------------------------
site.ts.dt <- na.omit(site.ts.dt)

lc.yrly.gte1985.dt <- site.ts.dt[first.yr <= 1986 & year >= 1985, .(ndvi.med = median(ndvi.avg, na.rm=T), ndvi.q95 = quantile(ndvi.avg, 0.95), ndvi.q05 = quantile(ndvi.avg, 0.05)), 
                                 by = c('landcov.name','year')][order(landcov.name, year)]


lc.fig <- ggplot(lc.yrly.gte1985.dt, aes(x=year, y=ndvi.med)) + facet_grid(cols = vars(landcov.name)) 
lc.fig <- lc.fig + geom_ribbon(aes(ymin = ndvi.q05, ymax=ndvi.q95)) + geom_line(col='gray')
lc.fig <- lc.fig + theme_bw() + labs(y="Meadian summer NDVI", x="Year")
lc.fig

tmax.fig <- tmax.fig + coord_cartesian(ylim=c(16, 26)) + scale_y_continuous(breaks=seq(16,26,2))
tmax.fig <- tmax.fig + theme(legend.position="none", strip.background = element_rect("gray90"), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 



#--------------------------------------------------------------------------------------------------------
# ANNUAL MEAN LANDSAT NDVI AND VCF TREE COVER BY ECOREGION
#--------------------------------------------------------------------------------------------------------
eco.yrly.gte1985.dt <- site.ts.dt[first.yr <= 1986 & year >= 1985, .(ndvi.med = median(ndvi.avg, na.rm=T), tree.med = median(vcf.tree, na.rm=T)), 
                                      by = c('eco.name','year')][order(eco.name, year)]

ndvi.eco.yrly.gte1985.fig <- xyplot(ndvi.med ~ year | eco.name, eco.yrly.gte1985.dt, type='b',
                                 ylab = 'Median July-Aug Landsat NDVI', xlab = 'Year', auto.key=T, scales = list(y=list(relation='free')))

tree.eco.yrly.gte1985.fig <- xyplot(tree.med ~ year | eco.name, eco.yrly.gte1985.dt, type='b',
                                    ylab = 'Median AVHRR Tree Cover (%)', xlab = 'Year', auto.key=T, scales = list(y=list(relation='free')))

ndvi.eco.yrly.gte1985.fig
tree.eco.yrly.gte1985.fig

jpeg('figures/lsat_ndvi_ecoreg_timeseries_1985to2016.jpg', 25, 10, units = 'in', res = 400)
ndvi.eco.yrly.gte1985.fig
dev.off()

jpeg('figures/avhrr_vcf_tree_ecoreg_timeseries_1985to2016.jpg', 25, 10, units = 'in', res = 400)
tree.eco.yrly.gte1985.fig
dev.off()

#--------------------------------------------------------------------------------------------------------
# ANNUAL median LANDSAT NDVI AND VCF TREE COVER BY ECOREGION X INTACTNESS
#--------------------------------------------------------------------------------------------------------
eco.intact.yrly.gte1985.dt <- site.ts.dt[first.yr <= 1986 & year >= 1985 & year <= 2016, .(ndvi.med = median(ndvi.avg, na.rm=T), tree.med = median(vcf.tree, na.rm=T)), 
                                by = c('eco.name','intact','year')][order(eco.name, intact, year)]

ndvi.eco.intact.yrly.gte1985.fig <- xyplot(ndvi.med ~ year | eco.name, eco.intact.yrly.gte1985.dt, groups = eco.intact.yrly.gte1985.dt$intact, type='b',
                                    ylab = 'Median July-Aug Landsat NDVI', xlab = 'Year', auto.key=T, scales = list(y=list(relation='free')))

tree.eco.intact.yrly.gte1985.fig <- xyplot(tree.med ~ year | eco.name, eco.intact.yrly.gte1985.dt, groups = eco.intact.yrly.gte1985.dt$intact, type='b',
                                    ylab = 'Median AVHRR Tree Cover (%)', xlab = 'Year', auto.key=T, scales = list(y=list(relation='free')))

ndvi.eco.intact.yrly.gte1985.fig
tree.eco.intact.yrly.gte1985.fig

jpeg('figures/lsat_ndvi_ecoreg_intactness_timeseries_1985to2016.jpg', 25, 10, units = 'in', res = 400)
ndvi.eco.intact.yrly.gte1985.fig
dev.off()

jpeg('figures/avhrr_vcf_tree_ecoreg_intactness_timeseries_1985to2016.jpg', 25, 10, units = 'in', res = 400)
tree.eco.intact.yrly.gte1985.fig
dev.off()

#--------------------------------------------------------------------------------------------------------
# EXPLORATION
#--------------------------------------------------------------------------------------------------------

lsat.eco.yrly.avg <- site.ts.dt[,.(ndvi.avg = mean(ndvi.avg, na.rm=T)), by = c('eco.name','year')]

xyplot(ndvi.avg ~ year | eco.name, lsat.eco.yrly.avg, type=c('b','r'),  
       auto.key=T, scales = list(y=list(relation='free')))

lsat.eco.yrly.avg <- site.ts.dt[year >= 1999,.(ndvi.avg = mean(ndvi.avg, na.rm=T)), by = c('eco.name','year')]

xyplot(ndvi.avg ~ year | eco.name, lsat.eco.yrly.avg, type=c('b','r'),  
       auto.key=T, scales = list(y=list(relation='free')))



lsat.eco.wild.yrly.avg <- site.ts.dt[year >= 1999,.(ndvi.avg = mean(ndvi.avg, na.rm=T)), by = c('eco.name','wild','year')]
xyplot(ndvi.avg ~ year | eco.name, lsat.eco.wild.yrly.avg, groups=lsat.eco.wild.yrly.avg$wild, type=c('b','r'),  
       auto.key=T, scales = list(y=list(relation='free')))


lsat.eco.wild.yrly.avg <- site.ts.dt[,.(ndvi.avg = mean(ndvi.avg, na.rm=T)), by = c('eco.name','wild','year')]
xyplot(ndvi.avg ~ year | eco.name, lsat.eco.wild.yrly.avg, groups=lsat.eco.wild.yrly.avg$wild, type=c('b','r'),
       auto.key=T, scales = list(y=list(relation='free')))


lsat.eco.tte.yrly.avg <- site.ts.dt[,.(ndvi.avg = mean(ndvi.avg, na.rm=T)), by = c('eco.name','tte.class','year')]
xyplot(ndvi.avg ~ year | eco.name, lsat.eco.tte.yrly.avg, groups=lsat.eco.tte.yrly.avg$tte.class, type='b',  
       auto.key=T, scales = list(y=list(relation='free')))

lsat.tte.yrly.avg <- site.ts.dt[,.(ndvi.avg = mean(ndvi.avg, na.rm=T)), by = c('tte.class','year')]
xyplot(ndvi.avg ~ year | tte.class, lsat.tte.yrly.avg, type=c('b','r'),  
       auto.key=T, scales = list(y=list(relation='free')))


lsat.tte.yrly.avg <- site.ts.dt[year>1999,.(nirv.avg = mean(nirv.avg, na.rm=T)), by = c('tte.class','year')]
xyplot(nirv.avg ~ year | tte.class, lsat.tte.yrly.avg, type=c('b','r'),  
       auto.key=T, scales = list(y=list(relation='free')))
