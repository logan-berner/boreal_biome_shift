# This R script plots mean summer NDVI by land cover type
# Date: 2019-06-20
rm(list=ls())
require(data.table)
require(ggplot2)

setwd('C:/Users/lb968/Google Drive/research/nau/nasa_above/boreal_ndvi/')
# setwd('C:/Users/Logan/Google Drive/research/nau/nasa_above/boreal_ndvi/')
source('code/fun_ggplot_stat_boxplot.R')

lsat <- fread('output/boreal_site_lsat_ja_vi_timeseries.csv')

lsat <- lsat[landcov.name != ''][landcov.name != 'Barren']

# compute mean annual summer NDVI for each site
lsat.avg <- lsat[year >= 2008, .(ndvi.avg = mean(ndvi.avg, na.rm=T), landcov.name = first(landcov.name)), by = 'site']

# sort land cover classes by median of average ndvi
lsat.avg.med <- lsat.avg[, .(ndvi.avg.med = median(ndvi.avg, na.rm=T)), by = 'landcov.name']
lsat.avg.med <- setorder(lsat.avg.med, ndvi.avg.med)
lsat.avg$landcov.name <- factor(lsat.avg$landcov.name, levels = lsat.avg.med$landcov.name)

# Plot ----------------------------------------------------------------------
lab.ndvi <- "Mean summer NDVI (unitless)"
lab.lc <- 'Landcover'

my.cex = 1.25

# PLOT: TMAX BY TREND ==========================================================================
fig <- ggplot() + stat_boxplot_custom(data=lsat.avg, aes(x=landcov.name, y=ndvi.avg), 
                                           qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='errorbar', width = 0.2)
fig <- fig + stat_boxplot_custom(data=lsat.avg, aes(x=landcov.name, y=ndvi.avg, fill='gray50'), 
                                           qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='boxplot', width = 0.7, notch=TRUE, outlier.shape=NA)
fig <- fig + theme_bw() + scale_fill_manual(values='gray50') + labs(y=lab.ndvi, x=lab.lc)
fig <- fig + coord_cartesian(ylim=c(0, 1)) + scale_y_continuous(breaks=seq(0,1,0.25))
fig <- fig + theme(legend.position="none", strip.background = element_rect("gray90"), strip.text = element_text(size=12),
                             axis.text=element_text(size=12), axis.text.x = element_text(angle = 45), axis.title=element_text(size=14,face="bold")) 
fig

jpeg('figures/lsat_ndvi_avg_by_landcov.jpg', width = 5, height = 4, res = 400, units = 'in')
fig
dev.off()
