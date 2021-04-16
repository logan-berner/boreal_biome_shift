# DESCRIPTION ==========================================================================
# THIS R SCRIPT CREATES BWPLOTS OF CLIMATE CONDITIONS IN AREAS WITH CONTRASTING LANDSAT NDVI TRENDS

# SETUP ==========================================================================
rm(list=ls())
require(data.table)
require(dplyr)
require(ggplot2)

setwd('C:/Users/lb968/Google Drive/research/nau/nasa_above/boreal_ndvi/')
# setwd('C:/Users/Logan/Google Drive/research/nau/nasa_above/boreal_ndvi/')
source('code/fun_ggplot_stat_boxplot.R')

# READ DATA, FACTORS, SUBSETS ==========================================================================
ndvi.trnd.site <- fread('output/boreal_site_lsat_ndvi_trends.csv')

ndvi.trnd.site$trend.cat[ndvi.trnd.site$trend.cat == 'browning.sig.p10'] <- 'neg.'
ndvi.trnd.site$trend.cat[ndvi.trnd.site$trend.cat == 'browning.sig.p5'] <- 'neg.'
ndvi.trnd.site$trend.cat[ndvi.trnd.site$trend.cat == 'insig'] <- 'none'
ndvi.trnd.site$trend.cat[ndvi.trnd.site$trend.cat == 'greening.sig.p5'] <- 'pos.'
ndvi.trnd.site$trend.cat[ndvi.trnd.site$trend.cat == 'greening.sig.p10'] <- 'pos.'


ndvi.trnd.site[period == '1985-2017', tmax.jja.period.avg.C := tmax.1985to2017.jja.avg.C]
ndvi.trnd.site[period == '2000-2017', tmax.jja.period.avg.C := tmax.2000to2017.jja.avg.C]
ndvi.trnd.site[period == '1985-2017', vpd.jja.period.avg.kPa := vpd.1985to2017.jja.avg.kPa]
ndvi.trnd.site[period == '2000-2017', vpd.jja.period.avg.kPa := vpd.2000to2017.jja.avg.kPa]
ndvi.trnd.site[period == '1985-2017', ppt.wy.period.avg.mm := ppt.1985to2017.wy.avg.mm]
ndvi.trnd.site[period == '2000-2017', ppt.wy.period.avg.mm := ppt.2000to2017.wy.avg.mm]
ndvi.trnd.site[period == '1985-2017', def.wy.period.avg.mm := def.1985to2017.wy.avg.mm]
ndvi.trnd.site[period == '2000-2017', def.wy.period.avg.mm := def.2000to2017.wy.avg.mm]



# ndvi.trnd.site <- ndvi.trnd.site %>% mutate(trend.cat = factor(trend.cat, levels = c('browning.sig.p5','browning.sig.p10','insig','greening.sig.p10','greening.sig.p5'),
#                                                                labels = c('browning\n(p<0.05)','browning\n(p<0.10)','no trend\n(p>0.10)','greening\n(p<0.10)','greening\n(p<0.05)')))
# ndvi.trnd.site <- ndvi.trnd.site %>% mutate(trend.cat = factor(trend.cat, levels = c('browning.sig.p5','browning.sig.p10','insig','greening.sig.p10','greening.sig.p5'),
#                                                                labels = c('negative\n(p<0.05)','negative\n(p<0.10)','no trend\n(p>0.10)','positive\n(p<0.10)','positive\n(p<0.05)')))
# ndvi.trnd.site <- ndvi.trnd.site %>% mutate(trend.cat = factor(trend.cat, levels = c('browning.sig.p5','browning.sig.p10','insig','greening.sig.p10','greening.sig.p5'),
#                                                                labels = c('negative\n(strong)','negative\n(weak)','no trend','positive\n(weak)','positive\n(strong)')))
# ndvi.trnd.site <- ndvi.trnd.site %>% mutate(trend.cat = factor(trend.cat, levels = c('browning.sig.p5','browning.sig.p10','insig','greening.sig.p10','greening.sig.p5'),
#                                                                labels = c('s. neg','w. neg','none','w. pos','s. pos)')))


# consolidate Mosaic and Mixed Forest types
ndvi.trnd.site.lc <- ndvi.trnd.site
# ndvi.trnd.site.lc$landcov.name[ndvi.trnd.site.lc$landcov.name == 'Mosaic'] <- 'MF'
ndvi.trnd.site.lc <- ndvi.trnd.site.lc %>% filter(landcov.name == 'DNF' | landcov.name == 'ENF' | landcov.name == 'DBF' | landcov.name == 'MF')
ndvi.trnd.site.lc <- ndvi.trnd.site.lc %>% mutate(landcov.name = factor(landcov.name, levels = c('ENF','DNF','DBF','MF')))
ndvi.trnd.site.lc <- ndvi.trnd.site.lc %>% mutate(landcov.name = factor(landcov.name, labels = c('Evergreen Needleleaf', 'Deciduous Needleleaf','Deciduous Broadleaf','Mixed')))

ndvi.trnd.site.lc <- data.table(ndvi.trnd.site.lc)

# PLOTTING PARAMETERS ==========================================================================
ylab.tmax <- expression('Mean summer high temperature ('~degree~'C)')
ylab.vpd <-  expression('Mean summer VPD (kPa)')
ylab.def <-  expression('Mean water year deficit (mm)')
ylab.ppt <- expression('Mean water year precipitation (mm)')

xlab.trnd <- 'Landsat NDVI trend'

# trend.cols <- adjustcolor(c('lightsalmon4','lightsalmon3','gray50','springgreen3','springgreen4'), 1)
trend.cols <- c('lightsalmon4','gray50','springgreen4')
my.cex = 1.25

# PLOT: TMAX BY TREND ==========================================================================
tmax.fig <- ggplot() + stat_boxplot_custom(data=ndvi.trnd.site, aes(x=trend.cat, y=tmax.jja.period.avg.C), 
                                           qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='errorbar', width = 0.2)
tmax.fig <- tmax.fig + stat_boxplot_custom(data=ndvi.trnd.site, aes(x=trend.cat, y=tmax.jja.period.avg.C, fill=trend.cat), 
                                           qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='boxplot', width = 0.7, notch=TRUE, outlier.shape=NA)
tmax.fig <- tmax.fig + facet_grid(cols = vars(period)) 
tmax.fig <- tmax.fig + theme_bw() + scale_fill_manual(values=trend.cols) + labs(y=ylab.tmax, x=xlab.trnd)
tmax.fig <- tmax.fig + coord_cartesian(ylim=c(16, 26)) + scale_y_continuous(breaks=seq(16,26,2))
tmax.fig <- tmax.fig + theme(legend.position="none", strip.background = element_rect("gray90"), strip.text = element_text(size=12),
                             axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 
tmax.fig

jpeg('figures/clim_tmax_vs_lsat_ndvi_trend.jpg', width = 5, height = 4, res = 400, units = 'in')
tmax.fig
dev.off()

# by forest type
tmax.lc.fig <- ggplot() + stat_boxplot_custom(data=ndvi.trnd.site.lc, aes(x=trend.cat, y=tmax.jja.period.avg.C), 
                                              qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='errorbar', width = 0.2, )
tmax.lc.fig <- tmax.lc.fig + stat_boxplot_custom(data=ndvi.trnd.site.lc, aes(x=trend.cat, y=tmax.jja.period.avg.C, fill=trend.cat), 
                                                 qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='boxplot', width = 0.7, notch=TRUE, outlier.shape=NA)
tmax.lc.fig <- tmax.lc.fig + facet_grid(landcov.name ~ period) 
tmax.lc.fig <- tmax.lc.fig + theme_bw() + scale_fill_manual(values=trend.cols) + labs(y=ylab.tmax, x=xlab.trnd)
tmax.lc.fig <- tmax.lc.fig + coord_cartesian(ylim=c(14, 26)) + scale_y_continuous(breaks=seq(14,26,3))
tmax.lc.fig <- tmax.lc.fig + theme(legend.position="none", strip.background = element_rect("gray90"), strip.text = element_text(size=12),
                                   axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 
tmax.lc.fig

jpeg('figures/clim_tmax_vs_lsat_ndvi_trend_by_landcov.jpg', width = 5, height = 6, res = 400, units = 'in')
tmax.lc.fig
dev.off()

# by forest type for 1985-2017
tmax.lc.fig <- ggplot() + stat_boxplot_custom(data=subset(ndvi.trnd.site.lc, period=='1985-2017'), aes(x=trend.cat, y=tmax.jja.period.avg.C), 
                                              qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='errorbar', width = 0.2, )
tmax.lc.fig <- tmax.lc.fig + stat_boxplot_custom(data=subset(ndvi.trnd.site.lc, period=='1985-2017'), aes(x=trend.cat, y=tmax.jja.period.avg.C, fill=trend.cat), 
                                                 qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='boxplot', width = 0.7, notch=TRUE, outlier.shape=NA)
tmax.lc.fig <- tmax.lc.fig + facet_grid(. ~ landcov.name) 
tmax.lc.fig <- tmax.lc.fig + theme_bw() + scale_fill_manual(values=trend.cols) + labs(y=ylab.tmax, x=xlab.trnd)
tmax.lc.fig <- tmax.lc.fig + coord_cartesian(ylim=c(14, 26)) + scale_y_continuous(breaks=seq(14,26,3))
tmax.lc.fig <- tmax.lc.fig + theme(legend.position="none", strip.background = element_rect("gray90"), strip.text = element_text(size=10),
                                   axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 
tmax.lc.fig

jpeg('figures/clim_tmax_vs_lsat_ndvi_trend_by_landcov_1985to2017.jpg', width = 7, height = 6, res = 400, units = 'in')
tmax.lc.fig
dev.off()



# PLOT: VPD BY TREND ==========================================================================
vpd.fig <- ggplot() + stat_boxplot_custom(data=ndvi.trnd.site, aes(x=trend.cat, y=vpd.jja.period.avg.kPa), 
                                          qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='errorbar', width = 0.2)
vpd.fig <- vpd.fig + stat_boxplot_custom(data=ndvi.trnd.site, aes(x=trend.cat, y=vpd.jja.period.avg.kPa, fill=trend.cat), 
                                         qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='boxplot', width = 0.7, notch=TRUE, outlier.shape=NA)
vpd.fig <- vpd.fig + facet_grid(cols = vars(period)) 
vpd.fig <- vpd.fig + theme_bw() + scale_fill_manual(values=trend.cols) + labs(y=ylab.vpd, x=xlab.trnd)
vpd.fig <- vpd.fig + coord_cartesian(ylim=c(0.4, 1.05)) + scale_y_continuous(breaks=seq(0.4,1,0.2))
vpd.fig <- vpd.fig + theme(legend.position="none", strip.background = element_rect("gray90"), strip.text = element_text(size=12), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 
vpd.fig

jpeg('figures/clim_vpd_vs_lsat_ndvi_trend.jpg', width = 5, height = 4, res = 400, units = 'in')
vpd.fig
dev.off()


# by forest type
vpd.lc.fig <- ggplot() + stat_boxplot_custom(data=ndvi.trnd.site.lc, aes(x=trend.cat, y=vpd.jja.period.avg.kPa), 
                                          qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='errorbar', width = 0.2)
vpd.lc.fig <- vpd.lc.fig + stat_boxplot_custom(data=ndvi.trnd.site.lc, aes(x=trend.cat, y=vpd.jja.period.avg.kPa, fill=trend.cat), 
                                         qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='boxplot', width = 0.7, notch=TRUE, outlier.shape=NA)
vpd.lc.fig <- vpd.lc.fig + facet_grid(landcov.name ~ period) 
vpd.lc.fig <- vpd.lc.fig + theme_bw() + scale_fill_manual(values=trend.cols) + labs(y=ylab.vpd, x=xlab.trnd)
vpd.lc.fig <- vpd.lc.fig + coord_cartesian(ylim=c(0.42, 1.05)) + scale_y_continuous(breaks=seq(0.4,1,0.2))
vpd.lc.fig <- vpd.lc.fig + theme(legend.position="none", strip.background = element_rect("gray90"), strip.text = element_text(size=12), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 
vpd.lc.fig

jpeg('figures/clim_vpd_vs_lsat_ndvi_trend_by_landcov.jpg', width = 5, height = 6, res = 400, units = 'in')
vpd.lc.fig
dev.off()


# PLOT: DEFICIT BY TREND ==========================================================================
def.fig <- ggplot() + stat_boxplot_custom(data=ndvi.trnd.site, aes(x=trend.cat, y=def.wy.period.avg.mm), 
                                          qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='errorbar', width = 0.2)
def.fig <- def.fig + stat_boxplot_custom(data=ndvi.trnd.site, aes(x=trend.cat, y=def.wy.period.avg.mm, fill=trend.cat), 
                                         qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='boxplot', width = 0.7, notch=TRUE, outlier.shape=NA)
def.fig <- def.fig + facet_grid(cols = vars(period)) 
def.fig <- def.fig + theme_bw() + scale_fill_manual(values=trend.cols) + labs(y=ylab.def, x=xlab.trnd)
def.fig <- def.fig + coord_cartesian(ylim=c(0, 200)) + scale_y_continuous(breaks=seq(0,200,50))
def.fig <- def.fig + theme(legend.position="none", strip.background = element_rect("gray90"), strip.text = element_text(size=12), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 
def.fig

jpeg('figures/clim_def_vs_lsat_ndvi_trend.jpg', width = 5, height = 4, res = 400, units = 'in')
def.fig
dev.off()


# by forest type
def.lc.fig <- ggplot() + stat_boxplot_custom(data=ndvi.trnd.site.lc, aes(x=trend.cat, y=def.wy.period.avg.mm), 
                                             qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='errorbar', width = 0.2)
def.lc.fig <- def.lc.fig + stat_boxplot_custom(data=ndvi.trnd.site.lc, aes(x=trend.cat, y=def.wy.period.avg.mm, fill=trend.cat), 
                                               qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='boxplot', width = 0.7, notch=TRUE, outlier.shape=NA)
def.lc.fig <- def.lc.fig + facet_grid(landcov.name ~ period) 
def.lc.fig <- def.lc.fig + theme_bw() + scale_fill_manual(values=trend.cols) + labs(y=ylab.def, x=xlab.trnd)
def.lc.fig <- def.lc.fig + coord_cartesian(ylim=c(0, 250)) + scale_y_continuous(breaks=seq(0,250,50))
def.lc.fig <- def.lc.fig + theme(legend.position="none", strip.background = element_rect("gray90"), strip.text = element_text(size=12), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 
def.lc.fig

jpeg('figures/clim_def_vs_lsat_ndvi_trend_by_landcov.jpg', width = 5, height = 6, res = 400, units = 'in')
def.lc.fig
dev.off()



# PLOT: ppt BY TREND ==========================================================================
ppt.fig <- ggplot() + stat_boxplot_custom(data=ndvi.trnd.site, aes(x=trend.cat, y=ppt.wy.period.avg.mm), 
                                          qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='errorbar', width = 0.2)
ppt.fig <- ppt.fig + stat_boxplot_custom(data=ndvi.trnd.site, aes(x=trend.cat, y=ppt.wy.period.avg.mm, fill=trend.cat), 
                                         qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='boxplot', width = 0.7, notch=TRUE, outlier.shape=NA)
ppt.fig <- ppt.fig + facet_grid(cols = vars(period)) 
ppt.fig <- ppt.fig + theme_bw() + scale_fill_manual(values=trend.cols) + labs(y=ylab.ppt, x=xlab.trnd)
ppt.fig <- ppt.fig + scale_y_continuous(limits=c(200,1200), breaks=seq(200,1200,300))
ppt.fig <- ppt.fig + theme(legend.position="none", strip.background = element_rect("gray90"), strip.text = element_text(size=12), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 
ppt.fig

jpeg('figures/clim_ppt_vs_lsat_ndvi_trend.jpg', width = 5, height = 4, res = 400, units = 'in')
ppt.fig
dev.off()

# by forest type
ppt.lc.fig <- ggplot() + stat_boxplot_custom(data=ndvi.trnd.site.lc, aes(x=trend.cat, y=ppt.wy.period.avg.mm), 
                                             qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='errorbar', width = 0.2, )
ppt.lc.fig <- ppt.lc.fig + stat_boxplot_custom(data=ndvi.trnd.site.lc, aes(x=trend.cat, y=ppt.wy.period.avg.mm, fill=trend.cat), 
                                               qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='boxplot', width = 0.7, notch=TRUE, outlier.shape=NA)
ppt.lc.fig <- ppt.lc.fig + facet_grid(landcov.name ~ period) 
ppt.lc.fig <- ppt.lc.fig + theme_bw() + scale_fill_manual(values=trend.cols) + labs(y=ylab.ppt, x=xlab.trnd)
ppt.lc.fig <- ppt.lc.fig + scale_y_continuous(limits=c(200,1200), breaks=seq(200,1200,300))
ppt.lc.fig <- ppt.lc.fig + theme(legend.position="none", strip.background = element_rect("gray90"), strip.text = element_text(size=12), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 
ppt.lc.fig

jpeg('figures/clim_ppt_vs_lsat_ndvi_trend_by_landcov.jpg', width = 5, height = 6, res = 400, units = 'in')
ppt.lc.fig
dev.off()


# by forest type
ppt.lc.fig <- ggplot() + stat_boxplot_custom(data=ndvi.trnd.site.lc[period == '1985-2017'], aes(x=trend.cat, y=ppt.wy.period.avg.mm), 
                                             qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='errorbar', width = 0.2, )
ppt.lc.fig <- ppt.lc.fig + stat_boxplot_custom(data=ndvi.trnd.site.lc[period == '1985-2017'], aes(x=trend.cat, y=ppt.wy.period.avg.mm, fill=trend.cat), 
                                               qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='boxplot', width = 0.7, notch=TRUE, outlier.shape=NA)
ppt.lc.fig <- ppt.lc.fig + facet_grid(. ~ landcov.name) 
ppt.lc.fig <- ppt.lc.fig + theme_bw() + scale_fill_manual(values=trend.cols) + labs(y=ylab.ppt, x=xlab.trnd)
ppt.lc.fig <- ppt.lc.fig + scale_y_continuous(limits=c(200,1200), breaks=seq(200,1200,300))
ppt.lc.fig <- ppt.lc.fig + theme(legend.position="none", strip.background = element_rect("gray90"), strip.text = element_text(size=10), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 
ppt.lc.fig

jpeg('figures/clim_ppt_vs_lsat_ndvi_trend_by_landcov_1985to2017.jpg', width = 7, height = 6, res = 400, units = 'in')
ppt.lc.fig
dev.off()
