# ASSESS CLIMATE CONDITIONS IN BOREAL AREAS WITH CONTRASTING LANDSAT NDVI TRENDS
# AUTHOR: LOGAN BERNER, NAU
# DATE: 2020-02-11

rm(list=ls())
require(dplyr)
require(data.table)
require(ggplot2)
require(reshape2)
require(R.utils)

setwd('/projects/above_gedi/lberner/boreal_ndvi')
source('/home/lb968/code/boreal_ndvi/fun_ggplot_stat_boxplot.R')


# READ ALL MC REPS INTO ONE DATA TABLE ============================================================
infiles <- list.files('output/ndvi_trends/', full.names = T)[1:200]
site.trnd.mc.rep.dt <- do.call("rbind", lapply(infiles, fread))

site.clim.dt<- fread('output/boreal_sample_site_climate_and_landcover.csv')

# CONSOLIDATE LANDSAT NDVI TREND CLASSES ==========================================================
site.trnd.mc.rep.dt$trend.cat[site.trnd.mc.rep.dt$trend.cat == 'browning.sig.p10'] <- 'neg.'
site.trnd.mc.rep.dt$trend.cat[site.trnd.mc.rep.dt$trend.cat == 'browning.sig.p5'] <- 'neg.'
site.trnd.mc.rep.dt$trend.cat[site.trnd.mc.rep.dt$trend.cat == 'insig'] <- 'none'
site.trnd.mc.rep.dt$trend.cat[site.trnd.mc.rep.dt$trend.cat == 'greening.sig.p5'] <- 'pos.'
site.trnd.mc.rep.dt$trend.cat[site.trnd.mc.rep.dt$trend.cat == 'greening.sig.p10'] <- 'pos.'


# DETERMINE SITES THAT 'CONFIDENTLY' GREENENED / BROWNED / STABLE ACROSS ALL MONTE CALRO REPS ===============
site.trnd.stable.dt <- site.trnd.mc.rep.dt[, .(n.cat = .N), by = c('site','period','trend.cat')]
site.trnd.stable.dt <- site.trnd.stable.dt[, n.tot := sum(n.cat), by = c('site','period')][, frac.cat := n.cat / n.tot]
site.trnd.stable.dt <- site.trnd.stable.dt[frac.cat >= 0.9]


# APPEND, RESCALE, AND RESTRUCTURE CLIMATE DATA ==================================================
site.trnd.stable.dt <- site.clim.dt[site.trnd.stable.dt, on='site']

site.trnd.stable.dt[period == '1985-2017', tmax.jja.period.avg.C := tmax.mean.1985to2017.jja.max.Cx100 / 100]
site.trnd.stable.dt[period == '2000-2017', tmax.jja.period.avg.C := tmax.mean.2000to2017.jja.max.Cx100 / 100]
site.trnd.stable.dt[period == '1985-2017', vpd.jja.period.avg.kPa := vpd.mean.1985to2017.jja.max.kPax100 / 100]
site.trnd.stable.dt[period == '2000-2017', vpd.jja.period.avg.kPa := vpd.mean.2000to2017.jja.max.kPax100 / 100]
site.trnd.stable.dt[period == '1985-2017', def.wy.period.avg.mm := def.mean.1985to2017.wy.sum.mm]
site.trnd.stable.dt[period == '2000-2017', def.wy.period.avg.mm := def.mean.2000to2017.wy.sum.mm]
site.trnd.stable.dt[period == '1985-2017', ppt.wy.period.avg.mm := ppt.mean.1985to2017.wy.sum.mm]
site.trnd.stable.dt[period == '2000-2017', ppt.wy.period.avg.mm := ppt.mean.2000to2017.wy.sum.mm]


# SUBSET SELECT FOREST TYPES FOR ANALYSIS AND PLOTTING (IN ADDITION TO BIOME-SCALE ANALYSIS) ======================
site.trnd.stable.lc.dt <- site.trnd.stable.dt
site.trnd.stable.lc.dt <- site.trnd.stable.lc.dt %>% filter(landcov.name == 'DNF' | landcov.name == 'ENF' | landcov.name == 'DBF' | landcov.name == 'MF')
site.trnd.stable.lc.dt <- site.trnd.stable.lc.dt %>% mutate(landcov.name = factor(landcov.name, levels = c('ENF','DNF','DBF','MF')))
site.trnd.stable.lc.dt <- site.trnd.stable.lc.dt %>% mutate(landcov.name = factor(landcov.name, labels = c('Evergreen Needleleaf', 'Deciduous Needleleaf','Deciduous Broadleaf','Mixed')))
site.trnd.stable.lc.dt <- data.table(site.trnd.stable.lc.dt)


# SUMMARY CLIMATE BY LANDSAT TREND CLASS + PERIOD + LAND COVER ==========================================
trend.cat.clim.smry.dt <- site.trnd.stable.dt[, .(landcov.name = 'All', 
                                                  tmax.jja.C.avg = sprintf("%.1f", mean(tmax.jja.period.avg.C, na.rm=T)),
                                                  tmax.jja.C.sd = sprintf("%.1f", sd(tmax.jja.period.avg.C, na.rm=T),1),
                                                  vpd.jja.kPa.avg = sprintf("%.2f", mean(vpd.jja.period.avg.kPa, na.rm=T)),
                                                  vpd.jja.kPa.sd = sprintf("%.2f", sd(vpd.jja.period.avg.kPa, na.rm=T)),
                                                  def.wy.mm.avg = sprintf("%.0f", mean(def.wy.period.avg.mm, na.rm=T)),
                                                  def.wy.mm.sd = sprintf("%.0f", sd(def.wy.period.avg.mm, na.rm=T)),
                                                  ppt.wy.mm.avg = sprintf("%.0f", mean(ppt.wy.period.avg.mm, na.rm=T)),
                                                  ppt.wy.mm.sd = sprintf("%.0f", sd(ppt.wy.period.avg.mm, na.rm=T)),
                                                  n.sites = .N), by = c('period','trend.cat')]

trend.cat.clim.smry.dt <- setorder(trend.cat.clim.smry.dt, landcov.name, period, trend.cat)

lc.trend.cat.clim.smry.dt <- site.trnd.stable.lc.dt[, .(tmax.jja.C.avg = sprintf("%.1f", mean(tmax.jja.period.avg.C, na.rm=T)),
                                                        tmax.jja.C.sd = sprintf("%.1f", sd(tmax.jja.period.avg.C, na.rm=T),1),
                                                        vpd.jja.kPa.avg = sprintf("%.2f", mean(vpd.jja.period.avg.kPa, na.rm=T)),
                                                        vpd.jja.kPa.sd = sprintf("%.2f", sd(vpd.jja.period.avg.kPa, na.rm=T)),
                                                        def.wy.mm.avg = sprintf("%.0f", mean(def.wy.period.avg.mm, na.rm=T)),
                                                        def.wy.mm.sd = sprintf("%.0f", sd(def.wy.period.avg.mm, na.rm=T)),
                                                        ppt.wy.mm.avg = sprintf("%.0f", mean(ppt.wy.period.avg.mm, na.rm=T)),
                                                        ppt.wy.mm.sd = sprintf("%.0f", sd(ppt.wy.period.avg.mm, na.rm=T)),
                                                        n.sites = .N), by = c('landcov.name','period','trend.cat')]

lc.trend.cat.clim.smry.dt <- setorder(lc.trend.cat.clim.smry.dt, landcov.name, period, trend.cat)

trend.cat.clim.smry.dt <- rbind(lc.trend.cat.clim.smry.dt, trend.cat.clim.smry.dt)
trend.cat.clim.smry.dt

fwrite(trend.cat.clim.smry.dt, 'output/clim_smry_for_lsat_ndvi_trend_categories.csv')

# 'fancy' verion of table
trend.cat.clim.smry.fncy.dt <- trend.cat.clim.smry.dt[,1:3]
trend.cat.clim.smry.fncy.dt$tmax.jja <- paste0(trend.cat.clim.smry.dt$tmax.jja.C.avg, ' (',trend.cat.clim.smry.dt$tmax.jja.C.sd,')')
trend.cat.clim.smry.fncy.dt$vpd.jja <- paste0(trend.cat.clim.smry.dt$vpd.jja.kPa.avg, ' (',trend.cat.clim.smry.dt$vpd.jja.kPa.sd,')')
trend.cat.clim.smry.fncy.dt$def.wy <- paste0(trend.cat.clim.smry.dt$def.wy.mm.avg, ' (',trend.cat.clim.smry.dt$def.wy.mm.sd,')')
trend.cat.clim.smry.fncy.dt$ppt.wy <- paste0(trend.cat.clim.smry.dt$ppt.wy.mm.avg, ' (',trend.cat.clim.smry.dt$ppt.wy.mm.sd,')')
trend.cat.clim.smry.fncy.dt$n.sites <- trend.cat.clim.smry.dt$n.sites

dcast(melt(trend.cat.clim.smry.fncy.dt, id.vars = c('landcov.name','period','trend.cat')), formula = variable + landcov.name + period ~ trend.cat, value.var = 'value')

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
tmax.fig <- ggplot() + stat_boxplot_custom(data=site.trnd.stable.dt, aes(x=trend.cat, y=tmax.jja.period.avg.C), 
                                           qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='errorbar', width = 0.2)
tmax.fig <- tmax.fig + stat_boxplot_custom(data=site.trnd.stable.dt, aes(x=trend.cat, y=tmax.jja.period.avg.C, fill=trend.cat), 
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
tmax.lc.fig <- ggplot() + stat_boxplot_custom(data=site.trnd.stable.lc.dt, aes(x=trend.cat, y=tmax.jja.period.avg.C), 
                                              qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='errorbar', width = 0.2, )
tmax.lc.fig <- tmax.lc.fig + stat_boxplot_custom(data=site.trnd.stable.lc.dt, aes(x=trend.cat, y=tmax.jja.period.avg.C, fill=trend.cat), 
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
tmax.lc.fig <- ggplot() + stat_boxplot_custom(data=subset(site.trnd.stable.lc.dt, period=='1985-2017'), aes(x=trend.cat, y=tmax.jja.period.avg.C), 
                                              qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='errorbar', width = 0.2, )
tmax.lc.fig <- tmax.lc.fig + stat_boxplot_custom(data=subset(site.trnd.stable.lc.dt, period=='1985-2017'), aes(x=trend.cat, y=tmax.jja.period.avg.C, fill=trend.cat), 
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
vpd.fig <- ggplot() + stat_boxplot_custom(data=site.trnd.stable.dt, aes(x=trend.cat, y=vpd.jja.period.avg.kPa), 
                                          qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='errorbar', width = 0.2)
vpd.fig <- vpd.fig + stat_boxplot_custom(data=site.trnd.stable.dt, aes(x=trend.cat, y=vpd.jja.period.avg.kPa, fill=trend.cat), 
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
vpd.lc.fig <- ggplot() + stat_boxplot_custom(data=site.trnd.stable.lc.dt, aes(x=trend.cat, y=vpd.jja.period.avg.kPa), 
                                             qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='errorbar', width = 0.2)
vpd.lc.fig <- vpd.lc.fig + stat_boxplot_custom(data=site.trnd.stable.lc.dt, aes(x=trend.cat, y=vpd.jja.period.avg.kPa, fill=trend.cat), 
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
def.fig <- ggplot() + stat_boxplot_custom(data=site.trnd.stable.dt, aes(x=trend.cat, y=def.wy.period.avg.mm), 
                                          qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='errorbar', width = 0.2)
def.fig <- def.fig + stat_boxplot_custom(data=site.trnd.stable.dt, aes(x=trend.cat, y=def.wy.period.avg.mm, fill=trend.cat), 
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
def.lc.fig <- ggplot() + stat_boxplot_custom(data=site.trnd.stable.lc.dt, aes(x=trend.cat, y=def.wy.period.avg.mm), 
                                             qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='errorbar', width = 0.2)
def.lc.fig <- def.lc.fig + stat_boxplot_custom(data=site.trnd.stable.lc.dt, aes(x=trend.cat, y=def.wy.period.avg.mm, fill=trend.cat), 
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
ppt.fig <- ggplot() + stat_boxplot_custom(data=site.trnd.stable.dt, aes(x=trend.cat, y=ppt.wy.period.avg.mm), 
                                          qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='errorbar', width = 0.2)
ppt.fig <- ppt.fig + stat_boxplot_custom(data=site.trnd.stable.dt, aes(x=trend.cat, y=ppt.wy.period.avg.mm, fill=trend.cat), 
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
ppt.lc.fig <- ggplot() + stat_boxplot_custom(data=site.trnd.stable.lc.dt, aes(x=trend.cat, y=ppt.wy.period.avg.mm), 
                                             qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='errorbar', width = 0.2, )
ppt.lc.fig <- ppt.lc.fig + stat_boxplot_custom(data=site.trnd.stable.lc.dt, aes(x=trend.cat, y=ppt.wy.period.avg.mm, fill=trend.cat), 
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
ppt.lc.fig <- ggplot() + stat_boxplot_custom(data=site.trnd.stable.lc.dt[period == '1985-2017'], aes(x=trend.cat, y=ppt.wy.period.avg.mm), 
                                             qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='errorbar', width = 0.2, )
ppt.lc.fig <- ppt.lc.fig + stat_boxplot_custom(data=site.trnd.stable.lc.dt[period == '1985-2017'], aes(x=trend.cat, y=ppt.wy.period.avg.mm, fill=trend.cat), 
                                               qs = c(0.025, 0.25,0.5, 0.75, 0.975), geom ='boxplot', width = 0.7, notch=TRUE, outlier.shape=NA)
ppt.lc.fig <- ppt.lc.fig + facet_grid(. ~ landcov.name) 
ppt.lc.fig <- ppt.lc.fig + theme_bw() + scale_fill_manual(values=trend.cols) + labs(y=ylab.ppt, x=xlab.trnd)
ppt.lc.fig <- ppt.lc.fig + scale_y_continuous(limits=c(200,1200), breaks=seq(200,1200,300))
ppt.lc.fig <- ppt.lc.fig + theme(legend.position="none", strip.background = element_rect("gray90"), strip.text = element_text(size=10), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 
ppt.lc.fig

jpeg('figures/clim_ppt_vs_lsat_ndvi_trend_by_landcov_1985to2017.jpg', width = 7, height = 6, res = 400, units = 'in')
ppt.lc.fig
dev.off()

