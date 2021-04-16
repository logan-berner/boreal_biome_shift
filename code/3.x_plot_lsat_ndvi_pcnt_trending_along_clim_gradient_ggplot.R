# SETUP ==========================================================================
# This R script 
rm(list=ls())
require(data.table)
require(dplyr)
require(ggplot2)
setwd('C:/Users/lb968/Google Drive/research/nau/nasa_above/boreal_ndvi/')
# setwd('C:/Users/Logan/Google Drive/research/nau/nasa_above/boreal_ndvi/')

# LOAD AND PREP DATA =====================================================================
ndvi.trnd.site <- fread('output/boreal_site_lsat_ndvi_trends.csv')

# collapse trend categories
ndvi.trnd.site$trend.cat[ndvi.trnd.site$trend.cat == 'greening.sig.p5'] <- 'positive'
ndvi.trnd.site$trend.cat[ndvi.trnd.site$trend.cat == 'greening.sig.p10'] <- 'positive'
ndvi.trnd.site$trend.cat[ndvi.trnd.site$trend.cat == 'browning.sig.p5'] <- 'negative'
ndvi.trnd.site$trend.cat[ndvi.trnd.site$trend.cat == 'browning.sig.p10'] <- 'negative'
ndvi.trnd.site$trend.cat[ndvi.trnd.site$trend.cat == 'insig'] <- 'no trend'

# create columns with average climate during respective periods 
ndvi.trnd.site[period == '1985-2017', tmax.jja.period.avg.C := tmax.1985to2017.jja.avg.C]
ndvi.trnd.site[period == '2000-2017', tmax.jja.period.avg.C := tmax.2000to2017.jja.avg.C]
ndvi.trnd.site[period == '1985-2017', vpd.jja.period.avg.kPa := vpd.1985to2017.jja.avg.kPa]
ndvi.trnd.site[period == '2000-2017', vpd.jja.period.avg.kPa := vpd.2000to2017.jja.avg.kPa]
ndvi.trnd.site[period == '1985-2017', ppt.wy.period.avg.mm := ppt.1985to2017.wy.avg.mm]
ndvi.trnd.site[period == '2000-2017', ppt.wy.period.avg.mm := ppt.2000to2017.wy.avg.mm]
ndvi.trnd.site[period == '1985-2017', def.wy.period.avg.mm := def.1985to2017.wy.avg.mm]
ndvi.trnd.site[period == '2000-2017', def.wy.period.avg.mm := def.2000to2017.wy.avg.mm]

# select forest land cover types 
ndvi.trnd.site.lc <- ndvi.trnd.site %>% filter(landcov.name == 'DNF' | landcov.name == 'ENF' | landcov.name == 'DBF' | landcov.name == 'MF')
ndvi.trnd.site.lc <- ndvi.trnd.site.lc %>% mutate(landcov.name = factor(landcov.name, levels = rev(c('MF','DBF','DNF','ENF'))))
ndvi.trnd.site.lc <- ndvi.trnd.site.lc %>% mutate(landcov.name = factor(landcov.name, labels = rev(c('Mixed','Deciduous Broadleaf','Deciduous Needleleaf','Evergreen Needleleaf'))))
ndvi.trnd.site.lc <- data.table(ndvi.trnd.site.lc)

min.sites = 50
# COMPUTE NDVI TREND FREQ ALONG TEMPERATURE GRADIENT ===========================================
trnd.freq.by.tmax <- ndvi.trnd.site %>% mutate(clim.grad = round(tmax.jja.period.avg.C)) %>%
  group_by(period, clim.grad, trend.cat) %>% 
  summarise(n.sites = n()) %>% group_by(period, clim.grad) %>% 
  mutate(n.sites.cat = sum(n.sites), pcnt.sites = n.sites / n.sites.cat * 100) %>%
  group_by(period) %>% mutate(n.sites.all = sum(n.sites), cat.frac.all.sites = n.sites.cat / n.sites.all) %>% 
  filter(n.sites.cat >= min.sites) %>% filter(trend.cat != 'no trend')

trnd.freq.by.tmax.lm <- trnd.freq.by.tmax %>% 
  group_by(period, trend.cat) %>% 
  summarise(slp = summary(lm(pcnt.sites ~ clim.grad))$coefficients[2,1],
            p = summary(lm(pcnt.sites ~ clim.grad))$coefficients[2,4],
            r2 = summary(lm(pcnt.sites ~ clim.grad))$r.squared)
trnd.freq.by.tmax.lm

# trnd.freq.by.tmax.cor <- trnd.freq.by.tmax %>% group_by(period, trend.cat) %>% 
#   summarise(r = cor.test(pcnt.sites, clim.grad, method = 'kendall')$estimate,
#             p = cor.test(pcnt.sites, clim.grad, method = 'kendall')$p.value)
# trnd.freq.by.tmax.cor

trnd.freq.by.tmax.lc <- ndvi.trnd.site.lc %>% mutate(clim.grad = round(tmax.1985to2017.jja.avg.C)) %>%
  group_by(period, landcov.name, clim.grad, trend.cat) %>% 
  summarise(n.sites = n()) %>% group_by(period, landcov.name, clim.grad) %>% 
  mutate(n.sites.cat = sum(n.sites), pcnt.sites = n.sites / n.sites.cat * 100) %>%
  group_by(landcov.name, period) %>% mutate(n.sites.all = sum(n.sites), cat.frac.all.sites = n.sites.cat / n.sites.all) %>% 
  filter(n.sites.cat >= min.sites) %>% filter(trend.cat != 'no trend')

# COMPUTE NDVI TREND FREQ ALONG VPD GRADIENT ===========================================
trnd.freq.by.vpd <- ndvi.trnd.site %>% mutate(clim.grad = round(vpd.jja.period.avg.kPa,1)) %>%
  group_by(period, clim.grad, trend.cat) %>% 
  summarise(n.sites = n()) %>% group_by(period, clim.grad) %>% 
  mutate(n.sites.cat = sum(n.sites), pcnt.sites = n.sites / n.sites.cat * 100) %>%
  group_by(period) %>% mutate(n.sites.all = sum(n.sites), cat.frac.all.sites = n.sites.cat / n.sites.all) %>% 
  filter(n.sites.cat >= min.sites) %>% filter(trend.cat != 'no trend')

trnd.freq.by.vpd.lc <- ndvi.trnd.site.lc %>% mutate(clim.grad = round(vpd.jja.period.avg.kPa,1)) %>%
  group_by(period, landcov.name, clim.grad, trend.cat) %>% 
  summarise(n.sites = n()) %>% group_by(period, landcov.name, clim.grad) %>% 
  mutate(n.sites.cat = sum(n.sites), pcnt.sites = n.sites / n.sites.cat * 100) %>%
  group_by(landcov.name, period) %>% mutate(n.sites.all = sum(n.sites), cat.frac.all.sites = n.sites.cat / n.sites.all) %>% 
  filter(n.sites.cat >= min.sites) %>% filter(trend.cat != 'no trend')


# COMPUTE NDVI TREND FREQ ALONG DEF GRADIENT ===========================================
trnd.freq.by.def <- ndvi.trnd.site %>% mutate(clim.grad = round(def.wy.period.avg.mm,-1)) %>%
  group_by(period, clim.grad, trend.cat) %>% 
  summarise(n.sites = n()) %>% group_by(period, clim.grad) %>% 
  mutate(n.sites.cat = sum(n.sites), pcnt.sites = n.sites / n.sites.cat * 100) %>%
  group_by(period) %>% mutate(n.sites.all = sum(n.sites), cat.frac.all.sites = n.sites.cat / n.sites.all) %>% 
  filter(n.sites.cat >= min.sites) %>% filter(trend.cat != 'no trend')

trnd.freq.by.def.lc <- ndvi.trnd.site.lc %>% mutate(clim.grad = round(def.wy.period.avg.mm,-1)) %>%
  group_by(period, landcov.name, clim.grad, trend.cat) %>% 
  summarise(n.sites = n()) %>% group_by(period, landcov.name, clim.grad) %>% 
  mutate(n.sites.cat = sum(n.sites), pcnt.sites = n.sites / n.sites.cat * 100) %>%
  group_by(landcov.name, period) %>% mutate(n.sites.all = sum(n.sites), cat.frac.all.sites = n.sites.cat / n.sites.all) %>% 
  filter(n.sites.cat >= min.sites) %>% filter(trend.cat != 'no trend')

# COMPUTE NDVI TREND FREQ ALONG ppt GRADIENT ===========================================
trnd.freq.by.ppt <- ndvi.trnd.site %>% mutate(clim.grad = round(ppt.wy.period.avg.mm,-1)) %>%
  group_by(period, clim.grad, trend.cat) %>% 
  summarise(n.sites = n()) %>% group_by(period, clim.grad) %>% 
  mutate(n.sites.cat = sum(n.sites), pcnt.sites = n.sites / n.sites.cat * 100) %>%
  group_by(period) %>% mutate(n.sites.all = sum(n.sites), cat.frac.all.sites = n.sites.cat / n.sites.all) %>% 
  filter(n.sites.cat >= min.sites) %>% filter(trend.cat != 'no trend')

trnd.freq.by.ppt.lc <- ndvi.trnd.site.lc %>% mutate(clim.grad = round(ppt.wy.period.avg.mm,-1)) %>%
  group_by(period, landcov.name, clim.grad, trend.cat) %>% 
  summarise(n.sites = n()) %>% group_by(period, landcov.name, clim.grad) %>% 
  mutate(n.sites.cat = sum(n.sites), pcnt.sites = n.sites / n.sites.cat * 100) %>%
  group_by(landcov.name, period) %>% mutate(n.sites.all = sum(n.sites), cat.frac.all.sites = n.sites.cat / n.sites.all) %>% 
  filter(n.sites.cat >= min.sites) %>% filter(trend.cat != 'no trend')

# PLOTTING PARAMETERS =========================================================================
xlab.tmax <- expression('Mean summer high temperature ('~degree~'C)')
xlab.vpd <- expression('Mean summer VPD (kPa)')
xlab.def <- expression('Mean water year deficit (mm)')
xlab.ppt <- expression('Mean water year precipitation (mm)')

ylab.pcnt.zone <- 'Percent of sites'

my.cex=1.25
trend.cols <- c('lightsalmon4','springgreen4')

# PLOT: % SITES WITH NDVI TREND ALONG TEMPERATURE GRADIENT =========================================================================
tmax.fig <- ggplot(trnd.freq.by.tmax, aes(clim.grad, pcnt.sites, colour=trend.cat))
tmax.fig <- tmax.fig + geom_line(aes(linetype=trend.cat), lty=1, lwd=2)
tmax.fig <- tmax.fig + geom_point()
tmax.fig <- tmax.fig + facet_grid(cols=vars(period))
tmax.fig <- tmax.fig + labs(y=ylab.pcnt.zone, x=xlab.tmax)
tmax.fig <- tmax.fig + scale_y_continuous(limits=c(0,65), breaks=seq(0,60,20))
tmax.fig <- tmax.fig + scale_color_manual(name='NDVI trend', values=trend.cols)
tmax.fig <- tmax.fig + theme_bw() + theme(strip.background = element_rect("gray90"), strip.text = element_text(size=12),
                                          axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
                                          legend.position = c(0.8, 0.8))
tmax.fig


jpeg('figures/lsat_ndvi_trend_freq_along_tmax_gradient.jpg', width = 7, height = 4, res = 400, units = 'in')
tmax.fig
dev.off()

# ndvi ~ tmax and land cover
tmax.lc.fig <- ggplot(trnd.freq.by.tmax.lc, aes(clim.grad, pcnt.sites, colour=trend.cat))
tmax.lc.fig <- tmax.lc.fig + geom_line(aes(linetype=trend.cat), lty=1, lwd=2)
tmax.lc.fig <- tmax.lc.fig + facet_grid(landcov.name ~ period)
tmax.lc.fig <- tmax.lc.fig + labs(y=ylab.pcnt.zone, x=xlab.tmax)
tmax.lc.fig <- tmax.lc.fig + scale_y_continuous(limits=c(0,65), breaks=seq(0,60,20))
tmax.lc.fig <- tmax.lc.fig + scale_color_manual(values=trend.cols)
tmax.lc.fig <- tmax.lc.fig + theme_bw() + theme(legend.position="none", strip.background = element_rect("gray90"), strip.text = element_text(size=12),
                                          axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
tmax.lc.fig

jpeg('figures/lsat_ndvi_trend_freq_along_tmax_gradient_by_landcov.jpg', width = 6, height = 8, res = 400, units = 'in')
tmax.lc.fig
dev.off()

# PLOT: % SITES WITH NDVI TREND ALONG PRECIPITATION GRADIENT =========================================================================
ppt.fig <- ggplot(trnd.freq.by.ppt, aes(clim.grad, pcnt.sites, colour=trend.cat))
ppt.fig <- ppt.fig + geom_line(aes(linetype=trend.cat), lty=1, lwd=2)
ppt.fig <- ppt.fig + facet_grid(cols=vars(period))
ppt.fig <- ppt.fig + theme_bw() + scale_fill_manual(name = "Trend", values=trend.cols) + labs(y=ylab.pcnt.zone, x=xlab.ppt)
ppt.fig <- ppt.fig + scale_y_continuous(limits=c(0,65), breaks=seq(0,60,20))
ppt.fig <- ppt.fig + scale_color_manual(values=trend.cols)
ppt.fig <- ppt.fig + theme(legend.position="none", strip.background = element_rect("gray90"), strip.text = element_text(size=12),, axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 
ppt.fig

jpeg('figures/lsat_ndvi_trend_freq_along_ppt_gradient.jpg', width = 7, height = 4, res = 400, units = 'in')
ppt.fig
dev.off()

# ndvi ~ ppt and land cover
ppt.lc.fig <- ggplot(trnd.freq.by.ppt.lc, aes(clim.grad, pcnt.sites, colour=trend.cat))
ppt.lc.fig <- ppt.lc.fig + geom_line(aes(linetype=trend.cat), lty=1, lwd=2)
ppt.lc.fig <- ppt.lc.fig + facet_grid(landcov.name ~ period)
ppt.lc.fig <- ppt.lc.fig + theme_bw() + scale_fill_manual(name = "Trend", values=trend.cols) + labs(y=ylab.pcnt.zone, x=xlab.ppt)
ppt.lc.fig <- ppt.lc.fig + scale_y_continuous(limits=c(0,65), breaks=seq(0,60,20))
ppt.lc.fig <- ppt.lc.fig + scale_color_manual(values=trend.cols)
ppt.lc.fig <- ppt.lc.fig + theme(legend.position="none", strip.background = element_rect("gray90"), strip.text = element_text(size=12),, axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 
ppt.lc.fig

jpeg('figures/lsat_ndvi_trend_freq_along_ppt_gradient_by_landcov.jpg', width = 6, height = 6, res = 400, units = 'in')
ppt.lc.fig
dev.off()


# PLOT: % SITES WITH NDVI TREND ALONG DEFICIT GRADIENT =========================================================================
def.fig <- ggplot(trnd.freq.by.def, aes(clim.grad, pcnt.sites, colour=trend.cat))
def.fig <- def.fig + geom_line(aes(linetype=trend.cat), lty=1, lwd=2)
def.fig <- def.fig + facet_grid(cols=vars(period))
def.fig <- def.fig + theme_bw() + scale_fill_manual(name = "Trend", values=trend.cols) + labs(y=ylab.pcnt.zone, x=xlab.def)
def.fig <- def.fig + scale_y_continuous(limits=c(0,65), breaks=seq(0,60,20))
def.fig <- def.fig + scale_color_manual(values=trend.cols)
def.fig <- def.fig + theme(legend.position="none", strip.background = element_rect("gray90"), strip.text = element_text(size=12),, axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 
def.fig

jpeg('figures/lsat_ndvi_trend_freq_along_def_gradient.jpg', width = 7, height = 4, res = 400, units = 'in')
def.fig
dev.off()

# ndvi ~ def and land cover
def.lc.fig <- ggplot(trnd.freq.by.def.lc, aes(clim.grad, pcnt.sites, colour=trend.cat))
def.lc.fig <- def.lc.fig + geom_line(aes(linetype=trend.cat), lty=1, lwd=2)
def.lc.fig <- def.lc.fig + facet_grid(landcov.name ~ period)
def.lc.fig <- def.lc.fig + theme_bw() + scale_fill_manual(name = "Trend", values=trend.cols) + labs(y=ylab.pcnt.zone, x=xlab.def)
def.lc.fig <- def.lc.fig + scale_y_continuous(limits=c(0,65), breaks=seq(0,60,20))
def.lc.fig <- def.lc.fig + scale_color_manual(values=trend.cols)
def.lc.fig <- def.lc.fig + theme(legend.position="none", strip.background = element_rect("gray90"), strip.text = element_text(size=12),, axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 
def.lc.fig

jpeg('figures/lsat_ndvi_trend_freq_along_def_gradient_by_landcov.jpg', width = 6, height = 6, res = 400, units = 'in')
def.lc.fig
dev.off()

# PLOT: % SITES WITH NDVI TREND ALONG VPD GRADIENT =========================================================================
vpd.fig <- ggplot(trnd.freq.by.vpd, aes(clim.grad, pcnt.sites, colour=trend.cat))
vpd.fig <- vpd.fig + geom_line(aes(linetype=trend.cat), lty=1, lwd=2)
vpd.fig <- vpd.fig + facet_grid(cols=vars(period))
vpd.fig <- vpd.fig + theme_bw() + scale_fill_manual(name = "Trend", values=trend.cols) + labs(y=ylab.pcnt.zone, x=xlab.vpd)
vpd.fig <- vpd.fig + scale_y_continuous(limits=c(0,65), breaks=seq(0,60,20))
vpd.fig <- vpd.fig + scale_color_manual(values=trend.cols)
vpd.fig <- vpd.fig + theme(legend.position="none", strip.background = element_rect("gray90"), strip.text = element_text(size=12),, axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 
vpd.fig

jpeg('figures/lsat_ndvi_trend_freq_along_vpd_gradient.jpg', width = 7, height = 4, res = 400, units = 'in')
vpd.fig
dev.off()

# ndvi ~ vpd and land cover
vpd.lc.fig <- ggplot(trnd.freq.by.vpd.lc, aes(clim.grad, pcnt.sites, colour=trend.cat))
vpd.lc.fig <- vpd.lc.fig + geom_line(aes(linetype=trend.cat), lty=1, lwd=2)
vpd.lc.fig <- vpd.lc.fig + facet_grid(landcov.name ~ period)
vpd.lc.fig <- vpd.lc.fig + theme_bw() + scale_fill_manual(name = "Trend", values=trend.cols) + labs(y=ylab.pcnt.zone, x=xlab.vpd)
vpd.lc.fig <- vpd.lc.fig + scale_y_continuous(limits=c(0,65), breaks=seq(0,60,20))
vpd.lc.fig <- vpd.lc.fig + scale_color_manual(values=trend.cols)
vpd.lc.fig <- vpd.lc.fig + theme(legend.position="none", strip.background = element_rect("gray90"), strip.text = element_text(size=12),, axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 
vpd.lc.fig

jpeg('figures/lsat_ndvi_trend_freq_along_vpd_gradient_by_landcov.jpg', width = 6, height = 6, res = 400, units = 'in')
vpd.lc.fig
dev.off()
