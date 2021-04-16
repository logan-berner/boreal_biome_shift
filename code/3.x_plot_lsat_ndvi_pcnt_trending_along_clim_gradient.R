# SETUP ==========================================================================
# This R script 
rm(list=ls())
require(data.table)
require(dplyr)
require(lattice)
require(latticeExtra)
require(grid)
# setwd('C:/Users/lb968/Google Drive/research/nau/nasa_above/boreal_ndvi/')
setwd('C:/Users/Logan/Google Drive/research/nau/nasa_above/boreal_ndvi/')


# LOAD AND PREP DATA =====================================================================
ndvi.trnd.site <- fread('output/boreal_site_lsat_ndvi_trends.csv')

# collapse trend categories
ndvi.trnd.site$trend.cat[ndvi.trnd.site$trend.cat == 'greening.sig.p5'] <- 'greening'
ndvi.trnd.site$trend.cat[ndvi.trnd.site$trend.cat == 'greening.sig.p10'] <- 'greening'
ndvi.trnd.site$trend.cat[ndvi.trnd.site$trend.cat == 'browning.sig.p5'] <- 'browning'
ndvi.trnd.site$trend.cat[ndvi.trnd.site$trend.cat == 'browning.sig.p10'] <- 'browning'
ndvi.trnd.site$trend.cat[ndvi.trnd.site$trend.cat == 'insig'] <- 'no trend'

# consolidate Mosaic and Mixed Forest types
ndvi.trnd.site$landcov.name[ndvi.trnd.site$landcov.name == 'Mosaic'] <- 'MF'
ndvi.trnd.site <- ndvi.trnd.site %>% filter(landcov.name == 'DNF' | landcov.name == 'ENF' | landcov.name == 'DBF' | landcov.name == 'MF') 
ndvi.trnd.site <- ndvi.trnd.site %>% mutate(landcov.name = factor(landcov.name, levels = c('Mosaic','MF','DBF','DNF','ENF')))
ndvi.trnd.site <- ndvi.trnd.site %>% mutate(landcov.name = factor(landcov.name, labels = c('Mosaic','Deciduous Broadleaf','Deciduous Needleleaf','Evergreen Needleleaf')))
ndvi.trnd.site <- data.table(ndvi.trnd.site)

# create columns with average climate during respective periods 
ndvi.trnd.site[period == '1985-2017', tmax.jja.period.avg.C := tmax.1985to2017.jja.avg.C]
ndvi.trnd.site[period == '2000-2017', tmax.jja.period.avg.C := tmax.2000to2017.jja.avg.C]
ndvi.trnd.site[period == '1985-2017', vpd.jja.period.avg.kPa := vpd.1985to2017.jja.avg.kPa]
ndvi.trnd.site[period == '2000-2017', vpd.jja.period.avg.kPa := vpd.2000to2017.jja.avg.kPa]
ndvi.trnd.site[period == '1985-2017', ppt.wy.period.avg.mm := ppt.1985to2017.wy.avg.mm]
ndvi.trnd.site[period == '2000-2017', ppt.wy.period.avg.mm := ppt.2000to2017.wy.avg.mm]
ndvi.trnd.site[period == '1985-2017', def.wy.period.avg.mm := def.1985to2017.wy.avg.mm]
ndvi.trnd.site[period == '2000-2017', def.wy.period.avg.mm := def.2000to2017.wy.avg.mm]

# COMPUTE NDVI TREND FREQ ALONG TEMPERATURE GRADIENT ===========================================
trnd.freq.by.tmax <- ndvi.trnd.site %>% mutate(clim.grad = round(ndvi.trnd.site$tmax.jja.period.avg.C)) %>%
  group_by(period, clim.grad, trend.cat) %>% 
  summarise(n.sites = n()) %>% group_by(period, clim.grad) %>% 
  mutate(n.sites.cat = sum(n.sites), pcnt.sites = n.sites / n.sites.cat * 100) %>%
  group_by(period) %>% mutate(n.sites.all = sum(n.sites), cat.frac.all.sites = n.sites.cat / n.sites.all) %>% 
  filter(n.sites.cat >= 10) %>% filter(trend.cat != 'no trend')

trnd.freq.by.tmax.lc <- ndvi.trnd.site %>% mutate(clim.grad = round(ndvi.trnd.site$tmax.1985to2017.jja.avg.C)) %>%
  group_by(period, landcov.name, clim.grad, trend.cat) %>% 
  summarise(n.sites = n()) %>% group_by(period, landcov.name, clim.grad) %>% 
  mutate(n.sites.cat = sum(n.sites), pcnt.sites = n.sites / n.sites.cat * 100) %>%
  group_by(landcov.name, period) %>% mutate(n.sites.all = sum(n.sites), cat.frac.all.sites = n.sites.cat / n.sites.all) %>% 
  filter(n.sites.cat >= 10) %>% filter(trend.cat != 'no trend')

# COMPUTE NDVI TREND FREQ ALONG VPD GRADIENT ===========================================
trnd.freq.by.vpd <- ndvi.trnd.site %>% mutate(clim.grad = round(ndvi.trnd.site$vpd.jja.period.avg.kPa,1)) %>%
  group_by(period, clim.grad, trend.cat) %>% 
  summarise(n.sites = n()) %>% group_by(period, clim.grad) %>% 
  mutate(n.sites.cat = sum(n.sites), pcnt.sites = n.sites / n.sites.cat * 100) %>%
  group_by(period) %>% mutate(n.sites.all = sum(n.sites), cat.frac.all.sites = n.sites.cat / n.sites.all) %>% 
  filter(n.sites.cat >= 10) %>% filter(trend.cat != 'no trend')


# COMPUTE NDVI TREND FREQ ALONG DEF GRADIENT ===========================================
trnd.freq.by.def <- ndvi.trnd.site %>% mutate(clim.grad = round(ndvi.trnd.site$def.wy.period.avg.mm,-1)) %>%
  group_by(period, clim.grad, trend.cat) %>% 
  summarise(n.sites = n()) %>% group_by(period, clim.grad) %>% 
  mutate(n.sites.cat = sum(n.sites), pcnt.sites = n.sites / n.sites.cat * 100) %>%
  group_by(period) %>% mutate(n.sites.all = sum(n.sites), cat.frac.all.sites = n.sites.cat / n.sites.all) %>% 
  filter(n.sites.cat >= 10) %>% filter(trend.cat != 'no trend')

trnd.freq.by.def.lc <- ndvi.trnd.site %>% mutate(clim.grad = round(ndvi.trnd.site$def.wy.period.avg.mm,-1)) %>%
  group_by(period, landcov.name, clim.grad, trend.cat) %>% 
  summarise(n.sites = n()) %>% group_by(period, landcov.name, clim.grad) %>% 
  mutate(n.sites.cat = sum(n.sites), pcnt.sites = n.sites / n.sites.cat * 100) %>%
  group_by(landcov.name, period) %>% mutate(n.sites.all = sum(n.sites), cat.frac.all.sites = n.sites.cat / n.sites.all) %>% 
  filter(n.sites.cat >= 10) %>% filter(trend.cat != 'no trend')

# PLOTTING PARAMETERS =========================================================================
sb <- trellis.par.get("strip.background") 
sb[["col"]][1] <- "gray90"
trellis.par.set("strip.background", sb) 

xlab.tmax <- expression('Mean summer high temperature ('~degree~'C)')
xlab.vpd <- expression('Mean summer VPD (kPa)')
xlab.def <- expression('Mean water year deficit (mm)')

ylab.pcnt.zone <- '% of sites'

my.cex=1.25
trend.cols <- c('lightsalmon4','springgreen4')

trend.key <- list(title='NDVI trend', corner=c(0.8,0.9),text=list(c('positive','negative')), rect=list(col=rev(trend.cols)), cex=0.85)

# PLOT: % SITES WITH NDVI TREND ALONG TEMPERATURE GRADIENT =========================================================================
trnd.by.tmax <- xyplot(pcnt.sites ~ clim.grad | period, trnd.freq.by.tmax, groups = trend.cat, key = trend.key,  
                       type='b', lwd = 2, pch=19, col.bg = trend.cols, col = trend.cols, 
                       ylab = list('Percent of sites', cex = my.cex), xlab=list(xlab.tmax, cex = my.cex), 
                       scales=list(x=list(draw=T), cex=my.cex, tck = c(1,0), alternating = F),
                       par.settings = list(strip.background=list(col="gray90")))

trnd.by.tmax

jpeg('figures/lsat_ndvi_trend_freq_along_tmax_gradient.jpg', width = 7, height = 4, res = 400, units = 'in')
trnd.by.tmax
dev.off()

# ndvi ~ tmax and land cover
trnd.by.tmax.lc <- xyplot(pcnt.sites ~ clim.grad | period + landcov.name, trnd.freq.by.tmax.lc, groups = trend.cat, key = trend.key,  
                       type='b', lwd = 2, pch=19, col.bg = trend.cols, col = trend.cols, 
                       ylab = list('Percent of sites', cex = my.cex), xlab=list(xlab.tmax, cex = my.cex), 
                       scales=list(x=list(draw=T), cex=my.cex, tck = c(1,0), alternating = F),
                       par.settings = list(strip.background=list(col="gray90")))

trnd.by.tmax.lc



# PLOT: % SITES WITH NDVI TREND ALONG VPD GRADIENT =========================================================================
trnd.by.vpd <- xyplot(pcnt.sites ~ clim.grad | period, trnd.freq.by.vpd, groups = trend.cat, key = trend.key,
                      type='b', lwd = 2, pch = 19, col.bg = trend.cols, col = trend.cols, 
                      ylab = list('Percent of sites', cex = my.cex), xlab=list(xlab.vpd, cex = my.cex), 
                      scales=list(x=list(draw=T), cex=my.cex, tck = c(1,0), alternating = F),
                      par.settings = list(strip.background=list(col="gray90")))

trnd.by.vpd

jpeg('figures/lsat_ndvi_trend_freq_along_vpd_gradient.jpg', width = 7, height = 4, res = 400, units = 'in')
trnd.by.vpd
dev.off()


# PLOT: % SITES WITH NDVI TREND ALONG VPD GRADIENT =========================================================================
trnd.by.def <- xyplot(pcnt.sites ~ clim.grad | period, trnd.freq.by.def, groups = trend.cat, key = trend.key,  
                      type='b', lwd = 2, pch = 19, col.bg = trend.cols, col = trend.cols, 
                      ylab = list('Percent of sites', cex = my.cex), xlab=list(xlab.def, cex = my.cex), 
                      scales=list(x=list(draw=T), cex=my.cex, tck = c(1,0), alternating = F),
                      par.settings = list(strip.background=list(col="gray90")))
trnd.by.def

jpeg('figures/lsat_ndvi_trend_freq_along_def_gradient.jpg', width = 7, height = 4, res = 400, units = 'in')
trnd.by.def
dev.off()

trnd.by.def.lc <- xyplot(pcnt.sites ~ clim.grad | period + landcov.name, trnd.freq.by.def.lc, groups = trend.cat, key = trend.key,  
                      type='b', lwd = 2, pch = 19, col.bg = trend.cols, col = trend.cols, 
                      ylab = list('Percent of sites', cex = my.cex), xlab=list(xlab.def, cex = my.cex), 
                      scales=list(x=list(draw=T), cex=my.cex, tck = c(1,0), alternating = F),
                      par.settings = list(strip.background=list(col="gray90")))
trnd.by.def.lc
