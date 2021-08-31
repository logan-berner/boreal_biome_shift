# ABOUT THIS SCRIPT  ==============================================================================================================
# This R script: 
# (1) maps tree cover; 
# (2) computes / plots mean tree cover in each greenness trend class;
# (3) summarizes greenness trends along tree cover gradients
# AUTHOR: LOGAN BERNER, NAU
# DATE: 2021-07-31

# SET UP WORKSPACE ==============================================================================================================
rm(list=ls())
.libPaths(c(.libPaths(), "~/R/", '/home/lb968/R/4.0.2'))
require(data.table)
require(ggplot2)
require(ggpubr)
require(ggspatial)
require(dplyr)
require(sf)
require(raster)
require(rgdal)
require(broom) # for tidy()

tmp.dir <- '/scratch/lb968/lsat_vi_trend_smry/'
tempfile(tmpdir=tmp.dir)

setwd('/projects/arctic/users/lberner/boreal_biome_shift/')

# CUSTOM FUNCTION ======================================================================================
quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

raster2spdf <- function(r){
  df <- as.data.frame(as(r, "SpatialPixelsDataFrame"))
  colnames(df) <- c("value", "x", "y")
  df
}

# # REDUCE RESOLUTION OF LAND COVER DATA FOR PLOTTING (OTHERWISE ERRORS OUT FROM TOO LARGE FILES) ==========================================
# treecov.r <- raster('data/gis_data/landcov/modis_treecov_median_2017to2019_boreal_laea_300m.tif')
# aoi.r <- raster('data/gis_data/boreal_sampling_frame_300m_laea.tif')
# 
# # aggregate sample frame to 1500 m and create mask for grid cells with at least 50% coverage
# aoi.sum.1500m.r <- raster::aggregate(aoi.r, fact = c(5,5), fun = sum, na.rm=T)
# aoi.pcnt.1500m.r <- aoi.sum.1500m.r / 25 * 100
# aoi.abv.50pcnt.1500m.r <- aoi.pcnt.1500m.r >= 50
# 
# # mask to sample frame
# treecov.r <- mask(treecov.r, aoi.r)
# 
# treecov.1500m.r <- raster::aggregate(treecov.r, fact = c(5,5), fun = mean, na.rm=T)
# treecov.1500m.r <- mask(treecov.1500m.r, aoi.abv.50pcnt.1500m.r)
# writeRaster(treecov.1500m.r, 'data/gis_data/landcov/modis_treecov_median_2017to2019_boreal_aoi_laea_1500m.tif', overwrite=T)


# READ IN FILES ================================================================================================================
site.dt <- fread('output/boreal_sample_site_climate_and_landcover.csv')
site.trnds.dt <- do.call("rbind", lapply(list.files('output/lsat_vi_gs_site_trends/mc_reps/', full.names = T), fread))

# spatial data sets
boreal.shp <- readOGR(dsn = 'data/gis_data/wwf_boreal_biome_laea.shp')
land.45n.shp <-  readOGR(dsn = 'data/gis_data/land_45n_laea.shp')
treecov.r <- raster('data/gis_data/landcov/modis_treecov_median_2017to2019_boreal_aoi_laea_1500m.tif')


# DATA PREP ==============================================================================================================
site.trnds.dt <- site.trnds.dt[trend.period == '2000to2019']
site.trnds.dt$treecov <- site.dt$treecov[match(site.trnds.dt$site, site.dt$site)]
site.trnds.dt$trend.cat <- factor(site.trnds.dt$trend.cat, levels = c('no_trend', 'browning','greening'), labels = c('none','browning','greening'))


# SUMMARIZE VEGETATION GREENNESS TRENDS BY TREE COVER ===============================================================================

# average tree cover in each trend class -------------------------------------
treecov.avg.mc.dt <- site.trnds.dt[, .(treecov.avg = mean(treecov)), by = c('rep','trend.cat')]
treecov.avg.mc.dt <- treecov.avg.mc.dt[trend.cat != '']
treecov.avg.mc.dt$trend.cat <- factor(treecov.avg.mc.dt$trend.cat, levels = c('greening','none','browning'))

treecov.avg.mc.smry.dt <- treecov.avg.mc.dt[, .(treecov.med = round(median(treecov.avg),1), 
                                          treecov.q025 = round(quantile(treecov.avg, 0.025),1), 
                                          treecov.q975 = round(quantile(treecov.avg, 0.975),1)),
                                      by = 'trend.cat']
treecov.avg.mc.smry.dt
treecov.avg.mc.smry.dt[, trend.cat := factor(trend.cat, levels = c('greening','none','browning'))]
setorder(treecov.avg.mc.smry.dt, trend.cat)

# fraction of sites that greenned versus browning along tree cover gradient --------------------------------
treecov.gradient.mc.dt <- site.trnds.dt[, .(n.sites.bin = .N), by = c('rep','treecov','trend.cat')]
treecov.gradient.mc.dt[, n.sites.tot := sum(n.sites.bin), by = c('rep')]
treecov.gradient.mc.dt[, n.sites.tc.bin := sum(n.sites.bin), by = c('rep','treecov')]
treecov.gradient.mc.dt[, pcnt.sites.tot := n.sites.bin / n.sites.tot * 100]
treecov.gradient.mc.dt[, pcnt.sites.tc.bin := n.sites.bin / n.sites.tc.bin * 100]

treecov.gradient.mc.smry.dt <- treecov.gradient.mc.dt[, .(pcnt.sites.tot.q500 = quantile(pcnt.sites.tot, 0.5),
                                          pcnt.sites.tc.bin.q500 = quantile(pcnt.sites.tc.bin, 0.5),
                                          pcnt.sites.tc.bin.q025 = quantile(pcnt.sites.tc.bin, 0.025),
                                          pcnt.sites.tc.bin.q975 = quantile(pcnt.sites.tc.bin, 0.975)), by = c('treecov','trend.cat')]

treecov.gradient.mc.smry.dt <- treecov.gradient.mc.smry.dt[trend.cat != '' & trend.cat != 'none']
treecov.gradient.mc.smry.dt[, trend.cat := factor(trend.cat, levels = c('greening','browning'))]
setorder(treecov.gradient.mc.smry.dt, treecov, trend.cat)


# correlation between tree cover and fraction of sites greening / browning ------------------------------------------------
quantile(site.trnds.dt$treecov, 0.995)

treecov.gradient.cor.mc.dt <- dcast(treecov.gradient.mc.dt[trend.cat != 'none' & treecov <= 70], rep + treecov ~ trend.cat, value.var = 'pcnt.sites.tc.bin')
treecov.gradient.cor.mc.dt <- treecov.gradient.cor.mc.dt[, .(r.grn = as.numeric(cor.test(treecov, greening, method = 'spearman')$estimate),
                                                             r.brn = as.numeric(cor.test(treecov, browning, method = 'spearman')$estimate)), by = rep]
treecov.gradient.cor.mc.smry.dt <- treecov.gradient.cor.mc.dt[, .(r.grn.med = median(r.grn), r.grn.q025 = quantile(r.grn, 0.025), r.grn.q975 = quantile(r.grn, 0.975),
                                                    r.brn.med = median(r.brn), r.brn.q025 = quantile(r.brn, 0.025), r.brn.q975 = quantile(r.brn, 0.975))]
treecov.gradient.cor.mc.smry.dt



# TREE COVER FIGURES  =================================================================================================

# tree cover map -----------------------------------------------------------------------------------------
# prep for mapping
boreal.tidy <- tidy(boreal.shp)
land.45n.tidy <- tidy(land.45n.shp)
treecov.spdf <- raster2spdf(treecov.r)
# lwr <- quantile(treecov.spdf$value, 0.025)
# upr <- quantile(treecov.spdf$value, 0.975)
# treecov.spdf$value[treecov.spdf$value < lwr] <- lwr
# treecov.spdf$value[treecov.spdf$value > upr] <- upr

treecov.map <- ggplot() +  
  geom_polygon(data=land.45n.tidy, aes(x = long, y = lat, group = group), color = "gray10", size = 0.1, fill = "gray80") +
  geom_polygon(data=boreal.tidy, aes(x = long, y = lat, group = group), color = NA, size = 0.1, fill = "white") +
  geom_raster(data=treecov.spdf, aes(x=x, y=y, fill=value), alpha=1) + 
  geom_polygon(data=boreal.tidy, aes(x = long, y = lat, group = group), color = "black", size = 0.2, fill = NA) +
  scale_fill_gradientn(colours = c('lightgoldenrod4','lightgoldenrod','yellow','green','green4','green4')) +
  labs(fill = 'Tree cover (%)') + coord_equal() + theme_void() +
  theme(legend.position="bottom", legend.box="horizontal", legend.key.width=unit(1.5, "cm"),
        legend.text=element_text(size=12), legend.title=element_text(size=14)) + 
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))

treecov.map


# mean tree cover for each vegetation greenness trend class -----------------------------------------------
trnd.cols <- c('springgreen3','gray','saddlebrown')

treecov.point <- ggplot(treecov.avg.mc.smry.dt, aes(x=trend.cat, y=treecov.med)) + 
  geom_errorbar(aes(ymin = treecov.q025, ymax = treecov.q975), width = 0.2, color = 'gray30', alpha = 0.5) +
  geom_point(aes(fill = trend.cat), shape = 21, size = 4, color = 'black') + 
  scale_fill_manual(values = trnd.cols) +  
  theme_bw() +  theme(legend.position = 'none', axis.text=element_text(size=12), axis.title=element_text(size=14)) + 
  labs(x = "Trend", y = 'Mean tree cover (%)') 

treecov.point


# fraction of vegetation G / B along tree cover gradient ---------------------------------------------------
tc.grn.cor.r <- paste0('r = ', sprintf("%.2f", treecov.gradient.cor.mc.smry.dt$r.grn.med),
                       ' [', sprintf("%.2f", treecov.gradient.cor.mc.smry.dt$r.grn.q975),
                       ' , ', sprintf("%.2f", treecov.gradient.cor.mc.smry.dt$r.grn.q025),']')

tc.brn.cor.r <- paste0('r = ', sprintf("%.2f", treecov.gradient.cor.mc.smry.dt$r.brn.med),
                       ' [', sprintf("%.2f", treecov.gradient.cor.mc.smry.dt$r.brn.q025),
                       ' , ', sprintf("%.2f", treecov.gradient.cor.mc.smry.dt$r.brn.q975),']')


treecov.line <- ggplot(treecov.gradient.mc.smry.dt, aes(x = treecov, y = pcnt.sites.tc.bin.q500, color = trend.cat, fill = trend.cat)) +
  geom_ribbon(aes(ymin = pcnt.sites.tc.bin.q025, ymax = pcnt.sites.tc.bin.q975), alpha = 0.5) + 
  geom_line(lwd = 1.5) + 
  annotate(geom = 'text', 40, Inf, size = 4, label = tc.grn.cor.r, vjust = 2, color = trnd.cols[1]) +
  annotate(geom = 'text', 40, -Inf, size = 4, label = tc.brn.cor.r, vjust = -2, color = trnd.cols[3]) + 
  scale_color_manual(name = expression('Greenness \n trend'), values = trnd.cols[c(1,3)]) +
  scale_fill_manual(values = trnd.cols[c(1,3)], name = 'fill') +
  theme_bw() + lims(x = c(0, 70), y = c(0, 45)) + 
  theme(legend.position = 'none', axis.text=element_text(size=12), axis.title=element_text(size=14)) + 
  labs(x = 'Tree cover (%)', y = 'Trend prevalence\n(% of sample sites)')

treecov.line


# COMBINE TREE COVER  FIGURES -------------------------------------------------------------------------------------------
ggarrange(treecov.map, treecov.point, treecov.line, ncol = 3, nrow = 1, labels=c('a','b','c'), widths = c(1,1,1.2), label.x = c(0.2, 0.2, 0.25), label.y = 0.98)
ggsave('figures/paper/fig_XX_lsat_vi_trend_by_treecov.jpg', width = 10, height = 3.5, units = 'in', dpi = 400)

#ggsave('figures/paper/fig_XX_lsat_vi_trend_by_treecov.jpg', width = 3.5, height = 7, units = 'in', dpi = 400)

# 
# # tree cover histogram
# treecov.hist <- ggplot(treecov.smry.mc.dt, aes(x = treecov, y = pcnt.sites.tot.q500, fill = trend.cat)) +
#   geom_area() +
#   scale_fill_manual(name = expression('Greenness \n trend'), values = trnd.cols, guide = guide_legend(reverse = TRUE)) +
#   theme_bw() + lims(x = c(0, 70)) +
#   theme(legend.position = 'none', legend.text=element_text(size=12), legend.title=element_text(size=12),
#         axis.text.x = element_text(angle = 0), axis.text=element_text(size=12), axis.title=element_text(size=12)) +
#   labs(x = 'Tree cover (%)', y = 'Fraction of sample sites')
# 
# treecov.hist