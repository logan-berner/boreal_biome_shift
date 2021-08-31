# ABOUT THIS SCRIPT  ==============================================================================================================
# This R script computes average SWI for greening and browning sites for each land cover type 
# AUTHOR: LOGAN BERNER, NAU
# DATE: 2021-04-27

# SET UP WORKSPACE ==============================================================================================================
rm(list=ls())
.libPaths(c(.libPaths(), "~/R/", '/home/lb968/R/4.0.2'))
require(data.table)
require(ggplot2)
require(ggpubr)
require(dplyr)
require(viridis)
require(ggspatial)
require(dplyr)
require(sf)
require(raster)
require(rgdal)
require(broom) # for tidy()

setwd('/projects/arctic/users/lberner/boreal_biome_shift/')


# CUSTOM FUNCTION ======================================================================================
raster2spdf <- function(r){
  df <- as.data.frame(as(r, "SpatialPixelsDataFrame"))
  colnames(df) <- c("value", "x", "y")
  df
}

# READ IN FILES ================================================================================================================
site.dt <- fread('output/boreal_sample_site_climate_and_landcover.csv')
site.trnds.dt <- do.call("rbind", lapply(list.files('output/lsat_vi_gs_site_trends/mc_reps/', full.names = T), fread))
boreal.shp <- readOGR(dsn = 'data/gis_data/wwf_boreal_biome_laea.shp')
land.45n.shp <-  readOGR(dsn = 'data/gis_data/land_45n_laea.shp')
swi.r <- raster('data/gis_data/terra_clim/swi_norms/terraclim_boreal_swi_mean_2000to2019_Cx100_laea_4km.tif')/100
lc.col.df <- fread('data/gis_data/landcov/landcov_colors.csv')
  
site.trnds.dt <- site.trnds.dt[trend.period == '2000to2019']
# site.trnds.dt <- site.trnds.dt[trend.cat != 'no_trend']

site.trnds.dt$landcov.name <- site.dt$landcov.name[match(site.trnds.dt$site, site.dt$site)]
site.trnds.dt$swi <- site.dt$swi.avg.degC[match(site.trnds.dt$site, site.dt$site)]

site.trnds.dt[, trend.cat := factor(trend.cat, levels = c('browning','no_trend','greening'), labels = c('browning','none','greening'))]

# compute average SWI of each land cover type (used to order these later on) and add land cover colors
setnames(lc.col.df, 'lc.class', 'landcov.name')

swi.biome.avg.dt <- site.dt[, .(landcov.name = 'Biome', swi.avg = mean(swi.avg.degC))]
swi.lc.avg.dt <- site.dt[, .(swi.avg = mean(swi.avg.degC)), by = 'landcov.name']
setorder(swi.lc.avg.dt, swi.avg)
swi.lc.avg.dt <- rbind(swi.lc.avg.dt, swi.biome.avg.dt)

lc.col.df <- lc.col.df[swi.lc.avg.dt, on = 'landcov.name']
lc.col.df[landcov.name == 'Biome', col := 'black']


# permutation tests for differences in SWI between G/B for each LC class -----------------------------------------------------------
test.dt <- site.trnds.dt[, c('site','trend.cat','rep','landcov.name','swi'), with = T]
test.dt <- test.dt[trend.cat != 'none']

biome.test.avg.dt <- test.dt[, .(landcov.name = 'Biome', swi.avg = mean(swi)), by = c('rep','trend.cat')]
test.avg.dt <- test.dt[, .(swi.avg = mean(swi)), by = c('rep','landcov.name','trend.cat')]
test.avg.dt <- rbind(biome.test.avg.dt, test.avg.dt)

setorder(test.avg.dt, landcov.name, rep, trend.cat)
test.avg.dt

test.avg.dt <- test.avg.dt[,.SD[sample(.N)],by=c('landcov.name','trend.cat')] # shuffle
test.avg.dt <- test.avg.dt[, shuffle := 1]
test.avg.dt[, shuffle := cumsum(shuffle), by = c('landcov.name','trend.cat')]
test.avg.wide.dt <- dcast(test.avg.dt, shuffle + landcov.name ~ trend.cat, value.var = 'swi.avg')

test.avg.wide.dt[greening > browning, g.gt.b := 1]
test.avg.wide.dt[greening < browning, g.gt.b := 0]
test.avg.wide.dt[browning > greening, b.gt.g := 1]
test.avg.wide.dt[browning < greening, b.gt.g := 0]

test.avg.smry.dt <- test.avg.wide.dt[, .(frac.g.gt.b = sum(g.gt.b)/1000, frac.b.gt.g = sum(b.gt.g)/1000), by = landcov.name]
test.avg.smry.dt[frac.g.gt.b >= 0.95 | frac.b.gt.g >= 0.95, sig := 1]
test.avg.smry.dt
test.avg.smry.dt[is.na(sig), sig := 0]
test.avg.smry.dt$landcov.name <- factor(test.avg.smry.dt$landcov.name, levels = swi.lc.avg.dt$landcov.name)
setorder(test.avg.smry.dt, landcov.name)
test.avg.smry.dt[, yval := 1:10]
test.sig.dt <- test.avg.smry.dt[sig == 1]
test.sig.dt

# compute average SWI at sites that greenned vs browned for each land cover type ------------------------------------------
biome.swi.avg.mc.dt <- site.trnds.dt[trend.cat != '', .(landcov.name = 'Biome', swi.avg = mean(swi)), by = c('rep','trend.cat')]
swi.avg.mc.dt <- site.trnds.dt[trend.cat != '', .(swi.avg = mean(swi)), by = c('rep','trend.cat','landcov.name')]
swi.avg.mc.dt <- rbind(biome.swi.avg.mc.dt, swi.avg.mc.dt)
swi.avg.mc.dt$trend.cat <- factor(swi.avg.mc.dt$trend.cat, levels = c('greening','none','browning'))

swi.avg.mc.smry.dt <- swi.avg.mc.dt[, .(swi.avg.med = round(median(swi.avg),1), 
                                        swi.avg.q025 = round(quantile(swi.avg, 0.025),1), 
                                        swi.avg.q975 = round(quantile(swi.avg, 0.975),1)),
                                    by = c('landcov.name','trend.cat')]
swi.avg.mc.smry.dt
swi.avg.mc.smry.dt$landcov.name <- factor(swi.avg.mc.smry.dt$landcov.name, levels = swi.lc.avg.dt$landcov.name)
setorder(swi.avg.mc.smry.dt, landcov.name)

fwrite(swi.avg.mc.smry.dt, 'output/lsat_vi_gs_trends_summarized_by_swi.csv')

# MAP OF MEAN ANNUAL SWI -----------------------------------------------------------------------------------------------------------------
boreal.tidy <- tidy(boreal.shp)
land.45n.tidy <- tidy(land.45n.shp)
swi.spdf <- raster2spdf(swi.r)
lwr <- as.numeric(quantile(swi.spdf$value, 0.025))
upr <- as.numeric(quantile(swi.spdf$value, 0.975))
swi.spdf$value[swi.spdf$value < lwr] <- lwr
swi.spdf$value[swi.spdf$value > upr] <- upr

fig1 <- ggplot() +  
  geom_polygon(data=land.45n.tidy, aes(x = long, y = lat, group = group), color = "gray10", size = 0.1, fill = "gray80") +
  # geom_polygon(data=boreal.tidy, aes(x = long, y = lat, group = group), color = NA, size = 0.1, fill = "white") +
  geom_raster(data=swi.spdf, aes(x=x, y=y, fill=value), alpha=1) + 
  geom_polygon(data=boreal.tidy, aes(x = long, y = lat, group = group), color = "black", size = 0.2, fill = NA) +
  scale_fill_gradientn(colours = plasma(6)) + #, values = c(0, .3, .5, .6, .7, 1.0)) + 
  labs(fill = expression('Mean summer warmth index ('*degree*'C)')) + coord_equal() + theme_void() +
  theme(legend.position="bottom", legend.box="horizontal", legend.text=element_text(size=12), 
        legend.title=element_text(size=14), legend.key.width=unit(1.5, "cm")) + 
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)) + 
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

fig1


# MEAN ANNUAL SWI FOR EACH TREND CLASS --------------------------------------------------------------------------------------------------
trend.cols <- c('springgreen3','gray','saddlebrown')
fig2 <- ggplot(swi.avg.mc.smry.dt, aes(x=landcov.name, y=swi.avg.med, group=trend.cat, fill=trend.cat)) + 
  geom_errorbar(aes(ymin = swi.avg.q025, ymax = swi.avg.q975), position = position_dodge(width = 0.5), width = 0.2, color = 'gray30', alpha = 0.5) +
  geom_point(aes(color = trend.cat, fill = trend.cat, shape=detectable), position = position_dodge(width = 0.5), shape = 21, size = 4, color = 'black', na.rm=T) + 
  scale_fill_manual(name = 'Trend:', values = trend.cols, guide = guide_legend(reverse = TRUE)) +
  coord_flip() + theme_bw() + theme(legend.position = c(0.8,0.2), legend.text=element_text(size=10), legend.title=element_text(size=12), 
                                    axis.text.y = element_text(color = lc.col.df$col), axis.text=element_text(size=12), axis.title=element_text(size=14)) + 
  labs(x = "", y = expression('Mean summer warmth index ('*degree*'C'*')')) +
  geom_vline(xintercept = c(9.5), linetype = 'dashed') + 
  annotate("text", x = test.sig.dt$yval, y=40, label = c("★"), size =  5)

fig2

# COMBINE FIGURES ------------------------------------------------------------------------------------------------------------------------
# ggarrange(fig1, fig2, ncol = 1, nrow = 2, labels=c('a','b'), heights = c(1,1), widths = c(1,1), label.x = c(0.1, 0.1), label.y = 0.98, align = 'v')
# ggsave('figures/paper/fig_6_swi.jpg', width = 4.5, height = 8, units = 'in')

ggarrange(fig1, fig2, ncol = 2, nrow = 1, labels=c('a','b'), heights = c(1,1), widths = c(1,1.2), label.x = c(0.91, 0.91), label.y = 0.98)
ggsave('figures/paper/fig_6_swi.jpg', width = 9, height = 4, units = 'in', dpi = 400)
