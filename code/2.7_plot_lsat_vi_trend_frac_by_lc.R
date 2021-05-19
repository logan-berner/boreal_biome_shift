# THIS R SCRIPT CREATES A MULTI-PANEL FIGURE SUMMARIZING VEGETATION GREENING AND BROWING W/IN LAND COVER TYPES ACROSS THE BOREAL FOREST BIOME.
# AUTHOR: LOGAN BERNER, NAU
# DATE: 2021-04-28
rm(list=ls())
require(data.table)
require(dplyr)
require(ggplot2)
require(ggpubr)
require(viridis)
require(tidyr)
setwd('/projects/arctic/users/lberner/boreal_biome_shift/')

quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

# LOAD FILES =================================================================================================================
landcov.frac.trnds.dt <- do.call("rbind", lapply(list.files('output/lsat_vi_gs_landcov_trends_frac/mc_reps_tabular/', full.names = T), fread))
landcov.frac.trnds.smry.dt <- fread('output/lsat_vi_gs_landcov_trends_frac/lsat_vi_gs_boreal_landcov_frac_trends_summary.csv')
biome.frac.trnds.smry.dt <- fread('output/lsat_vi_gs_biome_trends_frac/lsat_vi_gs_boreal_biome_frac_trends_summary.csv')
site.dt <- fread('output/boreal_sample_site_climate_and_landcover.csv')


# GENERAL DATA PREP =================================================================================================================

# focus on 2000 to 2019
landcov.frac.trnds.dt <- landcov.frac.trnds.dt[trend.period == '2000to2019']
landcov.frac.trnds.smry.dt <- landcov.frac.trnds.smry.dt[trend.period == '2000to2019']
biome.frac.trnds.smry.dt <- biome.frac.trnds.smry.dt[trend.period == '2000to2019']

landcov.frac.trnds.dt <- landcov.frac.trnds.dt[trend.cat != '']

# combine biome-wide and land cover specific summaries 
biome.frac.trnds.smry.dt[, landcov.name := "Biome"]
landcov.frac.trnds.smry.dt <- rbind(landcov.frac.trnds.smry.dt, biome.frac.trnds.smry.dt)

# set factor levels and labels
landcov.frac.trnds.smry.dt <- landcov.frac.trnds.smry.dt[, trend.cat := factor(trend.cat, levels = c('browning','greening','no_trend'), labels = c('browning','greening','no trend'))]
landcov.frac.trnds.dt <- landcov.frac.trnds.dt[, landcov.name := factor(landcov.name, levels = c('Sparse','Shrubland','Grassland','Wetland','Mosaic','DNF','ENF','DBF','MF'))]

# colors
# trnd.cols <- c('lightsalmon3','springgreen3')
trnd.cols <- c('tomato3','springgreen3')
n.cols <- length(levels(landcov.frac.trnds.dt$landcov.name))
lc.cols <- viridis::turbo(n.cols)


# FIGURE PANEL A: FRAC TRENDS BY LAND COVER TYPE =============================================================================================

fig.a <- ggplot(landcov.frac.trnds.dt[trend.cat != 'no_trend'], aes(x=landcov.name, y=pcnt.sites, fill=trend.cat)) + 
  stat_summary(fun.data = quantiles_95, geom="boxplot", position = 'dodge', width = 0.75) + 
  geom_vline(xintercept = c(5.5), linetype = 'dashed') +
  annotate('text', 7.5, 45, label = "Forest", fontface = 2, angle = 90) +
  annotate('text', 3, 45, label = "Non-Forest", fontface = 2, angle = 90) + 
  scale_fill_manual(name = expression('Greenness \n trend'), values = trnd.cols, guide = guide_legend(reverse = TRUE)) +
  theme_bw() + coord_flip() + 
  theme(legend.position = 'none', legend.text=element_text(size=12), legend.title=element_text(size=12), 
        axis.text.x = element_text(angle = 0), axis.text.y = element_text(color = lc.cols), axis.text=element_text(size=12), axis.title=element_text(size=12)) + 
  labs(x = "", y ='Greenness trends (% of sites)') 

fig.a


# FIGURE PANEL B: SCATTER PLOT OF FRAC GREENING VS FRAC BROWNING ACROSS LAND COVER TYPES ======================================================

# compute correlation betweew Frac G vs B 
landcov.frac.trnds.for.cor.dt <- dcast(landcov.frac.trnds.dt[trend.cat != 'no_trend'], rep + landcov.name ~ trend.cat, value.var = 'pcnt.sites')
landcov.frac.trnds.cor.dt <- landcov.frac.trnds.for.cor.dt[, .(r = as.numeric(cor.test(browning, greening)$estimate)), by = rep]
landcov.frac.trnds.cor.smry.dt <- landcov.frac.trnds.cor.dt[, .(r.med = median(r), r.q025 = quantile(r, 0.025), r.q975 = quantile(r, 0.975))]
landcov.frac.trnds.cor.smry.dt <- round(landcov.frac.trnds.cor.smry.dt,2)

cor.r.txt <- paste0('r = ', sprintf("%.2f", landcov.frac.trnds.cor.smry.dt$r.med),' [', sprintf("%.2f", landcov.frac.trnds.cor.smry.dt$r.q975),' , ', 
                    sprintf("%.2f", landcov.frac.trnds.cor.smry.dt$r.q025),']')

# plot
landcov.frac.trnds.smry.wide.dt <- dcast(landcov.frac.trnds.smry.dt, landcov.name + vi.name ~ trend.cat, value.var = c('pcnt.sites', 'pcnt.sites.q025', 'pcnt.sites.q975'))
landcov.frac.trnds.smry.wide.dt$landcov.name <- factor(landcov.frac.trnds.smry.wide.dt$landcov.name, levels = levels(landcov.frac.trnds.dt$landcov.name))

fig.b <- ggplot(landcov.frac.trnds.smry.wide.dt[vi.name == 'ensemble' & landcov.name != "Biome"], 
                aes(x = pcnt.sites_greening, y = pcnt.sites_browning, color = landcov.name)) + 
  geom_errorbar(aes(ymin = pcnt.sites.q025_browning, ymax = pcnt.sites.q975_browning), width = 0.2, color = 'gray30', alpha = 0.5) +
  geom_errorbarh(aes(xmin = pcnt.sites.q025_greening, xmax = pcnt.sites.q975_greening), height = 0.2, color = 'gray30', alpha = 0.5) + 
  # geom_point(size = 4) + scale_color_manual(values = lc.cols) + geom_point(size = 4, color = 'black') +
  geom_point(aes(color = landcov.name, fill = landcov.name), shape = 21, size = 4, color = 'black') + scale_color_manual(values = lc.cols) + 
  annotate(geom = 'text', 25, Inf, label = cor.r.txt, vjust = 2) + 
  theme_bw() + theme(legend.position = 'none', axis.text=element_text(size=12), axis.title=element_text(size=12), 
                     axis.title.x = element_text(colour = 'black'), axis.title.y = element_text(colour = 'black')) + 
  xlab("Vegetation greening (% of sites)") + ylab('Vegetation browning (% of sites)') 

fig.b



fig.b <- ggplot(landcov.frac.trnds.smry.wide.dt[vi.name == 'ensemble' & landcov.name != "Biome"], 
                aes(x = pcnt.sites_greening, y = pcnt.sites_browning)) + 
  geom_errorbar(aes(ymin = pcnt.sites.q025_browning, ymax = pcnt.sites.q975_browning), width = 0.2, color = 'gray30', alpha = 0.5) +
  geom_errorbarh(aes(xmin = pcnt.sites.q025_greening, xmax = pcnt.sites.q975_greening), height = 0.2, color = 'gray30', alpha = 0.5) + 
  geom_point(aes(color = landcov.name, fill = landcov.name), shape = 21, size = 4, color = 'black') + 
  scale_color_manual(values = lc.cols) + scale_fill_manual(values = lc.cols) + 
  annotate(geom = 'text', 25, Inf, label = cor.r.txt, vjust = 2) + 
  theme_bw() + theme(legend.position = 'none', axis.text=element_text(size=12), axis.title=element_text(size=12), 
                     axis.title.x = element_text(colour = 'black'), axis.title.y = element_text(colour = 'black')) + 
  xlab("Vegetation greening (% of sites)") + ylab('Vegetation browning (% of sites)') 

fig.b



# FIGURE PANEL C: ===============================================================================================

# average tmax by landcover  
landcov.clim.avg.dt <- site.dt[, .(tmax.mean.2000to2019.jja.avg = mean(tmax.mean.2000to2019.jja.max.Cx100)/100), by = landcov.name]
landcov.frac.trnds.dt <- landcov.frac.trnds.dt[landcov.clim.avg.dt, on = 'landcov.name']
landcov.frac.trnds.dt <- landcov.frac.trnds.dt[, landcov.name := factor(landcov.name, levels = c('Sparse','Shrubland','Grassland','Wetland','Mosaic','DNF','ENF','DBF','MF'))]

# correlation between frac G/B and summer Tmax
landcov.frac.trnds.tmax.cor.dt <- landcov.frac.trnds.dt[, .(r = as.numeric(cor.test(pcnt.sites, tmax.mean.2000to2019.jja.avg)$estimate)), by = c('trend.cat','rep')]
landcov.frac.trnds.tmax.cor.smry.dt <- landcov.frac.trnds.tmax.cor.dt[, .(r.med = median(r), r.q025 = quantile(r, 0.025), r.q975 = quantile(r, 0.975)), by = trend.cat]

grn.tmax.cor.r.txt <- paste0('r = ', sprintf("%.2f", landcov.frac.trnds.tmax.cor.smry.dt$r.med[3]),' [', sprintf("%.2f", landcov.frac.trnds.tmax.cor.smry.dt$r.q975[3]),' , ', 
                             sprintf("%.2f", landcov.frac.trnds.tmax.cor.smry.dt$r.q025[3]),']')

brn.tmax.cor.r.txt <- paste0('r = ', sprintf("%.2f", landcov.frac.trnds.tmax.cor.smry.dt$r.med[1]),' [', sprintf("%.2f", landcov.frac.trnds.tmax.cor.smry.dt$r.q025[1]),' , ', 
                             sprintf("%.2f", landcov.frac.trnds.tmax.cor.smry.dt$r.q975[1]),']')


# greening
fig.c <- ggplot(landcov.frac.trnds.dt[trend.cat == 'greening'], aes(x=tmax.mean.2000to2019.jja.avg, y=pcnt.sites, group = landcov.name, fill = landcov.name)) + 
  stat_summary(fun.data = quantiles_95, geom="boxplot", width = 0.15, alpha = 0.9) + 
  annotate(geom = 'text', 20.5, Inf, label = grn.tmax.cor.r.txt, vjust = 2) + 
  scale_fill_manual(values = lc.cols) + 
  theme_bw() + theme(legend.position = 'none', axis.text=element_text(size=12), axis.title=element_text(size=12), 
                     axis.title.y = element_text(colour = 'black')) + 
  xlab(expression('Mean summer high temperature ('*degree*'C'*')')) + ylab('Vegetation greening (% of sites)')

fig.c


# browning
fig.d <- ggplot(landcov.frac.trnds.dt[trend.cat == 'browning'], aes(x=tmax.mean.2000to2019.jja.avg, y=pcnt.sites, group = landcov.name, fill = landcov.name)) + 
  stat_summary(fun.data = quantiles_95, geom="boxplot", width = 0.15, alpha = 0.9) + 
  annotate(geom = 'text', 20.5, Inf, label = grn.tmax.cor.r.txt, vjust = 2) + 
  scale_fill_manual(values = lc.cols) + 
  theme_bw() + theme(legend.position = 'none', axis.text=element_text(size=12), axis.title=element_text(size=12), 
                     axis.title.y = element_text(colour = 'black')) + 
  xlab(expression('Mean summer high temperature ('*degree*'C'*')')) + ylab('Vegetation browning (% of sites)')

fig.d



# COMBINE PANELS =============================================================================================
fig.combo <- ggarrange(fig.a, fig.b, fig.c, fig.d, labels=c('(a)','(b)','(c)','(d)'), widths = c(1,1), label.x = c(0.85, 0.83), label.y = 0.98)
fig.combo

ggsave('figures/paper/fig_XX_lsat_vi_trend_frac_by_landcover.jpg', width = 8, height = 6.5, units = 'in')

# END PLOT =====================================================================================================