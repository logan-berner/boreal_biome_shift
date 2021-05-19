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
lat.frac.trnds.dt <- fread('output/lsat_vi_gs_latitude_frac_trends_summary.csv')

# GENERAL DATA PREP =================================================================================================================

# focus on 2000 to 2019
lat.frac.trnds.dt <- lat.frac.trnds.dt[trend.period == '2000to2019']
lat.frac.trnds.dt <- lat.frac.trnds.dt[trend.cat != '' & trend.cat != 'no_trend']

lat.frac.trnds.smry.dt <- lat.frac.trnds.dt[, .(pcnt.sites=median(pcnt.sites), pcnt.sites.q025=quantile(pcnt.sites,0.025), pcnt.sites.q975=quantile(pcnt.sites,0.975)),
                                            by = c('latitude.rnd','trend.cat')]

setorder(lat.frac.trnds.smry.dt, trend.cat, latitude.rnd)

# g2b=median(g2b), g2b.q025=quantile(g2b,0.025), g2b.q975=quantile(g2b,0.975), n.MC = .N),

# colors
trnd.cols <- c('tomato3','springgreen3')


# FIGURE PANEL A: FRAC TRENDS BY LAND COVER TYPE =============================================================================================

ggplot(lat.frac.trnds.smry.dt, aes(x=latitude.rnd, y=pcnt.sites)) + 
  geom_line(aes(group=trend.cat, color = trend.cat)) + 
  scale_fill_manual(name = expression('Greenness \n trend'), values = trnd.cols, guide = guide_legend(reverse = TRUE)) +
  theme_bw() +
  theme(legend.position = c(0.5,0.95), legend.text=element_text(size=12), legend.title=element_text(size=12), 
        axis.text.x = element_text(angle = 0), axis.text=element_text(size=12), axis.title=element_text(size=12)) + 
  labs(x = 'Latitude (degrees)', y = 'Greenness trends (% of sites)') 


# ggsave('figures/paper/fig_XX_lsat_vi_trend_frac_by_latitude.jpg', width = 5, height = 5, units = 'in')

# END PLOT =====================================================================================================