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
  require(sf)
  require(raster)
  require(rgdal)
  require(broom) # for tidy()
  require(ggspatial)
  
  setwd('/projects/arctic/users/lberner/boreal_biome_shift/')
  
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
  
  
  # LOAD FILES =================================================================================================================
  landcov.frac.trnds.dt <- do.call("rbind", lapply(list.files('output/lsat_vi_gs_landcov_trends_frac/mc_reps_tabular/', full.names = T), fread))
  landcov.frac.trnds.smry.dt <- fread('output/lsat_vi_gs_landcov_trends_frac/lsat_vi_gs_boreal_landcov_frac_trends_summary.csv')
  landcov.trnds.smry.dt <- fread('output/lsat_vi_gs_landcov_trends/lsat_vi_gs_boreal_landcov_trends_summary.csv')
  site.dt <- fread('output/boreal_sample_site_climate_and_landcover.csv')
  
  boreal.shp <- readOGR(dsn = 'data/gis_data/wwf_boreal_biome_laea.shp')
  land.45n.shp <-  readOGR(dsn = 'data/gis_data/land_45n_laea.shp')
  landcov.r <- raster('data/gis_data/landcov/esa_cci_landcov_consol_classes_boreal_aoi_2018_1500m_laea.tif')
  landcov.full.key <- fread('data/gis_data/landcov/ESACCI-LC-Legend.csv')
  
  
  # GENERAL DATA PREP =================================================================================================================
  
  # focus on 2000 to 2019
  landcov.frac.trnds.dt <- landcov.frac.trnds.dt[trend.period == '2000to2019']
  landcov.frac.trnds.smry.dt <- landcov.frac.trnds.smry.dt[trend.period == '2000to2019']
  landcov.frac.trnds.dt <- landcov.frac.trnds.dt[trend.cat != '']
  landcov.trnds.smry.dt <- landcov.trnds.smry.dt[trend.period == '2000to2019']
  
  # subset
  landcov.frac.trnds.smry.dt <- landcov.frac.trnds.smry.dt[vi.name == 'ensemble' & trend.cat != 'no_trend']
  
  # set factor levels and labels
  landcov.frac.trnds.smry.dt <- landcov.frac.trnds.smry.dt[, trend.cat := factor(trend.cat, levels = c('browning','greening'), labels = c('browning','greening'))]
  landcov.frac.trnds.smry.dt <- landcov.frac.trnds.smry.dt[, landcov.name := factor(landcov.name, levels = c('Sparse','Shrubland','Grassland','Wetland','Mosaic','DNF','ENF','DBF','MF'))]
  
  landcov.frac.trnds.dt <- landcov.frac.trnds.dt[, landcov.name := factor(landcov.name, levels = c('Sparse','Shrubland','Grassland','Wetland','Mosaic','DNF','ENF','DBF','MF'))]
  
  setorder(landcov.trnds.smry.dt,  -n.sites.landcov.q500)
  landcov.trnds.smry.dt <- landcov.trnds.smry.dt[, trend.cat := factor(trend.cat, levels = c('browning','no_trend','greening'), labels = c('browning','none','greening'))]
  landcov.trnds.smry.dt <- landcov.trnds.smry.dt[, landcov.name := factor(landcov.name, levels = rev(unique(landcov.trnds.smry.dt$landcov.name)))]
  
  # compute correlation betweew Frac G vs B 
  landcov.frac.trnds.for.cor.dt <- dcast(landcov.frac.trnds.dt[trend.cat != 'no_trend'], rep + landcov.name ~ trend.cat, value.var = 'pcnt.sites')
  landcov.frac.trnds.cor.dt <- landcov.frac.trnds.for.cor.dt[, .(r = as.numeric(cor.test(browning, greening)$estimate)), by = rep]
  landcov.frac.trnds.cor.smry.dt <- landcov.frac.trnds.cor.dt[, .(r.med = median(r), r.q025 = quantile(r, 0.025), r.q975 = quantile(r, 0.975))]
  landcov.frac.trnds.cor.smry.dt <- round(landcov.frac.trnds.cor.smry.dt,2)
  
  cor.r.txt <- paste0('r = ', sprintf("%.2f", landcov.frac.trnds.cor.smry.dt$r.med),' [', sprintf("%.2f", landcov.frac.trnds.cor.smry.dt$r.q975),' , ', 
                      sprintf("%.2f", landcov.frac.trnds.cor.smry.dt$r.q025),']')
  
  # cast wide 
  landcov.frac.trnds.smry.wide.dt <- dcast(landcov.frac.trnds.smry.dt, landcov.name + vi.name ~ trend.cat, value.var = c('pcnt.sites', 'pcnt.sites.q025', 'pcnt.sites.q975'))
  landcov.frac.trnds.smry.wide.dt$landcov.name <- factor(landcov.frac.trnds.smry.wide.dt$landcov.name, levels = levels(landcov.frac.trnds.dt$landcov.name))
  landcov.frac.trnds.smry.wide.dt <- landcov.frac.trnds.smry.wide.dt[vi.name == 'ensemble' & landcov.name != "Biome"]
  
  # FIGURE COLORS ===========================================================================================
  trnd.cols <- c('saddlebrown','springgreen3')
  n.cols <- length(levels(landcov.frac.trnds.dt$landcov.name))
  lc.col.df <- data.frame(lc.class = levels(landcov.frac.trnds.smry.dt$landcov.name), col = viridis::turbo(n.cols))
  fwrite(lc.col.df, 'data/gis_data/landcov/landcov_colors.csv')
  
  # LAND COVER MAP =================================================================================================
  landcov.key <- landcov.full.key[, .(code = first(rcls.val)), by = label]
  landcov.key <- landcov.key[code != 0 & code != 10]
  setnames(landcov.key, 'label','lc.class')
  lc.col.df <- landcov.key[lc.col.df, on = 'lc.class']
  lc.col.map.df <- copy(lc.col.df)
  lc.col.map.df <- setorder(lc.col.map.df, code)
  
  boreal.tidy <- tidy(boreal.shp)
  land.45n.tidy <- tidy(land.45n.shp)
  
  landcov.spdf <- raster2spdf(landcov.r)
  landcov.spdf <- subset(landcov.spdf, value != 10)
  landcov.spdf <- subset(landcov.spdf, value != 0)
  landcov.spdf <- landcov.spdf[order(landcov.spdf$value),]
  
  lc.map <- ggplot() +  
    geom_polygon(data=land.45n.tidy, aes(x = long, y = lat, group = group), color = "gray10", size = 0.1, fill = "gray80") +
    geom_polygon(data=boreal.tidy, aes(x = long, y = lat, group = group), color = NA, size = 0.1, fill = "white") +
    geom_raster(data=landcov.spdf, aes(x=x, y=y, fill=factor(value)), alpha=1) + 
    geom_polygon(data=boreal.tidy, aes(x = long, y = lat, group = group), color = "black", size = 0.2, fill = NA) +
    scale_fill_manual(values = lc.col.map.df$col, labels = lc.col.map.df$lc.class) +
    labs(fill = 'Land cover class') + coord_equal() + theme_void() +
    theme(legend.position="none", plot.margin = unit(c(0,0,0,0), "cm"))
  
  # lc.map
  
  
  # FIGURE PANEL B: OCCURRENCE OF TRENDS BY LAND COVER TYPE =============================================================================================
  setorder(landcov.trnds.smry.dt, landcov.name, -trend.cat)
  landcov.trnds.smry.dt[, error.pos := cumsum(lag(n.sites.landcov.trnd.q500, 1, default = 0)), by = c('landcov.name')]
  
  fig.a <- ggplot(landcov.trnds.smry.dt, aes(x=landcov.name, y=n.sites.landcov.trnd.q500, group=trend.cat, fill=trend.cat)) + 
    geom_bar(position = "stack", stat = "identity") + lims(y = c(0,33000)) + 
    geom_errorbar(data = landcov.trnds.smry.dt[trend.cat != 'none'], 
                  mapping = aes(ymin = error.pos+n.sites.landcov.trnd.q025, ymax = error.pos+n.sites.landcov.trnd.q975), 
                  width = 0.2, color = 'gray30', alpha = 0.75) + 
    scale_fill_manual(name = 'Trend', values = c(trnd.cols[1],'gray',trnd.cols[2]), guide = guide_legend(reverse = TRUE)) +
    coord_flip() + theme_bw() + theme(legend.position = c(0.6,0.3), legend.text=element_text(size=12), legend.title=element_text(size=14), 
                                      axis.text.y = element_text(color = lc.col.df$col[match(levels(landcov.trnds.smry.dt$landcov.name), lc.col.df$lc.class)]), 
                                      axis.text=element_text(size=12), axis.title=element_text(size=14)) + 
    labs(x = "", y ='Trend occurrence\n(number of sample sites)') 
  
  fig.a
  
  
  # FIGURE PANEL B: PREVALENCE OF TRENDS WITHIN EACH LAND COVER TYPE =============================================================================================
  fig.b <- ggplot(landcov.frac.trnds.smry.dt, aes(x=landcov.name, y=pcnt.sites, group=trend.cat, fill=trend.cat)) + 
    geom_errorbar(aes(ymin = pcnt.sites.q025, ymax = pcnt.sites.q975), position = position_dodge(width = 0.5), width = 0.2, color = 'gray30', alpha = 0.5) +
    geom_point(aes(color = trend.cat, fill = trend.cat, shape=detectable), position = position_dodge(width = 0.5), shape = 21, size = 4, color = 'black', na.rm=T) + 
    geom_vline(xintercept = c(5.5), linetype = 'dashed') + lims(y = c(0,50)) + 
    scale_fill_manual(name = 'Trend', values = trnd.cols) +
    annotate('text', 7.5, 49, label = "Forest", size = 3, fontface = 2, angle = 90) +
    annotate('text', 3, 49, label = "Non-Forest", size = 3, fontface = 2, angle = 90) + 
    coord_flip() + theme_bw() + theme(legend.position = 'none', axis.text.y = element_text(color = lc.col.df$col), 
                                      axis.text=element_text(size=12), axis.title=element_text(size=14)) + 
    labs(x = "", y ='Trend prevalence\n(% of sample sites)') 
  
  fig.b

    
  # FIGURE PANEL C: SCATTER PLOT OF FRAC GREENING VS FRAC BROWNING ACROSS LAND COVER TYPES ======================================================
  fig.c <- ggplot(landcov.frac.trnds.smry.wide.dt, aes(x = pcnt.sites_greening, y = pcnt.sites_browning)) + 
    geom_errorbar(aes(ymin = pcnt.sites.q025_browning, ymax = pcnt.sites.q975_browning), width = 0.2, color = 'gray30', alpha = 0.5) +
    geom_errorbarh(aes(xmin = pcnt.sites.q025_greening, xmax = pcnt.sites.q975_greening), height = 0.2, color = 'gray30', alpha = 0.5) + 
    geom_point(aes(color = landcov.name, fill = landcov.name), shape = 21, size = 4, color = 'black') + 
    scale_color_manual(values = lc.col.df$col) + scale_fill_manual(values = lc.col.df$col) + 
    annotate(geom = 'text', 25, Inf, size = 5, label = cor.r.txt, vjust = 2) + lims(y = c(0,45), x = c(0,45)) +
    theme_bw() + theme(legend.position = 'none', axis.text=element_text(size=12), axis.title=element_text(size=14)) + 
    xlab("Greening prevalence\n(% of sample sites)") + ylab('Browning prevalence\n(% of sample sites)') 
  
  fig.c
  
  
  # COMBINE PANELS =============================================================================================
  fig.col1 <- ggarrange(lc.map, fig.b, labels=c('a','c'), align = 'v', ncol = 1, nrow = 2, label.x = 0.91, label.y = 0.98)
  fig.col2 <- ggarrange(fig.a, fig.c, labels=c('b','d'), ncol = 1, nrow = 2, label.x = 0.91, label.y = 0.98)
  fig.combo <- ggarrange(fig.col1, fig.col2, ncol = 2)
  fig.combo
  ggsave('figures/paper/fig_3_landcover.jpg', width = 9, height = 6, units = 'in', dpi = 400)
  
  
  # END PLOT =====================================================================================================
  
  
  # # REDUCE RESOLUTION OF LAND COVER DATA FOR PLOTTING (OTHERWISE ERRORS OUT FROM TOO LARGE FILES) ==========================================
  # Mode <- function(x,...) {
  #   ux <- na.omit(unique(x))
  #   ux[which.max(tabulate(match(x, ux)))]
  # }
  # 
  # landcov.r <- raster('data/gis_data/landcov/esa_cci_landcov_consol_classes_2018_300m_laea.tif')
  # aoi.r <- raster('data/gis_data/boreal_sampling_frame_300m_laea.tif')
  # 
  # aoi.sum.1500m.r <- raster::aggregate(aoi.r, fact = c(5,5), fun = sum, na.rm=T)
  # aoi.pcnt.1500m.r <- aoi.sum.1500m.r / 25 * 100
  # aoi.abv.50pcnt.1500m.r <- aoi.pcnt.1500m.r >= 50
  # 
  # # mask to sample frame
  # landcov.r <- mask(landcov.r, aoi.r)
  # writeRaster(landcov.r, 'data/gis_data/landcov/esa_cci_landcov_consol_classes_boreal_aoi_2018_300m_laea.tif', overwrite=T)
  # landcov.r <- raster('data/gis_data/landcov/esa_cci_landcov_consol_classes_boreal_aoi_2018_300m_laea.tif')
  # 
  # # aggregate to coarser resolution
  # landcov.1500m.r <- raster::aggregate(landcov.r, fact = c(5,5), fun = Mode)
  # landcov.1500m.r <- mask(landcov.1500m.r, aoi.abv.50pcnt.1500m.r)
  # writeRaster(landcov.1500m.r, 'data/gis_data/landcov/esa_cci_landcov_consol_classes_boreal_aoi_2018_1500m_laea.tif', overwrite=T)
  # 