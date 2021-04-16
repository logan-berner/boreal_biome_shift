# DESCRIPTION ==========================================================================
# THIS R SCRIPT CREATES BWPLOTS OF CLIMATE CONDITIONS IN AREAS WITH CONTRASTING LANDSAT NDVI TRENDS

# SETUP ==========================================================================
rm(list=ls())
require(data.table)
require(ggplot2)

# setwd('C:/Users/lb968/Google Drive/research/nau/nasa_above/boreal_ndvi/')
setwd('C:/Users/Logan/Google Drive/research/nau/nasa_above/boreal_ndvi/')

# READ DATA, FACTORS, SUBSETS ==========================================================================
site.dt <- fread('output/boreal_site_lsat_ndvi_trends.csv')

site.dt$trend.cat[site.dt$trend.cat == 'browning.sig.p10'] <- 'neg.'
site.dt$trend.cat[site.dt$trend.cat == 'browning.sig.p5'] <- 'neg.'
site.dt$trend.cat[site.dt$trend.cat == 'insig'] <- 'none'
site.dt$trend.cat[site.dt$trend.cat == 'greening.sig.p5'] <- 'pos.'
site.dt$trend.cat[site.dt$trend.cat == 'greening.sig.p10'] <- 'pos.'

trend.cols <- c('lightsalmon4','gray50','springgreen4')
site.dt$trend.col <- NA
site.dt$trend.col[site.dt$trend.cat == 'neg.'] <- trend.cols[1]
site.dt$trend.col[site.dt$trend.cat == 'none'] <- trend.cols[2]
site.dt$trend.col[site.dt$trend.cat == 'pos.'] <- trend.cols[3]

site.dt <- site.dt[trend.cat != 'none']

site.gte1985.dt <- site.dt[period == '1985-2017']
site.gte2000.dt <- site.dt[period == '2000-2017']

n.samples <- 500
site.gte1985.dt <- site.gte1985.dt[sample(1:nrow(site.gte1985.dt), n.samples)]
site.gte2000.dt <- site.gte2000.dt[sample(1:nrow(site.gte2000.dt), n.samples)]

# PLOTTING PARAMETERS ==========================================================================
lab.tmax <- expression('Maximum summer temperature ('~degree~'C)')
lab.ppt <-  expression('Total water year precipitation (mm)')

my.cex = 1.25

# PLOT: TMAX vs PPT with TRENDS FROM 1985-2017 ==========================================================================
bioclim.fig <- ggplot(site.gte1985.dt, aes(x=tmax.1985.jja.C+tmax.1985to2017.jja.chng.C, y=ppt.1985.wy.mm+ppt.1985to2017.wy.chng.mm))
bioclim.fig <- bioclim.fig + geom_segment(aes(y=ppt.1985.wy.mm, yend=ppt.1985.wy.mm+ppt.1985to2017.wy.chng.mm,
                                              x=tmax.1985.jja.C, xend=tmax.1985.jja.C+tmax.1985to2017.jja.chng.C),
                                          colour = site.gte1985.dt$trend.col, alpha = 0.5, 
                                          arrow = arrow(angle = 25, length=unit(0.10,"cm"), ends="last", type = "closed"))
bioclim.fig <- bioclim.fig + theme_bw() + labs(x=lab.tmax, y=lab.ppt)
bioclim.fig <- bioclim.fig + scale_x_continuous(limits = c(16,26), breaks=seq(16,26,2))
bioclim.fig <- bioclim.fig + scale_y_continuous(limits = c(200,1400), breaks=seq(200,1400,200))
bioclim.fig <- bioclim.fig + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 
bioclim.fig

jpeg('figures/boreal_bioclim_change_1985to2017.jpg', width = 5, height = 4, res = 400, units = 'in')
bioclim.fig
dev.off()



# PLOT: TMAX vs PPT with TRENDS FROM 2000 - 2017 ==========================================================================
bioclim.fig <- ggplot(site.gte2000.dt, aes(x=tmax.2000.jja.C+tmax.2000to2017.jja.chng.C, y=ppt.2000.wy.mm+ppt.2000to2017.wy.chng.mm))
bioclim.fig <- bioclim.fig + geom_segment(aes(y=ppt.2000.wy.mm, yend=ppt.2000.wy.mm+ppt.2000to2017.wy.chng.mm,
                                              x=tmax.2000.jja.C, xend=tmax.2000.jja.C+tmax.2000to2017.jja.chng.C),
                                          colour = site.gte2000.dt$trend.col, alpha = 0.5, 
                                          arrow = arrow(angle = 25, length=unit(0.10,"cm"), ends="last", type = "closed"))
bioclim.fig <- bioclim.fig + theme_bw() + labs(x=lab.tmax, y=lab.ppt)
bioclim.fig <- bioclim.fig + scale_x_continuous(limits = c(16,26), breaks=seq(16,26,2))
bioclim.fig <- bioclim.fig + scale_y_continuous(limits = c(200,1400), breaks=seq(200,1400,200))
bioclim.fig <- bioclim.fig + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 
bioclim.fig

jpeg('figures/boreal_bioclim_change_2000to2017.jpg', width = 5, height = 4, res = 400, units = 'in')
bioclim.fig
dev.off()

