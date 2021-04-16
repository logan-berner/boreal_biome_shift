rm(list=ls())
require(data.table)
require(dplyr)
require(lattice)
require(latticeExtra)
require(grid)

setwd('C:/Users/lb968/Google Drive/research/nau/nasa_above/boreal_ndvi/')
# setwd('C:/Users/Logan/Google Drive/research/nau/nasa_above/boreal_ndvi/')

ndvi.trnd.site.cat.pcnt.by.lc <- fread('output/boreal_site_lsat_ndvi_trend_pcnt_by_landcov.csv', fill=T)
# ndvi.swi.cor.site.cat.pcnt.by.lc <- fread('output/lsat_ndvi_swi_correlations_pcnt_cat_by_zone.csv', fill=T)

#-----------------------------------------------------------------------------------------------------------
#------------------
# % TRENDING BY ZONE
#------------------
unique(ndvi.trnd.site.cat.pcnt.by.lc$landcov.name)

ndvi.trnd.site.cat.pcnt.by.lc <- ndvi.trnd.site.cat.pcnt.by.lc %>% 
  mutate(trend.cat = factor(trend.cat, levels = c('browning (p<0.05)','browning (p<0.10)','no trend','greening (p<0.10)','greening (p<0.05')))

ndvi.trnd.site.cat.pcnt.by.lc <- ndvi.trnd.site.cat.pcnt.by.lc %>% 
  mutate(landcov.name = factor(landcov.name, levels = c('Barren','Sparse','Wetland','Grassland','Shrubland','Mosaic','MF','DBF','DNF','ENF','All')))

ndvi.trnd.site.cat.pcnt.by.lc$pcnt.sites.label <- paste(round(ndvi.trnd.site.cat.pcnt.by.lc$pcnt.sites), '%', sep='')
ndvi.trnd.site.cat.pcnt.by.lc$pcnt.sites.label[ndvi.trnd.site.cat.pcnt.by.lc$pcnt.sites.label == '0%'] <- ''
ndvi.trnd.site.cat.pcnt.by.lc$pcnt.sites.label[ndvi.trnd.site.cat.pcnt.by.lc$pcnt.sites.label == '1%'] <- ''
ndvi.trnd.site.cat.pcnt.by.lc$pcnt.sites.label[ndvi.trnd.site.cat.pcnt.by.lc$pcnt.sites.label == '2%'] <- ''
ndvi.trnd.site.cat.pcnt.by.lc$pcnt.sites.label[ndvi.trnd.site.cat.pcnt.by.lc$pcnt.sites.label == '3%'] <- ''

ndvi.trnds.gte1985.site.by.lc <- ndvi.trnd.site.cat.pcnt.by.lc %>% filter(period == '1985-2017')
ndvi.trnds.gte2000.site.by.lc <- ndvi.trnd.site.cat.pcnt.by.lc %>% filter(period == '2000-2017')

#------------------
# % COR CATEGORY BY ZONE
#------------------

# ndvi.swi.cor.site.cat.pcnt.by.lc <- ndvi.swi.cor.site.cat.pcnt.by.lc %>% 
#   mutate(cor.cat = factor(cor.cat, levels = c('negative (p<0.05)','negative (p<0.10)','no cor.','positive (p<0.10)','positive (p<0.05')))
# 
# ndvi.swi.cor.site.cat.pcnt.by.lc <- ndvi.swi.cor.site.cat.pcnt.by.lc %>% 
#   mutate(lat.zone = factor(lat.zone, levels = c('High','Low','Oro','biome'), labels = c('High','Low','Oro','Arctic')))
# 
# ndvi.swi.cor.site.cat.pcnt.by.lc$pcnt.sites.label <- paste(round(ndvi.swi.cor.site.cat.pcnt.by.lc$pcnt.sites), '%', sep='')
# ndvi.swi.cor.site.cat.pcnt.by.lc$pcnt.sites.label[ndvi.swi.cor.site.cat.pcnt.by.lc$pcnt.sites.label == '0%'] <- ''
# ndvi.swi.cor.site.cat.pcnt.by.lc$pcnt.sites.label[ndvi.swi.cor.site.cat.pcnt.by.lc$pcnt.sites.label == '1%'] <- ''
# ndvi.swi.cor.site.cat.pcnt.by.lc$pcnt.sites.label[ndvi.swi.cor.site.cat.pcnt.by.lc$pcnt.sites.label == '2%'] <- ''
# 
# ndvi.swi.cor.gte1985.site.by.lc <- ndvi.swi.cor.site.cat.pcnt.by.lc %>% filter(period == '1985-2017')
# ndvi.swi.cor.gte2000.site.by.lc <- ndvi.swi.cor.site.cat.pcnt.by.lc %>% filter(period == '2000-2017')


#-------------------------------------------------------------------------------------------------------------------
# PLOT DETAILS
#-------------------------------------------------------------------------------------------------------------------
xlab.zone <- 'Landcover'
# ylab.pcnt.zone <- expression('(% of sites)')
ylab.pcnt.zone <- '% of sites'

my.cex=1.5
my.cex.adj=1

trend.cols <- c('lightsalmon4','lightsalmon3','gray50','springgreen3','springgreen4')
trend.key <- list(title='NDVI trend', corner=c(0.9,0.9),text=list(rev(levels(ndvi.trnds.gte2000.site.by.lc$trend.cat))), rect=list(col=rev(trend.cols)), cex=my.cex, ncols =5)

## define a lattice "axis function"
axis.L <-function(side, ..., line.col){
    if (side %in% c("bottom", "left")) {
      col <- trellis.par.get("axis.text")$col
      axis.default(side, ..., line.col = col)
      if (side == "bottom")
        grid::grid.lines(y = 0)
      if (side == "left")
        grid::grid.lines(x = 0)
    }
  }

## hide panel and strip borders by using col = NA
sty <- list()
sty$axis.line$col <- NA
sty$strip.border$col <- NA
sty$strip.background$col <- NA


# 2 PANEL FIGURE -- % trends ===========================================================================

plot.pcnt.trnds.gte1985 <- barchart(landcov.name ~ pcnt.sites, ndvi.trnds.gte1985.site.by.lc, groups = trend.cat, stack = TRUE, horizontal = TRUE, col = trend.cols,
                                    ylab = '', xlab=list('Percent of sites', cex = my.cex), scales=list(x=list(draw=T), cex=my.cex, tck = c(1,0)), xlim = c(-5,110),
                                    panel = function(x,y,...){
                                      panel.barchart(x,y,...)
                                      panel.text(ndvi.trnds.gte1985.site.by.lc$pcnt.position, y, ndvi.trnds.gte1985.site.by.lc$pcnt.sites.label, cex=0.85, col='white')
                                      panel.text(105, y, ndvi.trnds.gte1985.site.by.lc$n.sites.lc, col = 'black', rot=90)
                                    })


plot.pcnt.trnds.gte2000 <- barchart(landcov.name ~ pcnt.sites, ndvi.trnds.gte2000.site.by.lc, groups = trend.cat, stack = TRUE, horizontal = TRUE, col = trend.cols,
                                    ylab = '', xlab=list('Percent of sites', cex = my.cex), scales=list(x=list(draw=T), cex=my.cex, tck = c(1,0)), xlim = c(-5,110),
                                    panel = function(x,y,...){
                                      panel.barchart(x,y,...)
                                      panel.text(ndvi.trnds.gte2000.site.by.lc$pcnt.position, y, ndvi.trnds.gte2000.site.by.lc$pcnt.sites.label, cex=0.85, col='white')
                                      panel.text(105, y, ndvi.trnds.gte2000.site.by.lc$n.sites.lc, col = 'black', rot=90)
                                    })


jpeg('figures/Landsat_NDVI_trend_freq_by_landcov.jpg', 9, 10, units = 'in', res = 400)

print(plot.pcnt.trnds.gte1985, position=c(0.1,0.47,0.95,0.95), more=T)
print(plot.pcnt.trnds.gte2000, position=c(0.1,0.0,0.95,0.48), more=T)

# labels
grid.text(expression(bold("(a)")), .19, 0.93, gp=gpar(fontsize=18))
grid.text(expression(bold("(b)")), .19, .46, gp=gpar(fontsize=18))

grid.text('1985 - 2017', 0.95, 0.73, gp=gpar(fontsize=19), rot=90)
grid.text('2000 - 2017', 0.95, 0.27, gp=gpar(fontsize=19), rot=90)
grid.text('Landcover', 0.05, 0.55, gp=gpar(fontsize=25), rot=90)
# grid.text('Boreal land cover', 0.55, 0.02, gp=gpar(fontsize=20))

dev.off()



# #-------------------------------------------------------------------------------------------------------------------
# # PLOT TRENDS
# #-------------------------------------------------------------------------------------------------------------------
# plot.pcnt.trnds.gte1985 <- barchart(pcnt.sites ~ landcov.name, ndvi.trnds.gte1985.site.by.lc, groups = trend.cat, stack = T, col = trend.cols,
#                                     ylab = '', xlab='', scales=list(x=list(draw=F), cex=my.cex, tck = c(1,0)), par.settings = sty, axis = axis.L,
#                                     panel=function(x,y,groups,subscripts,...){
#                                       panel.barchart(x,y,groups=groups,subscripts=subscripts,...)
#                                       panel.abline(v = 10.5, lty=3, lwd=2)
#                                     })
# 
# plot.pcnt.trnds.gte1985 <- plot.pcnt.trnds.gte1985 + layer(panel.text(x, ndvi.trnds.gte1985.site.by.lc$pcnt.position, ndvi.trnds.gte1985.site.by.lc$pcnt.sites.label, 
#                                                                       data = ndvi.trnds.gte1985.site.by.lc, cex=0.85, col='white'))
# 
# plot.pcnt.trnds.gte1985 <- plot.pcnt.trnds.gte1985 + layer(panel.text(x, 104, ndvi.trnds.gte1985.site.by.lc$n.sites.zone, 
#                                                                       data = ndvi.trnds.gte1985.site.by.lc, cex=0.85, col='black'))
# 
# plot.pcnt.trnds.gte2000 <- barchart(pcnt.sites ~ landcov.name, ndvi.trnds.gte2000.site.by.lc, groups = trend.cat, stack =T, col = trend.cols, 
#                                     ylab='', xlab='', scales=list(cex=my.cex, tck = c(1,0)), par.settings = sty, axis = axis.L,
#                                     panel=function(x,y,groups,subscripts,...){
#                                       panel.barchart(x,y,groups=groups,subscripts=subscripts,...)
#                                       panel.abline(v = 10.5, lty=3, lwd=2)
#                                     })
# plot.pcnt.trnds.gte2000 <- plot.pcnt.trnds.gte2000 + layer(panel.text(x, ndvi.trnds.gte2000.site.by.lc$pcnt.position, ndvi.trnds.gte2000.site.by.lc$pcnt.sites.label, 
#                                                                       data = ndvi.trnds.gte2000.site.by.lc, cex=0.85, col='white'))
# 
# plot.pcnt.trnds.gte2000 <- plot.pcnt.trnds.gte2000 + layer(panel.text(x, 104, ndvi.trnds.gte2000.site.by.lc$n.sites.zone, 
#                                                                       data = ndvi.trnds.gte2000.site.by.lc, cex=0.85, col='black'))


#--------------------------------------------------------------------------------
# PERCENT OF SITES WITH LANDSAT NDVI - SWI CORELATION BY ZONE
#--------------------------------------------------------------------------------

# plot.pcnt.clim.cor.gte1985 <- barchart(pcnt.sites ~ lat.zone, ndvi.swi.cor.gte1985.site.by.lc, groups = cor.cat, stack =T, col = trend.cols, 
#                                        ylab='', xlab='', scales=list(x = list(draw=F), y = list(labels=NULL), tck = c(1,0)), par.settings = sty, axis = axis.L,
#                                        panel=function(x,y,groups,subscripts,...){
#                                          panel.barchart(x,y,groups=groups,subscripts=subscripts,...)
#                                          panel.abline(v = 3.5, lty=3, lwd=2)
#                                        })
# 
# plot.pcnt.clim.cor.gte1985 <- plot.pcnt.clim.cor.gte1985 + layer(panel.text(x, ndvi.swi.cor.gte1985.site.by.lc$pcnt.position, ndvi.swi.cor.gte1985.site.by.lc$pcnt.sites.label, 
#                                                                             data = ndvi.swi.cor.gte1985.site.by.lc, cex=0.85, col='white'))
# 
# plot.pcnt.clim.cor.gte1985 <- plot.pcnt.clim.cor.gte1985 + layer(panel.text(x, 104, ndvi.swi.cor.gte1985.site.by.lc$n.sites.zone, 
#                                                                             data = ndvi.swi.cor.gte1985.site.by.lc, cex=0.85, col='black'))
# 
# plot.pcnt.clim.cor.gte2000 <- barchart(pcnt.sites ~ lat.zone, ndvi.swi.cor.gte2000.site.by.lc, groups = cor.cat, stack =T, col = trend.cols, 
#                                        ylab='', xlab='', scales=list(y=list(labels = NULL), cex=my.cex, tck = c(1,0)), par.settings = sty, axis = axis.L,
#                                        panel=function(x,y,groups,subscripts,...){
#                                          panel.barchart(x,y,groups=groups,subscripts=subscripts,...)
#                                          panel.abline(v = 3.5, lty=3, lwd=2)
#                                        })
# plot.pcnt.clim.cor.gte2000 <- plot.pcnt.clim.cor.gte2000 + layer(panel.text(x, ndvi.swi.cor.gte2000.site.by.lc$pcnt.position, ndvi.swi.cor.gte2000.site.by.lc$pcnt.sites.label, 
#                                                                             data = ndvi.swi.cor.gte2000.site.by.lc, cex=0.85, col='white'))
# 
# plot.pcnt.clim.cor.gte2000 <- plot.pcnt.clim.cor.gte2000 + layer(panel.text(x, 104, ndvi.swi.cor.gte2000.site.by.lc$n.sites.zone, 
#                                                                             data = ndvi.swi.cor.gte2000.site.by.lc, cex=0.85, col='black'))

# #--------------------------------------------------------------------------------
# # 4 PANEL FIGURES -- % trends then clim cors
# #--------------------------------------------------------------------------------
# 
# jpeg('figures/Landsat_NDVI_trend_NDVIvsSWI_cor_zonal_freq.jpg', 10,6, units = 'in', res = 400)
# 
# print(plot.pcnt.trnds.gte1985, position=c(0.1,0.50,0.56,0.95), more=T)
# print(plot.pcnt.trnds.gte2000, position=c(0.1,0.03,0.56,0.56), more=T)
# 
# # ndvi-clim cors
# print(plot.pcnt.clim.cor.gte1985, position=c(0.50,0.50,0.90,0.95), more=T)
# print(plot.pcnt.clim.cor.gte2000, position=c(0.50,0.03,0.90,0.56))
# 
# # labels
# grid.text(expression(bold("(a)")), .19, 0.93, gp=gpar(fontsize=18))
# grid.text(expression(bold("(b)")), .54, 0.93, gp=gpar(fontsize=18))
# grid.text(expression(bold("(c)")), .19, .51, gp=gpar(fontsize=18))
# grid.text(expression(bold("(d)")), .54, .51, gp=gpar(fontsize=18))
# 
# grid.text(expression('NDVI'['max']~'trend'), 0.35, 0.97, gp=gpar(fontsize=19), rot=0)
# grid.text(expression('NDVI'['max']~'-SWI correlation'), 0.71, 0.97, gp=gpar(fontsize=19), rot=0)
# grid.text('1985 - 2017', 0.90, 0.73, gp=gpar(fontsize=19), rot=90)
# grid.text('2000 - 2017', 0.90, 0.33, gp=gpar(fontsize=19), rot=90)
# grid.text('Percent of sites', 0.1, 0.55, gp=gpar(fontsize=19), rot=90)
# grid.text('Arctic zone', 0.55, 0.02, gp=gpar(fontsize=20))
# 
# dev.off()

#--------------------------------------------------------------------------------
# END SCRIPT
#--------------------------------------------------------------------------------