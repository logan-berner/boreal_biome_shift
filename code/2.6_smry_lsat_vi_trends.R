# ABOUT THIS SCRIPT  ==============================================================================================================
# This R script summarizes boreal forest vegetation greenness trends across Monte Carlo simulations
# AUTHOR: LOGAN BERNER, NAU
# DATE: 2021-04-27

# SET UP WORKSPACE ==============================================================================================================
rm(list=ls())
.libPaths(c(.libPaths(), "~/R/", '/home/lb968/R/4.0.2'))
require(data.table)
require(ggplot2)
require(raster)
require(dplyr)
require(rgdal)

tmp.dir <- '/scratch/lb968/lsat_vi_trend_smry/'
tempfile(tmpdir=tmp.dir)
rasterOptions(tmpdir=tmp.dir)

setwd('/projects/arctic/users/lberner/boreal_biome_shift/')

# READ IN FILES ================================================================================================================
site.trnds.dt <- do.call("rbind", lapply(list.files('output/lsat_vi_gs_site_trends/mc_reps/', full.names = T), fread))
landcov.frac.trnds.dt <- do.call("rbind", lapply(list.files('output/lsat_vi_gs_landcov_trends_frac/mc_reps_tabular/', full.names = T), fread))
ecounit.frac.trnds.dt <- do.call("rbind", lapply(list.files('output/lsat_vi_gs_ecounit_trends_frac/mc_reps_tabular/', full.names = T), fread))
ecounit.median.trnds.dt <- do.call("rbind", lapply(list.files('output/lsat_vi_gs_ecounit_trends_median/mc_reps_tabular/', full.names = T), fread))
biome.frac.trnds.dt <- do.call("rbind", lapply(list.files('output/lsat_vi_gs_biome_trends_frac/mc_reps_tabular/', full.names = T), fread))

ecounit.r <- raster('data/gis_data/ecological_land_unit_boreal_aoi_300m_laea.tif')     
boreal.r <- raster('data/gis_data/wwf_boreal_biome_laea_300m.tif')
boreal.r[] <- NA
boreal.pxl.dt <- data.table(cellid = 1:ncell(boreal.r), ecounit = values(ecounit.r))
boreal.pxl.dt <- na.omit(boreal.pxl.dt)

# # SITE TRENDS NDVI ================================================================================================================
# site.trnds.smry.dt <- site.trnds.dt[, .(int=median(int, na.rm = T), int.q025=quantile(int,0.025, na.rm = T), int.q975=quantile(int,0.975, na.rm = T),
#                                         vi.change=median(total.change, na.rm = T), vi.change.q025=quantile(total.change,0.025, na.rm = T), vi.change.q975=quantile(total.change,0.975, na.rm = T),
#                                         vi.change.pcnt=median(total.change.pcnt, na.rm = T), vi.change.pcnt.q025=quantile(total.change.pcnt,0.025, na.rm = T), vi.change.pcnt.q975=quantile(total.change.pcnt,0.975, na.rm = T)),
#                                     by = c('site','period')]
# 
# fwrite(site.trnds.smry.dt, 'output/lsat_vi_gs_site_trends/boreal_lsat_vi_site_trend_summary.csv')

# SITES TRENDS BY LATITUDE ===============================================================================================================================
site.trnds.dt[, latitude.rnd := round(latitude/5)*5] # round to nearest 5th degree
lat.smry.dt <- site.trnds.dt[, .(n.sites = .N), by = c('trend.period','latitude.rnd','rep','trend.cat')]
lat.smry.dt[, n.sites.lat := sum(n.sites), by = c('trend.period','latitude.rnd','rep')]
lat.smry.dt[, pcnt.sites := n.sites / n.sites.lat * 100]
fwrite(lat.smry.dt, 'output/lsat_vi_gs_latitude_frac_trends_summary.csv')


# BIOME: FRACTION OF TRENDS IN EACH CATEGORY BY VEG INDEX =========================================================================================================
biome.frac.trnds.dt <- biome.frac.trnds.dt[trend.cat != '']
biome.frac.trnds.dt[, n.sites := as.numeric(n.sites)]

## calc ratio of greening to browning
g2b.dt <- dcast(biome.frac.trnds.dt, trend.period + rep ~ trend.cat, value.var = 'pcnt.sites')
g2b.dt <- g2b.dt[, g2b := greening / browning][, c('browning','greening','no_trend') := NULL]
biome.frac.trnds.dt <- g2b.dt[biome.frac.trnds.dt, on = c('trend.period','rep')]

# summary for each VI 
biome.frac.trnds.smry.dt <- biome.frac.trnds.dt[, .(n.sites=median(n.sites), n.sites.q025=quantile(n.sites,0.025), n.sites.q975=quantile(n.sites,0.975),
                                                    pcnt.sites=median(pcnt.sites), pcnt.sites.q025=quantile(pcnt.sites,0.025), pcnt.sites.q975=quantile(pcnt.sites,0.975), 
                                                    g2b=median(g2b), g2b.q025=quantile(g2b,0.025), g2b.q975=quantile(g2b,0.975), n.MC = .N),
                                                by = c('trend.period','vi.name','trend.cat')]

# summary across ensemble
biome.frac.trnds.ens.smry.dt <- biome.frac.trnds.dt[, .(n.sites=median(n.sites), n.sites.q025=quantile(n.sites,0.025), n.sites.q975=quantile(n.sites,0.975),
                                                        pcnt.sites=median(pcnt.sites), pcnt.sites.q025=quantile(pcnt.sites,0.025), pcnt.sites.q975=quantile(pcnt.sites,0.975), 
                                                        g2b=median(g2b), g2b.q025=quantile(g2b,0.025), g2b.q975=quantile(g2b,0.975), n.MC = .N, vi.name = 'ensemble'),
                                                    by = c('trend.period','trend.cat')]

# combine ensemble and all VIs
biome.frac.trnds.smry.dt <- rbind(biome.frac.trnds.smry.dt, biome.frac.trnds.ens.smry.dt)

# round numeric cols
biome.frac.trnds.smry.dt <- cbind(biome.frac.trnds.smry.dt[, 1:3], round(biome.frac.trnds.smry.dt[,-c(1:3)],1))

# fancy table
biome.frac.trnds.smry.fancy.dt <- biome.frac.trnds.smry.dt[, .(n.sites = paste0(sprintf("%.0f", n.sites),' [',sprintf("%.0f", n.sites.q025),', ',sprintf("%.0f", n.sites.q975),']'),
                                                               pcnt.sites = paste0(sprintf("%.1f", pcnt.sites),' [',sprintf("%.1f", pcnt.sites.q025),', ',sprintf("%.1f", pcnt.sites.q975),']'),
                                                               g2b = paste0(sprintf("%.1f", g2b),' [',sprintf("%.1f", g2b.q025),', ',sprintf("%.1f", g2b.q975),']'), n.MC = n.MC), 
                                                           by=c('trend.period','vi.name','trend.cat')]

biome.frac.trnds.smry.fancy.dt$vi.name <- factor(biome.frac.trnds.smry.fancy.dt$vi.name, levels = c('ndvi','evi2','nirv','kndvi','ensemble'))
biome.frac.trnds.smry.fancy.dt <- biome.frac.trnds.smry.fancy.dt[order(trend.period, vi.name)]

# write out
fwrite(biome.frac.trnds.smry.dt, 'output/lsat_vi_gs_biome_trends_frac/lsat_vi_gs_boreal_biome_frac_trends_summary.csv')
fwrite(biome.frac.trnds.smry.fancy.dt, 'output/lsat_vi_gs_biome_trends_frac/lsat_vi_gs_boreal_biome_frac_trends_summary_fancy.csv')


# LANDCOVER: FRACTION OF TRENDS IN EACH LANDCOVER CATEGORY ===========================================================================
landcov.frac.trnds.dt <- landcov.frac.trnds.dt[trend.cat != '']
landcov.frac.trnds.dt[, n.sites := as.numeric(n.sites)]

## calc ratio of greening to browning
g2b.dt <- dcast(landcov.frac.trnds.dt, trend.period + rep + landcov.name ~ trend.cat, value.var = 'pcnt.sites')
g2b.dt <- g2b.dt[, g2b := greening / browning][, c('browning','greening','no_trend') := NULL]
landcov.frac.trnds.dt <- g2b.dt[landcov.frac.trnds.dt, on = c('trend.period','rep','landcov.name')]

# summary for each VI 
landcov.frac.trnds.smry.dt <- landcov.frac.trnds.dt[, .(n.sites=median(n.sites), n.sites.q025=quantile(n.sites,0.025), n.sites.q975=quantile(n.sites,0.975),
                                                        pcnt.sites=median(pcnt.sites), pcnt.sites.q025=quantile(pcnt.sites,0.025), pcnt.sites.q975=quantile(pcnt.sites,0.975), 
                                                        g2b=median(g2b), g2b.q025=quantile(g2b,0.025), g2b.q975=quantile(g2b,0.975), n.MC = .N),
                                                    by = c('trend.period','vi.name','landcov.name','trend.cat')]

# summary across ensemble
landcov.frac.trnds.ens.smry.dt <- landcov.frac.trnds.dt[, .(n.sites=median(n.sites), n.sites.q025=quantile(n.sites,0.025), n.sites.q975=quantile(n.sites,0.975),
                                                            pcnt.sites=median(pcnt.sites), pcnt.sites.q025=quantile(pcnt.sites,0.025), pcnt.sites.q975=quantile(pcnt.sites,0.975), 
                                                            g2b=median(g2b), g2b.q025=quantile(g2b,0.025), g2b.q975=quantile(g2b,0.975), n.MC = .N, vi.name = 'ensemble'),
                                                        by = c('trend.period','landcov.name','trend.cat')]

# combine ensemble and all VIs
landcov.frac.trnds.smry.dt <- rbind(landcov.frac.trnds.smry.dt, landcov.frac.trnds.ens.smry.dt)

# round numeric cols
landcov.frac.trnds.smry.dt <- cbind(landcov.frac.trnds.smry.dt[, 1:4], round(landcov.frac.trnds.smry.dt[,-c(1:4)],1))

# fancy table
landcov.frac.trnds.smry.fancy.dt <- landcov.frac.trnds.smry.dt[, .(n.sites = paste0(sprintf("%.0f", n.sites),' [',sprintf("%.0f", n.sites.q025),', ',sprintf("%.0f", n.sites.q975),']'),
                                                                   pcnt.sites = paste0(sprintf("%.1f", pcnt.sites),' [',sprintf("%.1f", pcnt.sites.q025),', ',sprintf("%.1f", pcnt.sites.q975),']'),
                                                                   g2b = paste0(sprintf("%.1f", g2b),' [',sprintf("%.1f", g2b.q025),', ',sprintf("%.1f", g2b.q975),']'), n.MC = n.MC), 
                                                               by=c('trend.period','vi.name','landcov.name','trend.cat')]

landcov.frac.trnds.smry.fancy.dt$vi.name <- factor(landcov.frac.trnds.smry.fancy.dt$vi.name, levels = c('ndvi','evi2','nirv','kndvi','ensemble'))
landcov.frac.trnds.smry.fancy.dt <- landcov.frac.trnds.smry.fancy.dt[order(trend.period, vi.name, landcov.name, trend.cat)]

# write out
fwrite(landcov.frac.trnds.smry.dt, 'output/lsat_vi_gs_landcov_trends_frac/lsat_vi_gs_boreal_landcov_frac_trends_summary.csv')
fwrite(landcov.frac.trnds.smry.fancy.dt, 'output/lsat_vi_gs_landcov_trends_frac/lsat_vi_gs_boreal_landcov_frac_trends_summary_fancy.csv')


# ECOREGION: FRACTION OF TRENDS IN EACH ECOUNIT (SPATIALIZE) =========================================================================================
str(ecounit.frac.trnds.dt)

ecounit.frac.trnds.ens.smry.dt <- ecounit.frac.trnds.dt[, .(n.sites.q500=quantile(n.sites, 0.500), n.sites.q025=quantile(n.sites,0.025), n.sites.q975=quantile(n.sites,0.975),
                                                        pcnt.sites.q500=quantile(pcnt.sites, 0.500), pcnt.sites.q025=quantile(pcnt.sites,0.025), pcnt.sites.q975=quantile(pcnt.sites,0.975)),
                                                    by = c('trend.period','ecounit','trend.cat')]

ecounit.frac.trnds.ens.smry.long.dt <- melt(ecounit.frac.trnds.ens.smry.dt, id.vars = c('trend.period','trend.cat','ecounit'))
ecounit.frac.trnds.ens.smry.long.dt[, value := round(value)]


 # spatialize trend fractions ------------------------
trend.periods <- c('1985to2019','2000to2019')
trend.cats <- c('browning','greening')
vars <- unique(ecounit.frac.trnds.ens.smry.long.dt$variable)

# i = trend.periods[1]
# j = trend.cats[1]
# k = vars[1]

for (i in trend.periods){
  for (j in trend.cats){
    for(k in vars){
      dt <- ecounit.frac.trnds.ens.smry.long.dt[trend.period == i & trend.cat == j & variable == k]
      pxl.dt <- boreal.pxl.dt[dt, on = 'ecounit']
      pxl.dt <- na.omit(pxl.dt)
      
      trnd.r <- boreal.r
      trnd.r[pxl.dt$cellid] <- round(pxl.dt$value)
      
      outname <- paste0('output/lsat_vi_gs_ecounit_trends_frac/lsat_vi_gs_boreal_ecounit_', j,'_', gsub('\\.','_', k),'_', i,'_300m_laea.tif')
      writeRaster(trnd.r, outname, datatype = 'INT2U', overwrite=T)
      
      rm(trnd.r)
      print(paste('finished gridding ', i,j,k, sep= ' '))
    }
  }
}


# ECOREGION : MEDIAN TREND PER ECOUNIT ================================================================================================
ecounit.median.trnds.ens.smry.dt <- ecounit.median.trnds.dt[, .(total.change.pcnt.med.q500=median(total.change.pcnt.med),
                                                                total.change.pcnt.med.q025=quantile(total.change.pcnt.med,0.025),
                                                                total.change.pcnt.med.q975=quantile(total.change.pcnt.med,0.975)),
                                                        by = c('trend.period','ecounit')]

ecounit.median.trnds.ens.smry.long.dt <- melt(ecounit.median.trnds.ens.smry.dt, id.vars = c('trend.period','ecounit'))
ecounit.median.trnds.ens.smry.long.dt[, value := round(value)]

trend.periods <- c('1985to2019','2000to2019')
vars <- unique(ecounit.median.trnds.ens.smry.long.dt$variable)

# i = trend.periods[1]
# j = vars[1]

for (i in trend.periods){
  for(j in vars){
    dt <- ecounit.median.trnds.ens.smry.long.dt[trend.period == i & variable == j]
    pxl.dt <- boreal.pxl.dt[dt, on = 'ecounit']
    pxl.dt <- na.omit(pxl.dt)
      
    trnd.r <- boreal.r
    trnd.r[pxl.dt$cellid] <- round(pxl.dt$value)
      
    outname <- paste0('output/lsat_vi_gs_ecounit_trends_median/lsat_vi_gs_boreal_ecounit_', gsub('\\.','_', j),'_', i,'_300m_laea.tif')
    writeRaster(trnd.r, outname, datatype = 'INT2S', overwrite=T)
      
    rm(trnd.r)
    print(paste('finished gridding ', i,j, sep= ' '))
  }
}


# CLEAN UP ======================================================================================================================
gc()
removeTmpFiles()
unlink(tmp.dir, recursive = T)
print("All done!!")
# END SCRIPT ====================================================================================================================