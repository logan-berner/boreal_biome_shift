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
require(sf)

tmp.dir <- '/scratch/lb968/lsat_vi_trend_smry/'
tempfile(tmpdir=tmp.dir)
rasterOptions(tmpdir=tmp.dir)

setwd('/projects/arctic/users/lberner/boreal_biome_shift/')

# READ IN FILES ================================================================================================================
site.dt <- fread('output/boreal_sample_site_climate_and_landcover.csv')
site.trnds.dt <- do.call("rbind", lapply(list.files('output/lsat_vi_gs_site_trends/mc_reps/', full.names = T), fread))
landcov.frac.trnds.dt <- do.call("rbind", lapply(list.files('output/lsat_vi_gs_landcov_trends_frac/mc_reps_tabular/', full.names = T), fread))
landcov.trnds.dt <- do.call("rbind", lapply(list.files('output/lsat_vi_gs_landcov_trends/mc_reps_tabular/', full.names = T), fread))
ecounit.frac.trnds.dt <- do.call("rbind", lapply(list.files('output/lsat_vi_gs_ecounit_trends_frac/mc_reps_tabular/', full.names = T), fread))
ecounit.median.trnds.dt <- do.call("rbind", lapply(list.files('output/lsat_vi_gs_ecounit_trends_median/mc_reps_tabular/', full.names = T), fread))
biome.frac.trnds.dt <- do.call("rbind", lapply(list.files('output/lsat_vi_gs_biome_trends_frac/mc_reps_tabular/', full.names = T), fread))

ecounit.r <- raster('data/gis_data/ecological_land_unit_boreal_aoi_300m_laea.tif')     
boreal.r <- raster('data/gis_data/wwf_boreal_biome_laea_300m.tif')

boreal.pxl.dt <- data.table(cellid = 1:ncell(boreal.r), ecounit = values(ecounit.r))
boreal.pxl.dt <- na.omit(boreal.pxl.dt)

# MEDIAN PERCENT CHANGE IN VEGETATION GREENNESS FOR EACH SAMPLE SITE DURING BOTH PERIODS ====================================================
site.trnds.dt[pval <= 0.10, sig := 0]
site.trnds.dt[pval > 0.10, sig := 1]

site.median.trnd.dt <- site.trnds.dt[, .(tau.q500 = quantile(tau, 0.50), tau.q025 = quantile(tau, 0.025), tau.q975 = quantile(tau, 0.975),
                                         total.change.pcnt.q500 = quantile(total.change.pcnt, 0.50), total.change.pcnt.q025 = quantile(total.change.pcnt, 0.025), total.change.pcnt.q975 = quantile(total.change.pcnt, 0.975),  
                                         sig.p10 = sum(sig)/1000), by = c('site','trend.period')]
site.median.trnd.dt <- site.dt[site.median.trnd.dt, on = 'site']

fwrite(site.median.trnd.dt, 'output/lsat_vi_gs_site_trends/lsat_vi_gs_boreal_site_median_trends.csv')

# spatialize and write out
site.sf <- st_as_sf(site.median.trnd.dt, coords = c("lon", "lat"), crs = 4326, agr = "constant")
site.1985.sf <- site.sf %>% filter(trend.period == '1985to2019')
site.2000.sf <- site.sf %>% filter(trend.period == '2000to2019')
st_write(site.1985.sf, dsn = 'output/lsat_vi_gs_site_trends/lsat_vi_gs_boreal_site_median_trends_1985to2019.shp', append = F)
st_write(site.2000.sf, dsn = 'output/lsat_vi_gs_site_trends/lsat_vi_gs_boreal_site_median_trends_2000to2019.shp', append = F)


# MEDIAN PERCENT INCREASES IN VEGETATION GREENNESS ACROSS ALL SAMPLE SITES ========================================================================
fivenum(site.trnds.dt$total.change.pcnt)
biome.median.pcnt.change.reps.dt <- site.trnds.dt[, .(total.change.pcnt=median(total.change.pcnt, na.rm = T)), by = c('rep','trend.period')]
biome.median.pcnt.change.reps.dt[, .(vi.change.pcnt.q500=median(total.change.pcnt, na.rm = T), 
     vi.change.pcnt.q025=quantile(total.change.pcnt,0.025, na.rm = T), 
     vi.change.pcnt.q975=quantile(total.change.pcnt,0.975, na.rm = T)),
  by = c('trend.period')]


# # SITES TRENDS BY LATITUDE ===============================================================================================================================
# avg.lat.of.trend.cats.reps.dt <- site.trnds.dt[, .(lat.avg = mean(latitude, na.rm=T)), by = c('rep','trend.cat','trend.period')]
# avg.lat.of.trend.cats.reps.dt[, .(lat.q500=median(lat.avg, na.rm = T), lat.q025=quantile(lat.avg,0.025, na.rm = T), lat.q975=quantile(lat.avg,0.975, na.rm = T)),
#   by = c('trend.period', 'trend.cat')]


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


# LANDCOVER: FRACTION OF ALL TRENDS OCCURING IN EACH LANDCOVER CATEGORY ===========================================================================
landcov.trnds.dt <- landcov.trnds.dt[trend.cat != '']

# summary across ensemble
landcov.trnds.ens.smry.dt <- landcov.trnds.dt[, .(n.sites.landcov.q500=quantile(n.sites.landcov, 0.500), n.sites.landcov.q025=quantile(n.sites.landcov,0.025), n.sites.landcov.q975=quantile(n.sites.landcov,0.975),
                                                  n.sites.landcov.trnd.q500=quantile(n.sites.landcov.trnd, 0.500), n.sites.landcov.trnd.q025=quantile(n.sites.landcov.trnd,0.025), n.sites.landcov.trnd.q975=quantile(n.sites.landcov.trnd,0.975),
                                                  pcnt.sites.q500=quantile(pcnt.sites, 0.500), pcnt.sites.q025=quantile(pcnt.sites,0.025), pcnt.sites.q975=quantile(pcnt.sites,0.975)), 
                                              by = c('trend.period','landcov.name','trend.cat')]

# round numeric cols
landcov.trnds.ens.smry.dt <- cbind(landcov.trnds.ens.smry.dt[, 1:3], round(landcov.trnds.ens.smry.dt[,-c(1:3)],1))

# fancy table
landcov.trnds.smry.fancy.dt <- landcov.trnds.ens.smry.dt[, .(n.sites.landcov = paste0(sprintf("%.0f", n.sites.landcov.q500),' [',sprintf("%.0f", n.sites.landcov.q025),', ',sprintf("%.0f", n.sites.landcov.q975),']'),
                                                             pcnt.sites = paste0(sprintf("%.1f", pcnt.sites.q500),' [',sprintf("%.1f", pcnt.sites.q025),', ',sprintf("%.1f", pcnt.sites.q975),']')),
                                                         by=c('trend.period','landcov.name','trend.cat')]

landcov.trnds.smry.fancy.dt <- landcov.trnds.smry.fancy.dt[order(trend.period, landcov.name, trend.cat)]

# write out
fwrite(landcov.trnds.ens.smry.dt, 'output/lsat_vi_gs_landcov_trends/lsat_vi_gs_boreal_landcov_trends_summary.csv')
fwrite(landcov.trnds.smry.fancy.dt, 'output/lsat_vi_gs_landcov_trends/lsat_vi_gs_boreal_landcov_trends_summary_fancy.csv')


# NUMBER OF SAMPLE SITES IN EACH 30 X 30 GRID CELL DURING BOTH TIME PERIODS ===================================================================== 

# resample boreal to 30 x 30 km grid
aoi.sum.30km.r <- raster::aggregate(boreal.r, fact = 100, fun = sum)
aoi.cellid.30km.r <- aoi.sum.30km.r
aoi.cellid.30km.r[] <- 1:ncell(aoi.cellid.30km.r)

# determine which sites were used for trend analysis in each time period
site.trnds.period.dt <- site.trnds.dt[, .(latitude = first(latitude), longitude = first(longitude)), by = c('site','trend.period')]

# spatalize sample sites use in trend analysis
site.sf <- st_as_sf(site.trnds.period.dt, coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>% st_transform(crs = st_crs(boreal.r))

# extract grid cell id for each sample site 
site.trnds.period.dt$cell.id <- raster::extract(aoi.cellid.30km.r, site.sf)

# summarize number of sample sites per grid cell
n.sites.cell.dt <- site.trnds.period.dt[, .(n.sites = .N), by = c('cell.id','trend.period')]

# spatalize number of sample sites per grid cell
blank.30km.r <- aoi.cellid.30km.r
blank.30km.r[] <- NA

n.sites.gte1985.30km.r <- blank.30km.r
n.sites.gte1985.30km.r[n.sites.cell.dt[trend.period == '1985to2019']$cell.id] <- n.sites.cell.dt[trend.period == '1985to2019']$n.sites
plot(n.sites.gte1985.30km.r)
writeRaster(n.sites.gte1985.30km.r, 'output/lsat_vi_gs_site_trends/lsat_vi_gs_boreal_site_count_per_gridcell_1985to2019_30km_laea.tif' , datatype = 'INT2U', overwrite=T)

n.sites.gte2000.30km.r <- blank.30km.r
n.sites.gte2000.30km.r[n.sites.cell.dt[trend.period == '2000to2019']$cell.id] <- n.sites.cell.dt[trend.period == '2000to2019']$n.sites
plot(n.sites.gte2000.30km.r)
writeRaster(n.sites.gte2000.30km.r, 'output/lsat_vi_gs_site_trends/lsat_vi_gs_boreal_site_count_per_gridcell_2000to2019_30km_laea.tif' , datatype = 'INT2U', overwrite=T)

# resample to 300 m (use these as data availability masks in the next steps)
n.sites.gte1985.rsmpl.300m.r <- resample(n.sites.gte1985.30km.r, boreal.r, method = 'ngb')
n.sites.gte2000.rsmpl.300m.r <- resample(n.sites.gte2000.30km.r, boreal.r, method = 'ngb')


# ECOREGION: FRACTION OF TRENDS IN EACH ECOUNIT (SPATIALIZE) =========================================================================================
blank.300m.r <- boreal.r
blank.300m.r[] <- NA

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
# j = trend.cats[2]
# k = vars[4]

for (i in trend.periods){
  for (j in trend.cats){
    for(k in vars){
      dt <- ecounit.frac.trnds.ens.smry.long.dt[trend.period == i & trend.cat == j & variable == k]
      pxl.dt <- boreal.pxl.dt[dt, on = 'ecounit']
      pxl.dt <- na.omit(pxl.dt)
      
      trnd.r <- blank.300m.r
      trnd.r[pxl.dt$cellid] <- round(pxl.dt$value)
      
      # mask out 300 m ecounit grid cells if the overlaying 30 km grid cell doesn't contain any sample sites
      if (i == '1985to2019'){
        trnd.r <- mask(trnd.r, n.sites.gte1985.rsmpl.300m.r)
      } else {
        trnd.r <- mask(trnd.r, n.sites.gte2000.rsmpl.300m.r)
      }
      
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

# i = trend.periods[2]
# j = vars[3]

for (i in trend.periods){
  for(j in vars){
    dt <- ecounit.median.trnds.ens.smry.long.dt[trend.period == i & variable == j]
    pxl.dt <- boreal.pxl.dt[dt, on = 'ecounit']
    pxl.dt <- na.omit(pxl.dt)
      
    trnd.r <- blank.300m.r
    trnd.r[pxl.dt$cellid] <- round(pxl.dt$value)
    
    # mask out 300 m ecounit grid cells if the overlaying 30 km grid cell doesn't contain any sample sites
    if (i == '1985to2019'){
      trnd.r <- mask(trnd.r, n.sites.gte1985.rsmpl.300m.r)
    } else {
      trnd.r <- mask(trnd.r, n.sites.gte2000.rsmpl.300m.r)
    }
      
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