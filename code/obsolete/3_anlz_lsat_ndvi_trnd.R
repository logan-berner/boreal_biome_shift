#-------------------------------------------------------------------------------------------------------------------
# ANALYZE EFFECTS OF THERMOKARST EXTENT OF LANDSAT NDVI TRENDS
# AUTHOR: LOGAN BERNER, NAU
# DATE: 2019-07-01
#-------------------------------------------------------------------------------------------------------------------
rm(list=ls())
require(data.table)
require(dplyr)
require(lattice)
require(ranger)
require(randomForest)

# setwd('C:/Users/lb968/Google Drive/research/nau/nasa_above/permafrost_ndvi/')
setwd('C:/Users/Logan/Google Drive/research/nau/nasa_above/boreal_ndvi/')

# landsat ndvi trends at each site
site.dt <- fread('output/boreal_site_lsat_ndvi_trends.csv')

# reclassify landsat ndvi trends
site.dt$trend.cat[site.dt$trend.cat == 'greening.sig.p5'] <- 'positive'
site.dt$trend.cat[site.dt$trend.cat == 'greening.sig.p10'] <- 'positive'
site.dt$trend.cat[site.dt$trend.cat == 'browning.sig.p5'] <- 'negative'
site.dt$trend.cat[site.dt$trend.cat == 'browning.sig.p10'] <- 'negative'
site.dt$trend.cat[site.dt$trend.cat == 'insig'] <- 'no trend'

# consolidate Mosaic and Mixed Forest types
# site.dt$landcov.name[site.dt$landcov.name == 'Mosaic'] <- 'MF'
site.dt <- site.dt %>% filter(landcov.name == 'DNF' | landcov.name == 'ENF' | landcov.name == 'DBF' | landcov.name == 'MF') 
# site.dt <- site.dt %>% mutate(landcov.name = factor(landcov.name, levels = rev(c('Mosaic','MF','DBF','DNF','ENF'))))
# site.dt <- site.dt %>% mutate(landcov.name = factor(landcov.name, labels = rev(c('Mosaic','Deciduous Broadleaf','Deciduous Needleleaf','Evergreen Needleleaf'))))
site.dt <- data.table(site.dt)

# select climate-related columns
cols <- colnames(site.dt)[c(13,26,29:52,65:72)]
site.dt <- na.omit(site.dt)
site.gte1985.dt <- site.dt[period == '1985-2017', .SD, .SDcols = cols]
site.gte2000.dt <- site.dt[period == '2000-2017', .SD, .SDcols = cols]

# FIT RF MODEL FOR DATA FROM 1985 - 2007 ==================================================================
site.gte1985.train <- site.gte1985.dt %>% group_by(trend.cat) %>% sample_n(1000) %>% ungroup() # %>% dplyr::select(-c("trend.cat")) 

site.gte1985.rf <- ranger(trend.cat ~ ., data = site.gte1985.dt, num.trees = 500, mtry = 10)
site.gte1985.rf

site.gte1985.rf <- randomForest(trend.cat ~ ., data = site.gte1985.train, importance = T, proximity = T)
site.gte1985.rf
varImpPlot(site.gte1985.rf)


# FIT RF MODEL FOR DATA FROM 1985 - 2007 ==================================================================
site.gte2000.train <- site.gte2000.dt %>% group_by(trend.cat) %>% sample_n(500) %>% ungroup() %>% dplyr::select(-c("trend.cat"))

site.gte2000.rf <- randomForest(slope ~ ., data = site.gte2000.train, importance = T, proximity = TRUE)
site.gte2000.rf
varImpPlot(site.gte2000.rf)
