# This R script takes seasonal Terra Climate data for the boreal domain and computes norms, anomalies, and trends.
# Each job array is a variable x season (8 x 4 = 32 jobs in array)
# Author: Logan Berner
# Date: 2021-01-15
#---------------------------------------------------------------------------------------------------------------------
rm(list=ls())
.libPaths(c(.libPaths(), "~/R/", "/home/lb968/R/4.0.2/"))
args <- commandArgs(TRUE)
i = as.numeric(args[1])
# i=23

require(R.utils)
require(raster)
require(rgdal)

wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
laea <- CRS("+proj=laea +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

source('~/code/function/fun_stack_trend.R')
setwd('/projects/arctic/users/lberner/boreal_biome_shift/data/gis_data/terra_clim/')

# create output directories
mkdirs('seasonal_trends')
mkdirs('seasonal_norms')
mkdirs('seasonal_anoms')
mkdirs('seasonal_cv')

# SET UP TEMPORARY DIRECTORY FOR RASTER =====================================================================================
tmp.dir <- paste('/scratch/lb968/Rtmp/tclim_seas_smry_',i, '/', sep='')
mkdirs(tmp.dir)
rasterOptions(tmpdir = tmp.dir)


# CUSTORM FUNCTIONS ===============================================================================================
# http://www.samuelbosch.com/2018/02/speeding-up-mean-and-standard-deviation.html
stack_sd <- function(x) {
  s0 <- nlayers(x)
  s1 <- raster(x, layer=1)
  s2 <- s1^2
  for(ri in 2:s0) {
    r <- raster(x, layer=ri)
    s1 <- s1 + r
    s2 <- s2 + r^2
  }
  stk.sd=sqrt((s0 * s2 - s1 * s1)/(s0 * (s0 - 1)))
  stk.sd
}


# METADATA ABOUT VARIABLES ===============================================================================================
all.files <- list.files('seasonal', full.names = T)
all.files.short <- list.files('seasonal', full.names = F)

meta.df <- data.frame(var=c('def','pdsi','ppt','soil','swe','tmax','tmin','vpd'),
                      units = c('mm','unitlessx100','mm','mm','mm','Cx100','Cx100','kPax100'),
                      bitdepth = c('INT2U','INT2S','INT2U','INT2U','INT2U','INT2S','INT2S','INT2U'),
                      fun = c('sum','mea','sum','mean','mean','max','mean','max'))

season.df <- data.frame(var = rep(meta.df$var, each = 4), 
                        season = rep(c('djf','mam','jja','son'), length(meta.df$var))) 
                        
var <- as.character(season.df$var[i])
var.meta.df <- meta.df[which(meta.df$var %in% var),]

season  <- as.character(season.df$season[i])

# get files for specific variable and season
var.seas.files <- grep(var, all.files[grep(season, all.files.short)], value = T)


# LOOP THROUGHT PERIODS ===============================================================================================
# periods.df <- data.frame(start.yr = c(1959,1985,2000), end.yr = c(2019, 2019, 2019))
periods.df <- data.frame(start.yr = c(2000), end.yr = c(2019))
n.periods <- nrow(periods.df)

print('starting loop...')

for(j in 1:n.periods){
    in.files <- unique(grep(paste(periods.df$start.yr[j]:periods.df$end.yr[j], collapse ='|'), var.seas.files, value = T))
    stk <- stack(in.files)
    
    #--------------------------------------------------
    # climate norm
    stk.avg <- mean(stk, na.rm=T)
    stk.avg <- round(stk.avg)
    stk.avg.outfile <- paste('seasonal_norms/terraclim_boreal_', var,'_mean_',periods.df$start.yr[j],'to', periods.df$end.yr[j],'_',season, '_', var.meta.df$fun, '_', var.meta.df$units, '_laea_4km.tif', sep='')
    writeRaster(stk.avg, stk.avg.outfile, datatype = as.character(var.meta.df$bitdepth), overwrite =T)

    # #--------------------------------------------------
    # # climate variability (Coef of variability)
    stk.sd <- stack_sd(stk/100)*100 # divide and then multiply by 100 to avoid 'interget overflow' error from too large of values
    stk.cv <- stk.sd / stk.avg * 100
    stk.cv <- round(stk.cv)
    stk.cv.outfile <- paste('seasonal_cv/terraclim_boreal_', var,'_cv_',periods.df$start.yr[j],'to', periods.df$end.yr[j],'_',season, '_', var.meta.df$fun, '_pcnt_laea_4km.tif', sep='')
    writeRaster(stk.cv, stk.cv.outfile, datatype = 'INT1U', overwrite =T)

    #--------------------------------------------------
    # climate extremes
    stk.anom <- stk - stk.avg
    stk.z <- stk.anom / stk.sd
    stk.sd.eq0 <- stk.sd == 0 # identify pixels w/ no variability through time (e.g., DEF never above zero in winter)
    stk.z[stk.sd.eq0 == 1] <- 0 # set non-varying pixels to have a z-score of 0

    if (var %in% c('pdsi','ppt','soil','swe','tmin')){
      stk.z.extrm <- abs(min(stk.z))
    } else if (var %in% c('def','tmax','vpd')){
      stk.z.extrm <- max(stk.z)
    } else (print('not sure what to do with this variable...'))

    stk.anom.outfile <- paste('seasonal_anoms/terraclim_boreal_', var,'_anom_',periods.df$start.yr[j],'to', periods.df$end.yr[j],'_',season, '_', var.meta.df$fun, '_maxzscore_laea_4km.tif', sep='')
    writeRaster(stk.z.extrm, stk.anom.outfile, overwrite = T)
    
    #--------------------------------------------------
    # climate trend
    stk.trnd <- stack.trend(stk)

    ## deal with non-NA pixels that show no variability through time
    stk.trnd.na <- is.na(stk.trnd[[1]]) # id pixels in trend map that are NA
    stk.avg.notna <- is.na(stk.avg) == F # id pixels in var stack that are valid
    stk.trnd.missing <- stk.avg.notna == 1 & stk.trnd.na == 1 # id valied pixels that are missing trend data

    stk.trnd[[1]][stk.trnd.missing == 1] <- 0 # trend
    stk.trnd[[2]][stk.trnd.missing == 1] <- 0 # total.change
    stk.trnd[[3]][stk.trnd.missing == 1] <- 0 # intercept
    stk.trnd[[4]][stk.trnd.missing == 1] <- 1 # pval
    stk.trnd[[5]][stk.trnd.missing == 1] <- 0 # tau
    stk.trnd[[6]][stk.trnd.missing == 1] <- 0 # trend.pcnt

    stk.trnd.outfile <- paste('seasonal_trends/terraclim_boreal_', var,'_trend_',periods.df$start.yr[j],'to', periods.df$end.yr[j],'_',season, '_', var.meta.df$fun, '_', var.meta.df$units, '_laea_4km.tif', sep='')
    writeRaster(stk.trnd, stk.trnd.outfile, overwrite =T)
    
    print(paste0('finished: ', j))
}

# delete tmp directory
unlink(tmp.dir, recursive = T)
print('done!')

# END SCRIPT ===============================================================================================