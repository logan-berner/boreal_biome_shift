# This R script takes monthly Terra Climate data for the boreal domain and computes the Summer warmth index as well as SWI norms, anomalies, and trends.
# Author: Logan Berner
# Date: 2021-08-02
#---------------------------------------------------------------------------------------------------------------------
rm(list=ls())
require(R.utils)
require(raster)
require(rgdal)

laea <- CRS("+proj=laea +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

source('~/code/function/fun_stack_trend.R')
setwd('/projects/arctic/users/lberner/boreal_biome_shift/data/gis_data/terra_clim/')

# create output directories
mkdirs('swi_monthly')
mkdirs('swi_annual')
mkdirs('swi_norms')
mkdirs('swi_anoms')
mkdirs('swi_trends')

# SET UP TEMPORARY DIRECTORY FOR RASTER ====================================================================================
tmp.dir <- '/scratch/lb968/Rtmp/swi'
mkdirs(tmp.dir)
rasterOptions(tmpdir = tmp.dir)


# CUSTORM FUNCTIONS ====================================================================================
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


# COMPUTE MONTHLY SUMMER WARMTH INDEX ====================================================================================
tmax.files <- list.files('monthly', pattern = 'tmax', full.names = T)
tmin.files <- list.files('monthly', pattern = 'tmin', full.names = T)
n.time.steps <- length(tmax.files)
swi.outnames <- gsub('tmin','swi', gsub('monthly','swi_monthly', tmin.files))

for (i in 1:n.time.steps){
  tmax.r <- raster(tmax.files[i])
  tmin.r <- raster(tmin.files[i])
  tavg.r <- (tmax.r + tmin.r)/2
  swi.r <- tavg.r
  swi.r[swi.r <= 0] <- 0
  writeRaster(swi.r, swi.outnames[i], overwrite=T, dataType='INT2U')
  print(i/n.time.steps)
}


# COMPUTE ANNUAL SUMMER WARMTH INDEX ====================================================================================
yrs <- 1958:2019
n.yrs <- length(yrs)
swi.monthly.files <- list.files('swi_monthly/', full.names = T)

for (i in 1:n.yrs){
  yr.files <- swi.monthly.files[grep(yrs[i], swi.monthly.files)]
  stk <- stack(yr.files)
  stk.sum <- sum(stk)
  outname <- paste0('swi_annual/terraclim_boreal_swi_',yrs[i],'_Cx100_laea_4km.tif')
  writeRaster(stk.sum, outname, overwrite=T, datatype='INT2U')
  print(i/n.yrs)
}


# LOOP THROUGHT PERIODS ====================================================================================
# periods.df <- data.frame(start.yr = c(1959,1985,2000), end.yr = c(2019, 2019, 2019))
periods.df <- data.frame(start.yr = c(2000), end.yr = c(2019))
n.periods <- nrow(periods.df)

print('starting loop...')

swi.files <- list.files('swi_annual/', full.names = T)

for(j in 1:n.periods){
  in.files <- unique(grep(paste(periods.df$start.yr[j]:periods.df$end.yr[j], collapse ='|'), swi.files, value = T))
  stk <- stack(in.files)
    
  #--------------------------------------------------
  # climate norm
  stk.avg <- mean(stk, na.rm=T)
  stk.avg <- round(stk.avg)
  stk.avg.outfile <- paste('swi_norms/terraclim_boreal_swi_mean_',periods.df$start.yr[j],'to', periods.df$end.yr[j],'_Cx100_laea_4km.tif', sep='')
  writeRaster(stk.avg, stk.avg.outfile, datatype = 'INT2U', overwrite =T)
  print('finished computing avgs')

  #--------------------------------------------------
  # climate variability (Coef of variability)
  stk.sd <- stack_sd(stk/100)*100 # divide and then mulitply by 100 to avoid interger overflow error
  # stk.cv <- stk.sd / stk.avg * 100
  # stk.cv <- round(stk.cv)
  # stk.cv.outfile <- paste('wateryear_cv/terraclim_boreal_', var,'_cv_',periods.df$start.yr[j],'to', periods.df$end.yr[j],'_wy_', var.meta.df$fun, '_pcnt_laea_4km.tif', sep='')
  # writeRaster(stk.cv, stk.cv.outfile, datatype = 'INT1U', overwrite =T)
  # print('finished computing cv')

  #--------------------------------------------------
  # climate extremes
  stk.anom <- stk - stk.avg
  stk.z <- stk.anom / stk.sd
  stk.z.extrm <- max(stk.z)
  
  stk.anom.outfile <- paste('swi_anoms/terraclim_boreal_swi_anom_',periods.df$start.yr[j],'to', periods.df$end.yr[j],'_maxzscore_laea_4km.tif', sep='')
  writeRaster(stk.z.extrm, stk.anom.outfile, overwrite = T)
  print('finished computing anoms')
  
  #--------------------------------------------------
  # climate trend
  stk.trnd <- stack.trend(stk)

  stk.trnd.outfile <- paste('swi_trends/terraclim_boreal_swi_trend_',periods.df$start.yr[j],'to', periods.df$end.yr[j],'_Cx100_laea_4km.tif', sep='')
  writeRaster(stk.trnd, stk.trnd.outfile, overwrite =T)
  print('finished computing trends')
  print(paste0('finished: ', j))
}

# delete tmp directory
unlink(tmp.dir, recursive = T)
print('done')


# END SCRIPT ====================================================================================