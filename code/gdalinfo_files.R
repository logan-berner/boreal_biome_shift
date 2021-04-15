require(gdalUtils)

files <- list.files('/projects/above_gedi/geodata/climate/terra_climate', full.names = T)
files
check <- files[450]
gdalinfo(check, sd = 1)
