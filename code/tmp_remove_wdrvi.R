# Decide to remove WDRVI as an index in the analysis and instead focus on NDVI, EVI2, NIRv, and kNDVI

rm(list=ls())
setwd('/projects/arctic/users/lberner/boreal_biome_shift/')
require(data.table)

ts.files <- list.files('output/lsat_vi_gs_site_timeseries/', full.names = T)
xcal.files <- list.files('output/xcal/', full.names = T)

# identify and delete files associated with WDRVI ----------------------------------------
wdrvi.reps <- grep('wdrvi', xcal.files, value = T)
wdrvi.reps <- substr(wdrvi.reps, 11,14)
wdrvi.reps <- unique(wdrvi.reps)

wdrvi.ts.files <- unique(grep(paste(wdrvi.reps,collapse="|"), ts.files, value=TRUE))
unlink(wdrvi.ts.files)

wdrvi.xcal.files <- unique(grep(paste(wdrvi.reps,collapse="|"), xcal.files, value=TRUE))
unlink(wdrvi.xcal.files)


# sequentially rename remaining files ---------------------------------------------------
ts.files <- list.files('output/lsat_vi_gs_site_timeseries/', full.names = T)
length(ts.files)
done.rep <- substr(ts.files, nchar(ts.files)-7, nchar(ts.files)-4)

# time series files
ids.dt <- data.table(old.id = done.rep, new.id = 1:length(done.rep))
ids.dt[, new.id.pad := character()]
ids.dt[new.id < 1000, new.id.pad := paste0('0', new.id)]
ids.dt[new.id < 100, new.id.pad := paste0('00', new.id)]
ids.dt[new.id < 10, new.id.pad := paste0('000', new.id)]

ts.new.file.names <- paste0(substr(ts.files, 1, nchar(ts.files)-8), ids.dt$new.id.pad)
file.rename(ts.files, ts.new.file.names)


# xcal files
xcal.files <- list.files('output/xcal/', full.names = T)
length(xcal.files)

xcal.all.reps <- substr(xcal.files, 23, 27)
xcal.all.reps <- gsub('_','',xcal.all.reps)
xcal.all.reps <- sort(xcal.all.reps)
length(unique(xcal.all.reps))

# xcal.id.dt <- rbind(ids.dt, ids.dt, ids.dt, ids.dt, ids.dt, ids.dt)
# setorder(xcal.id.dt, old.id)

for (i in 2:length(xcal.all.reps)){
  rep.old.files <- grep(xcal.all.reps[i], xcal.files, value = T)
  rep.new.files <- gsub(xcal.all.reps[i], ids.dt$new.id.pad[i], rep.old.files)
  file.rename(rep.old.files, rep.new.files)
}

  xcal.id.dt$old.file.names <- grep(paste(unique(xcal.id.dt$old.id), collapse="|"), xcal.files, value=TRUE)
  
  xcal.files.dt <- data.table(old.name = xcal.files)
  
  xcal.id.dt[, old.name := xcal.files]
  
  
  xcal.jpg.files <- list.files('output/xcal/', pattern = 'jpg', full.names = T)
  length(xcal.jpg.files)
  
  xcal.rdata.files <- list.files('output/xcal/', pattern = 'RData', full.names = T)
  length(xcal.rdata.files)
  
  xcal.csv.files <- list.files('output/xcal/', pattern = 'csv', full.names = T)
  length(xcal.csv.files)
}


incomplete.reps <- xcal.all.reps[xcal.all.reps %in% done.rep == F]
incomplete.xcal.files <- grep(paste(unique(incomplete.reps), collapse="|"), xcal.files, value=TRUE)
unlink(incomplete.xcal.files)
