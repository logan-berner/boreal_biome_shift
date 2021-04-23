rm(list=ls())
.libPaths(c(.libPaths(), "~/R/", '/home/lb968/R/4.0.2'))
require(data.table)
require(dplyr)
require(randomForest)
require(caret) # confusionMatrix() and train()
require(pdp) # partial dependency plots
require(R.utils)

args <- commandArgs(TRUE)
# i = as.numeric(args[1])
i = 1

setwd('/projects/arctic/users/lberner/boreal_biome_shift/')

# LOAD DATA SETS AND CREATE OUTPUT DIRS ====================================================================================
vi.trnds.dt <- fread(list.files('output/lsat_vi_gs_site_trends/mc_reps/',full.names = T)[i])
site.dt <- fread('output/boreal_sample_site_climate_and_landcover.csv', fill=T)

# create output dirs
mkdirs('output/lsat_vi_gs_site_trends_attribute/')
mkdirs('output/lsat_vi_gs_site_trends_attribute/rf_models_mcreps')
mkdirs('output/lsat_vi_gs_site_trends_attribute/accuracy_mcreps')
mkdirs('output/lsat_vi_gs_site_trends_attribute/conf_mtx_mcreps')
mkdirs('output/lsat_vi_gs_site_trends_attribute/var_imp_mcreps')
mkdirs('output/lsat_vi_gs_site_trends_attribute/partial_depend_mcreps')

# reformat i for sorting
if (i < 10){
  i <- paste0('000',i)
} else if (i < 100){
  i <- paste0('00',i)
} else if (i < 1000){
  i <- paste0('0',i)
}


# COMBINE LANDSAT TREND DATA WITH SITES CONDITIONS ==================================================
vi.rep <- unique(vi.trnds.dt$vi.name)
trend.periods <- c('1985to2019','2000to2019')
j = trend.periods[2]

for (j in trend.periods){
  
  # subset time period
  vi.period.trnds.dt <- vi.trnds.dt[trend.period == j]
  
  # set trend category levels and labels
  vi.period.trnds.dt[, trend.cat := factor(trend.cat, levels = c('browning','no_trend','greening'), labels = c('browning','none','greening'))]
  trend.classes <- c('browning','none','greening')
  
  # add specific columns to site data table
  cols <- c('site','trend.cat')
  vi.period.trnds.dt <- vi.period.trnds.dt[, ..cols]
  site.dt <- vi.period.trnds.dt[site.dt, on = 'site']
  
  # drop sites w/o a trend assessment
  site.dt <- site.dt[is.na(trend.cat)==F]
  
  # drop a few cols 
  site.dt <- site.dt[, c('lat','lon','site','eco.name','eco.num','landcov.code','ecounit') := NULL]
  
  # !!!!!!!!!!!!!! drop columns filled nas !!!!!!!!!!!!!!!!!!!!!
  cols <- colnames(site.dt)[as.vector(which(colSums(is.na(site.dt)) == 0))]
  site.dt <- site.dt[, ..cols]
  
  # drop na's
  site.dt <- na.omit(site.dt)
  
  # balance sample size across trend classes
  trend.cat.min.n <- min(site.dt[, .(n.sites = .N), by = trend.cat]$n.sites)
  site.dt <- site.dt[, .SD[sample(.N, trend.cat.min.n, replace = F)], by = c('trend.cat')]
  
  # !!!!!!!!!!!!! trim dataset for testing !!!!!!!!!!!!!!!!!!!!!!
  site.dt <- site.dt[sample(1:nrow(site.dt), nrow(site.dt)*0.2)]
  dim(site.dt)
  
  # set characters to factors and separate factor and numeric columns
  site.dt <- site.dt %>% mutate_if(is.character,as.factor) %>% as.data.table()
  site.fac.dt <- site.dt[, sapply(site.dt, is.numeric)==F, with=F] 
  site.num.dt <- site.dt[, sapply(site.dt, is.numeric), with=F]
  
  # numeric columns: drop numeric cols that are all zero
  cols <- colnames(site.num.dt)[as.vector(which(colSums(site.num.dt) == 0))]
  site.num.dt <- site.num.dt[, (cols) := NULL]
  
  # numeric columns: screen highly correlated variables if present using a subsample of data
  print('screening highly correlated values...')
  cors <- cor(site.num.dt[sample(1:nrow(site.num.dt), nrow(site.num.dt)*0.1)], use = 'pair', method = 'spearman')
  # cors <- na.omit(cors)
  high.cors <- findCorrelation(cors, cutoff = 0.75)
  site.num.dt <- site.num.dt[, (high.cors) := NULL]
  
  # recombine factor and numeric columns
  site.dt <- cbind(site.fac.dt, site.num.dt)
  
  # check data again
  summary(site.dt)
  dim(site.dt)
  
  # split into training and eval data
  train.indx <- sample(1:nrow(site.dt), size = nrow(site.dt)*0.75)
  site.train.dt <- site.dt[train.indx]
  site.eval.dt <- site.dt[-train.indx]
  
  # fit random forest model
  print('fitting models...')
  control <- trainControl(method="oob", number=20, search="random")
  
  rf.trained <- train(trend.cat~., data=site.train.dt, method="rf", metric="Accuracy", trControl=control, ntree = 500, importance = T)
  
  # derive confusion matrix
  print('starting model evaluation...')
  site.eval.dt$predicted <- predict.train(rf.trained, newdata = site.eval.dt[,-1])
  conf.mtx <- confusionMatrix(reference = site.eval.dt$trend.cat, data = site.eval.dt$predicted, mode='sens_spec')
  conf.mtx.dt <- data.table(conf.mtx$table)
  conf.mtx.dt[, ':='(vi.name = vi.rep, rep = i)]
  accur.dt <- data.table(class = trend.classes, as.data.table(conf.mtx$byClass))
  accur.dt[, OverallAccuracy := conf.mtx$overall[1]]
  accur.dt[, ':='(vi.name = vi.rep, rep = i)]
  
  # variable importance
  rf.best.fit <- rf.trained$finalModel
  var.imp.dt <- cbind(data.table(var = rownames(rf.best.fit$importance)), data.table(rf.best.fit$importance))[order(MeanDecreaseAccuracy, decreasing = T)]
  var.imp.dt[, ':='(vi.name = vi.rep, rep = i)]
  
  # partial dependency of 20 most important variables
  print('computing partial dependencies...')
  
  pd.list <- list()
  cnt = 1
  
  vars <- var.imp.dt$var[1:10]
  
  for (k in vars){
    for (l in trend.classes){
      pd <- data.table(partial(object = rf.trained, pred.var = k, which.class = l, train = site.dt, smooth = T, type = 'classification', prob = T, trim.outliers = T))
      names(pd) <- c('x.value','prob')
      
      # compute ecdf to help determine where caution needed during interpretation
      fun.ecdf <- ecdf(site.dt[[k]]) # returns NAs for categorical variables
      pd[, ecdf := fun.ecdf(pd$x.value)]
      
      # store output
      pd <- cbind(pd, data.table(var = k, class = l, vi.name = vi.rep, rep = i))
      pd.list[[cnt]] <- pd
      cnt = cnt + 1
      print(paste(j,k, sep=' '))
    }
  }
  
  pd.dt <- rbindlist(pd.list)
  
  # write output to disk
  print('writting output...')
  save(rf.best.fit, file = paste0('output/lsat_vi_gs_site_trends_attribute/rf_models_mcreps/vi_trend_rf_classifier_',j,'_mcrep_',i,'.RDATA'))
  fwrite(conf.mtx.dt, paste0('output/lsat_vi_gs_site_trends_attribute/conf_mtx_mcreps/vi_trend_rf_conf_mtx_',j,'_mcrep_',i,'.csv'))
  fwrite(accur.dt, paste0('output/lsat_vi_gs_site_trends_attribute/accuracy_mcreps/vi_trend_rf_accuracy_',j,'_mcrep_',i,'.csv'))
  fwrite(var.imp.dt, paste0('output/lsat_vi_gs_site_trends_attribute/var_imp_mcreps/vi_trend_rf_var_imp_',j,'_mcrep_',i,'.csv'))
  fwrite(pd.dt, paste0('output/lsat_vi_gs_site_trends_attribute/partial_depend_mcreps/vi_trend_rf_partial_dependency_',j,'_mcrep_',i,'.csv'))
  
  print(paste0("finished ", j))
}

print('All done!')
# END SCRIPT ----------------------------------------------------------------------------------------------------------