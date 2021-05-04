rm(list=ls())
.libPaths(c(.libPaths(), "~/R/", '/home/lb968/R/4.0.2'))
require(data.table)
require(dplyr)
require(randomForest)
require(caret) # confusionMatrix() and train()
require(pdp) # partial dependency plots
require(R.utils)

args <- commandArgs(TRUE)
i = as.numeric(args[1])
# i = 1

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
# trend.periods <- c('1985to2019','2000to2019')
trend.periods <- c('2000to2019')
lc.classes <- unique(site.dt$landcov.name)

# j = trend.periods[1]
# k = lc.classes[1]
for (j in trend.periods){
  
  # subset time period
  vi.period.trnds.dt <- vi.trnds.dt[trend.period == j]
  
  # set trend category levels and labels
  vi.period.trnds.dt[, trend.cat := factor(trend.cat, levels = c('browning','no_trend','greening'), labels = c('browning','none','greening'))]
  trend.classes <- c('browning','none','greening')
  
  # drop climate data associated with 1985 - 2019 
  site.dt <- site.dt[, grep(1985, colnames(site.dt)) := NULL]
  
  # add specific columns to site data table
  cols <- c('site','trend.cat')
  vi.period.trnds.dt <- vi.period.trnds.dt[, ..cols]
  site.dt <- vi.period.trnds.dt[site.dt, on = 'site']
  
  # drop sites w/o a trend assessment
  site.dt <- site.dt[is.na(trend.cat)==F]
  
  # drop a few cols 
  site.dt <- site.dt[, c('lat','lon','site','eco.name','eco.num','landcov.code','ecounit') := NULL]
  
  # drop na's
  site.dt <- na.omit(site.dt)
  
  # # !!!!!!!!!!!!! trim dataset for testing !!!!!!!!!!!!!!!!!!!!!!
  # site.dt <- site.dt[sample(1:nrow(site.dt), nrow(site.dt)*0.05)]
  # dim(site.dt)
  
  # set characters to factors and separate factor and numeric columns
  site.dt <- site.dt %>% mutate_if(is.character,as.factor) %>% as.data.table()
  site.fac.dt <- site.dt[, sapply(site.dt, is.numeric)==F, with=F] 
  site.num.dt <- site.dt[, sapply(site.dt, is.numeric), with=F]
  
  # numeric columns: drop numeric cols that have near zero variance
  site.num.dt <- site.num.dt[, (nearZeroVar(site.num.dt)) := NULL]
  
  # numeric columns: screen highly correlated variables if present using a subsample of data
  print('screening highly correlated values...')
  cors.dt <- cor(site.num.dt[sample(1:nrow(site.num.dt), nrow(site.num.dt)*0.1)], use = 'pair', method = 'pearson')
  cors.dt <- data.table(cors.dt)
  high.cors <- findCorrelation(cors.dt, cutoff = 0.75)
  site.num.dt <- site.num.dt[, (high.cors) := NULL]
  
  # recombine factor and numeric columns
  site.dt <- cbind(site.fac.dt, site.num.dt)
  
  # check data again
  summary(site.dt)
  dim(site.dt)
  
  for (k in lc.classes){
    
    # select data for land cover type of interest
    lc.dt <- site.dt[landcov.name == k]
      
    # balance sample size across trend classes
    trend.cat.min.n <- min(lc.dt[, .(n.sites = .N), by = trend.cat]$n.sites)
    lc.dt <- lc.dt[, .SD[sample(.N, trend.cat.min.n, replace = F)], by = c('trend.cat')]
    
    # split into training and eval data
    train.indx <- sample(1:nrow(lc.dt), size = nrow(lc.dt)*0.75)
    lc.train.dt <- lc.dt[train.indx]
    lc.eval.dt <- lc.dt[-train.indx]
    
    # fit random forest model
    print('fitting models...')
    control <- trainControl(method="oob", number=20, search="random")
    rf.trained <- train(trend.cat~., data=lc.train.dt, method="rf", metric="Accuracy", trControl=control, ntree = 500, importance = T)
    
    # derive confusion matrix
    print('starting model evaluation...')
    lc.eval.dt$predicted <- predict.train(rf.trained, newdata = lc.eval.dt[,-1])
    conf.mtx <- confusionMatrix(reference = lc.eval.dt$trend.cat, data = lc.eval.dt$predicted, mode='sens_spec')
    conf.mtx.dt <- data.table(conf.mtx$table)
    conf.mtx.dt[, ':='(landcov.name = k, vi.name = vi.rep, rep = i)]
    
    accur.dt <- data.table(class = trend.classes, as.data.table(conf.mtx$byClass))
    accur.dt[, OverallAccuracy := conf.mtx$overall[1]]
    accur.dt[, ':='(landcov.name = k, vi.name = vi.rep, rep = i)]

    # variable importance
    rf.best.fit <- rf.trained$finalModel
    var.imp.dt <- cbind(data.table(var = rownames(rf.best.fit$importance)), data.table(rf.best.fit$importance))[order(MeanDecreaseAccuracy, decreasing = T)]
    var.imp.dt[, ':='(landcov.name = k, vi.name = vi.rep, rep = i)]
    
    # partial dependency of the 10 most important variables
    print('computing partial dependencies...')
    
    pd.list <- list()
    cnt = 1
    
    vars <- var.imp.dt$var[1:10]
    
    for (l in vars){
      for (m in trend.classes){
        pd <- data.table(partial(object = rf.trained, pred.var = l, which.class = m, train = site.dt, smooth = T, type = 'classification', prob = T, trim.outliers = T))
        names(pd) <- c('x.value','prob')
        
        # compute ecdf to help determine where caution needed during interpretation
        fun.ecdf <- ecdf(site.dt[[l]]) # returns NAs for categorical variables
        pd[, ecdf := fun.ecdf(pd$x.value)]
        
        # store output
        pd <- cbind(pd, data.table(var = l, class = m, landcov.name = k, vi.name = vi.rep, rep = i))
        pd.list[[cnt]] <- pd
        cnt = cnt + 1
        print(paste('finished',j,k,l, sep=' '))
      }
    }
    
    pd.dt <- rbindlist(pd.list)
    
    # write output to disk
    print('writting output...')
    save(rf.best.fit, file = paste0('output/lsat_vi_gs_site_trends_attribute/rf_models_mcreps/vi_trend_rf_classifier_',tolower(k),'_',j,'_mcrep_',i,'.RDATA'))
    fwrite(conf.mtx.dt, paste0('output/lsat_vi_gs_site_trends_attribute/conf_mtx_mcreps/vi_trend_rf_conf_mtx_',tolower(k),'_',j,'_mcrep_',i,'.csv'))
    fwrite(accur.dt, paste0('output/lsat_vi_gs_site_trends_attribute/accuracy_mcreps/vi_trend_rf_accuracy_',tolower(k),'_',j,'_mcrep_',i,'.csv'))
    fwrite(var.imp.dt, paste0('output/lsat_vi_gs_site_trends_attribute/var_imp_mcreps/vi_trend_rf_var_imp_',tolower(k),'_',j,'_mcrep_',i,'.csv'))
    fwrite(pd.dt, paste0('output/lsat_vi_gs_site_trends_attribute/partial_depend_mcreps/vi_trend_rf_partial_dependency_',tolower(k),'_',j,'_mcrep_',i,'.csv'))
    print(paste("finished", j, k, sep = ' '))
  }
}

print('All done!')
# END SCRIPT ----------------------------------------------------------------------------------------------------------