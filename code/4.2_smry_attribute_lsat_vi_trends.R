# This R script summarized random forest models used to attribute Landsat VI trends and creates output figure
rm(list=ls())
.libPaths(c(.libPaths(), "~/R/", '/home/lb968/R/3.5'))
require(data.table)
require(dplyr)
require(ggplot2)
require(ggpubr)
require(R.utils)
require(zoo) # rollapply
setwd('/projects/arctic/users/lberner/boreal_biome_shift/')

# LOAD DATA SETS ====================================================================================
accur.dt <- do.call("rbind", lapply(list.files('output/lsat_vi_gs_site_trends_attribute/accuracy_mcreps/', full.names = T), fread))
conf.mtx.dt <- do.call("rbind", lapply(list.files('output/lsat_vi_gs_site_trends_attribute/conf_mtx_mcreps/', full.names = T), fread))
var.imp.dt <- do.call("rbind", lapply(list.files('output/lsat_vi_gs_site_trends_attribute/var_imp_mcreps/', full.names = T), fread))
pd.dt <- do.call("rbind", lapply(list.files('output/lsat_vi_gs_site_trends_attribute/partial_depend_mcreps/', full.names = T), fread))


# SUMMARIZE ACROSS MONTE CARLO SIMULATIONS ====================================================================================

# classification accuracy
setnames(accur.dt, 'Balanced Accuracy', 'BalancedAccuracy')
cols <- names(accur.dt)[c(1,2,11,12)]
accur.q500.mtx <- as.matrix(accur.dt[, lapply(.SD, function(x){round(median(x),2)}), .SDcols = cols])
accur.q025.mtx <- as.matrix(accur.dt[, lapply(.SD, function(x){round(quantile(x,0.025),2)}), .SDcols = cols])
accur.q975.mtx <- as.matrix(accur.dt[, lapply(.SD, function(x){round(quantile(x,0.975),2)}), .SDcols = cols])
accur.smry.dt <- data.table(matrix(paste0(accur.q500.mtx, ' [', accur.q025.mtx, ', ', accur.q975.mtx,']'), ncol = 4))
colnames(accur.smry.dt) <- capitalize(colnames(accur.q500.mtx))
accur.smry.dt
fwrite(accur.smry.dt, 'output/lsat_vi_gs_site_trends_attribute/lsat_vi_gs_site_trend_rf_classification_accuracy_summary.csv')

# confusion matrix
conf.mtx.dt[, Prediction := factor(Prediction, levels = c('browning','greening'))]
conf.mtx.dt[, Reference := factor(Reference, levels = c('browning','greening'))]
conf.mtx.smry.dt <- conf.mtx.dt[, .(N=round(median(N)), N.q025=round(quantile(N,0.025)), N.q975=round(quantile(N,0.975))), by = c('Reference','Prediction')]
conf.mtx.smry.fncy.dt <- conf.mtx.smry.dt[,1:3]
conf.mtx.smry.fncy.dt$N <- paste0(conf.mtx.smry.dt$N, ' [', conf.mtx.smry.dt$N.q025, ', ', conf.mtx.smry.dt$N.q975, ']')
conf.mtx.smry.fncy.dt <- dcast(conf.mtx.smry.fncy.dt, Reference ~ Prediction, value.var = 'N')
conf.mtx.smry.fncy.dt
fwrite(conf.mtx.smry.fncy.dt, 'output/lsat_vi_gs_site_trends_attribute/lsat_vi_gs_site_trend_rf_confusion_matrix_summary.csv')

# variable importance
var.imp.smry.dt <- var.imp.dt[, .(imp.q500=median(MeanDecreaseAccuracy), 
                                  imp.q025=quantile(MeanDecreaseAccuracy,0.025), 
                                  imp.q975=quantile(MeanDecreaseAccuracy,0.975), 
                                  n.models = .N), 
                              by = c('var')]

var.imp.smry.dt <- setorder(var.imp.smry.dt, -imp.q500)
top.vars.dt <- var.imp.smry.dt[, head(.SD, 6)]
fwrite(var.imp.smry.dt, 'output/lsat_vi_gs_site_trends_attribute/lsat_vi_gs_site_trend_rf_variable_importance_summary.csv')

# partial dependency
vars <- unique(pd.dt$var)
pd.dt <- pd.dt[, x.value := as.numeric(x.value)]
pd.interp.list <- list()

cnt = 1
for (j in unique(pd.dt$var)){
  var.dt <- pd.dt[var == j]
  x.min <- min(var.dt$x.value)
  x.max <- max(var.dt$x.value)
  x.rng <- x.max - x.min
  x.seq <- seq(x.min, x.max, by = x.rng / 50)
  for (k in unique(var.dt$class)){
    var.class.dt <- var.dt[class == k]
    for (l in unique(var.class.dt$rep)){
      dt <- var.class.dt[rep == l]
      if(sd(dt$x.value)==0){
        next()
      } else {
        prob.fun <- approxfun(dt$x.value, dt$prob)
        ecdf.fun <- approxfun(dt$x.value, dt$ecdf)
        pd.interp.list[[cnt]] <- data.table(var = j, class = k, rep = l, x.value = x.seq, prob = prob.fun(x.seq), ecdf = ecdf.fun(x.seq))
        print(cnt)
        cnt = cnt + 1
      }
    }
  }
}

  
pd.dt <- rbindlist(pd.interp.list)

pd.smry.dt <- pd.dt[, .(prob=median(prob, na.rm=T), prob.q025=quantile(prob,0.025, na.rm=T), prob.q975=quantile(prob,0.975, na.rm=T),
                                ecdf = median(ecdf, na.rm=T)), by = c('var','class','x.value')]

pd.smry.dt[var == 'vpd.change.kPa', x.value := x.value * 1000]

# CREATE FIGURES ====================================================================================


# VARIABLE IMPORTANCE -------------------------------------------
varimp.labs <- c('swi.avg.degC' = expression('Mean Summer Warmth Index ('*degree*'C)'),
                 'magt.degC' = expression('Mean Annual Soil Temperature ('*degree*'C)'),
                 'elev.m' = expression('Elevation (m)'),
                 'soil.n.00to05cm.gkg' = expression('Soil Nitrogen (g kg'^-1*')'),
                 'swi.change.degC' = expression(Delta~'Summer Warmth Index ('*degree*'C)'),
                 'vpd.change.kPa' = expression(Delta~'Summer VPD (Pa)'))

var.imp.smry.top6.dt <- var.imp.smry.dt[1:6,]
var.imp.smry.top6.dt[, var := factor(var, levels = rev(var))]

var.imp.fig <- ggplot(var.imp.smry.top6.dt, aes(x = var, y = imp.q500)) +
  geom_errorbar(aes(ymin = imp.q025, ymax = imp.q975), position = position_dodge(width = 0.5), width = 0.2, color = 'gray30', alpha = 0.5) +
  geom_point(position = position_dodge(width = 0.5), shape = 21, size = 4, fill = 'black', color = 'black', na.rm=T) + 
  coord_flip() + labs(x = 'Predictor variable', y = 'Mean decrease in accuracy') + 
  scale_x_discrete(labels = varimp.labs) + 
  theme_bw() + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
                     axis.title = element_text(size = 12))
var.imp.fig


jpeg('figures/Landsat_VI_trend_classification_varimp.jpg', 6, 5, res = 400, units = 'in')
var.imp.fig
dev.off()

# PARTIAL DEPENDENCIES -----------------------------------------
trend.cols <- c('saddlebrown','springgreen3')

pd.labs <- c(paste0("Mean~Summer~Warmth~Index~(", "degree*", "C)"),
             paste0("Mean~Annual~Soil~Temperature~(", "degree*", "C)"),
             paste0('Elevation~(m)'),
             paste0("Soil~Nitrogen~(g~kg", "^-1)"),
             paste0("Delta~", "Summer~Warmth~Index~(", "degree*", 'C)'),
             paste0("Delta~", "Summer~VPD~(Pa)"))

pd.smry.top.dt <- pd.smry.dt[var %in% top.vars.dt$var]
pd.smry.top.dt[, var := factor(var, levels = top.vars.dt$var, labels = pd.labs)]
pd.smry.top.dt[, class := factor(class, levels = c('browning','greening'))]
pd.smry.top.dt <- pd.smry.top.dt[ecdf > 0.025 & ecdf < 0.975]

pd.fig <- ggplot(pd.smry.top.dt, aes(x.value, prob, group = class)) + 
  geom_ribbon(aes(x=x.value, ymin=prob.q025, ymax=prob.q975, fill=class), alpha = 0.50) + 
  geom_line(aes(color=class), size = 1.5) + 
  scale_fill_manual(values = trend.cols) + scale_color_manual(values = trend.cols) + 
  facet_wrap(var ~ ., scales = 'free', labeller = label_parsed) + 
  xlab('Value of predictor variable') + ylab('Classification probability') + 
  labs(color='Trend: ', fill = 'Trend: ') + 
  theme_bw() + theme(legend.position = 'bottom', legend.text=element_text(size=10), legend.title=element_text(size=12), legend.spacing.x = unit(1.0, "cm")) + 
  theme(strip.text = element_text(size = 8.5), axis.text = element_text(size = 10), axis.title = element_text(size = 12))

pd.fig

jpeg('figures/Landsat_VI_trend_classification_partial_depend.jpg', 10, 8, res = 400, units = 'in')
pd.fig
dev.off()


# combined var importance and partial dependency
combo.fig <- ggarrange(var.imp.fig, pd.fig, labels = c("a", "b"), label.x = 0.01, label.y = 1.03, ncol = 1, nrow = 2, heights=c(1,2))
combo.fig

#pdf('figures/paper/Landsat_VI_trend_classification_varImp_parDepend.pdf',7, 7)
jpeg('figures/paper/Landsat_NDVI_trend_classification_varImp_parDepend.jpg', 8.5, 8, res = 400, units = 'in')
combo.fig
dev.off()


#--------------------------------------------------------------------------------------------------
ggplot(accur.dt, aes(x = vi.name, y = OverallAccuracy)) + geom_boxplot()