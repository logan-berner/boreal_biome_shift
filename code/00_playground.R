
# compute average SWI of each land cover type (used to order these later on) and add land cover colors
setnames(lc.col.df, 'lc.class', 'landcov.name')

swi.biome.avg.dt <- site.dt[, .(landcov.name = 'Biome', swi.avg = mean(swi.avg.degC))]
swi.lc.avg.dt <- site.dt[, .(swi.avg = mean(swi.avg.degC)), by = 'landcov.name']
setorder(swi.lc.avg.dt, swi.avg)
swi.lc.avg.dt <- rbind(swi.lc.avg.dt, swi.biome.avg.dt)

lc.col.df <- lc.col.df[swi.lc.avg.dt, on = 'landcov.name']
lc.col.df[landcov.name == 'Biome', col := 'black']


# permutation tests for differences in SWI between G/B for each LC class -----------------------------------------------------------
test.dt <- site.trnds.dt[, c('site','trend.cat','rep','landcov.name','swi'), with = T]
test.dt <- test.dt[trend.cat != 'none']

biome.test.avg.dt <- test.dt[, .(landcov.name = 'Biome', swi.avg = mean(swi)), by = c('rep','trend.cat')]
test.avg.dt <- test.dt[, .(swi.avg = mean(swi)), by = c('rep','landcov.name','trend.cat')]
test.avg.dt <- rbind(biome.test.avg.dt, test.avg.dt)

setorder(test.avg.dt, landcov.name, rep, trend.cat)
test.avg.dt

test.avg.dt <- test.avg.dt[,.SD[sample(.N)],by=c('landcov.name','trend.cat')] # shuffle
test.avg.dt <- test.avg.dt[, shuffle := 1]
test.avg.dt[, shuffle := cumsum(shuffle), by = c('landcov.name','trend.cat')]
test.avg.wide.dt <- dcast(test.avg.dt, shuffle + landcov.name ~ trend.cat, value.var = 'swi.avg')

test.avg.wide.dt[greening > browning, g.gt.b := 1]
test.avg.wide.dt[greening < browning, g.gt.b := 0]
test.avg.wide.dt[browning > greening, b.gt.g := 1]
test.avg.wide.dt[browning < greening, b.gt.g := 0]

test.avg.smry.dt <- test.avg.wide.dt[, .(frac.g.gt.b = sum(g.gt.b)/1000, frac.b.gt.g = sum(b.gt.g)/1000), by = landcov.name]
test.avg.smry.dt[frac.g.gt.b >= 0.95 | frac.b.gt.g >= 0.95, sig := 1]
test.avg.smry.dt
test.avg.smry.dt[is.na(sig), sig := 0]
test.avg.smry.dt$landcov.name <- factor(test.avg.smry.dt$landcov.name, levels = swi.lc.avg.dt$landcov.name)
setorder(test.avg.smry.dt, landcov.name)
test.avg.smry.dt[, yval := 1:10]
test.sig.dt <- test.avg.smry.dt[sig == 1]
test.sig.dt

# compute average SWI at sites that greenned vs browned for each land cover type ------------------------------------------
biome.swi.avg.mc.dt <- site.trnds.dt[trend.cat != '', .(landcov.name = 'Biome', swi.avg = mean(swi)), by = c('rep','trend.cat')]
swi.avg.mc.dt <- site.trnds.dt[trend.cat != '', .(swi.avg = mean(swi)), by = c('rep','trend.cat','landcov.name')]
swi.avg.mc.dt <- rbind(biome.swi.avg.mc.dt, swi.avg.mc.dt)
swi.avg.mc.dt$trend.cat <- factor(swi.avg.mc.dt$trend.cat, levels = c('greening','none','browning'))

swi.avg.mc.smry.dt <- swi.avg.mc.dt[, .(swi.avg.med = round(median(swi.avg),1), 
                                        swi.avg.q025 = round(quantile(swi.avg, 0.025),1), 
                                        swi.avg.q975 = round(quantile(swi.avg, 0.975),1)),
                                    by = c('landcov.name','trend.cat')]
swi.avg.mc.smry.dt
swi.avg.mc.smry.dt$landcov.name <- factor(swi.avg.mc.smry.dt$landcov.name, levels = swi.lc.avg.dt$landcov.name)
setorder(swi.avg.mc.smry.dt, landcov.name)



trend.cols <- c('springgreen3','gray','saddlebrown')

fig2 <- ggplot(swi.avg.mc.smry.dt, aes(x=landcov.name, y=swi.avg.med, group=trend.cat, fill=trend.cat)) + 
  geom_errorbar(aes(ymin = swi.avg.q025, ymax = swi.avg.q975), position = position_dodge(width = 0.5), width = 0.2, color = 'gray30', alpha = 0.5) +
  geom_point(aes(color = trend.cat, fill = trend.cat, shape=detectable), position = position_dodge(width = 0.5), shape = 21, size = 4, color = 'black', na.rm=T) + 
  scale_fill_manual(name = 'Trend:', values = trend.cols, guide = guide_legend(reverse = TRUE)) +
  coord_flip() + theme_bw() + theme(legend.position = c(0.7,0.2), legend.text=element_text(size=10), legend.title=element_text(size=12), 
                                    axis.text.y = element_text(color = lc.col.df$col), axis.text=element_text(size=12), axis.title=element_text(size=14)) + 
  labs(x = "", y = expression('Mean summer warmth index ('*degree*'C'*')')) +
  geom_vline(xintercept = c(9.5), linetype = 'dashed') + 
  annotate("text", x = test.sig.dt$yval, y=70, label = c("★"), size =  5)

fig2

