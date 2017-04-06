ScatterDotSize_INTERACTIVE<-function(x, y, s, n){ #n is the name of dataset
  library(ggplot2)
  library(plotly)
  p <- ggplot(data = n, aes(x = x, y = y)) +
    geom_point(aes(text = paste("test:", dat$nClose)), size = dat$nClose) +
    geom_smooth(method=lm,aes(colour = dat$duration, fill = dat$duration)) + facet_wrap(~ dat$nClose) +
    geom_jitter()
  (gg <- ggplotly(p))
}



#################################################
#trying to make it pretty
#################################################

library(readr)
dat <- read_csv("~/DHS_We-can-do-it/Group2Workspace/group2data.csv")
View(dat)



#example code
library(plotly)
library(ggplot2)

dat <- dat[dat$ratio>=0.01,]
dat <- dat[dat$MHt!=1,]

p <- ggplot(data = dat, aes(x = dat$ratio, y = dat$nClose, color=dat$duration_week)) +
  geom_point(shape=1, alpha=1/2) +
  geom_smooth(method=lm,colour="black") + facet_grid(dat$MHt~., scales = "free", labeller = "label_both") +
  geom_jitter() +
  scale_color_gradient(low="#05D9F6", high="#5011D1", limits=c(0.0, 1100.14)) +
  ylab("Open/Close Dates") +
  xlab("MH Ratio")
ggplotly(p)


p <- ggplot(data = dat, aes(x = dat$ratio, y = dat$nClose, color=dat$duration_week)) +
  geom_point(shape=1, alpha=1/2) +
  geom_smooth(method=lm,colour="black") + facet_grid(dat$MHt~., labeller = "label_both") +
  geom_jitter() +
  scale_color_gradient(low="#05D9F6", high="#5011D1", limits=c(0.0, 1100.14)) +
  ylab("Open/Close Dates") +
  xlab("MH Ratio") 
ggplotly(p)






