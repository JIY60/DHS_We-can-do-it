library(readr)
dat <- read_csv("~/DHS_We-can-do-it/Group2Workspace/newduration.csv")
View(dat)








#graphing

ScatterDotSize<-function(x, y, s, n){ #n is the name of dataset
  library(ggplot2)
  ggplot(aes(x=x, y=y), data = n) +
    geom_point(aes(size=s)) +
    geom_smooth(method=lm) + # Add linear regression line 
    geom_jitter()
}
#number of MH
ScatterDotSize(newdat$nMH,newdat$nClose,newdat$nClients, newdat)
#ratio of MH
ScatterDotSize(newdat$ratio,newdat$nClose,newdat$nClients, newdat)





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



library(plotly)
plot_ly(dat, x = dat$ratio, y = dat$duration,
        mode = "markers", color = dat$ratio, size = dat$ratio)


















#example code
library(plotly)
library(ggplot2)

p <- ggplot(data = dat, aes(x = dat$ratio, y = dat$nClose)) +
  geom_point(aes(text = paste("test:", dat$nClose)), size = dat$nClose) +
  geom_smooth(method=lm,aes(colour = dat$duration, fill = dat$duration)) + facet_wrap(~ dat$nClose) +
  geom_jitter()
(gg <- ggplotly(p))

p <- ggplot(data = dat, aes(x = dat$ratio, y = dat$duration)) +
  geom_point(aes(text = paste("test:", dat$nClose)), size = dat$nClose) +
  geom_smooth(method=lm,aes(colour = dat$duration, fill = dat$duration)) + facet_wrap(~ dat$nClose) +
  geom_jitter()
(gg <- ggplotly(p))


#two y's close time and dureation
#make it into a function
