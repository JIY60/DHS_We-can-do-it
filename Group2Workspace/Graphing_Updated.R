








#graphing

dat<-read.table("ShortenClientsMerged.txt")

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


#################################################
#trying to make it pretty
#################################################



library(plotly)
plot_ly(dat2, x = dat2$ratio, y = dat2$nClose,
        mode = "markers", color = dat2$ratio, size = dat2$ratio)




ggplot(aes(x=x, y=y), data = n) +
  geom_point(aes(size=s)) +
  geom_smooth(method=lm) + # Add linear regression line 
  geom_jitter()


#example code
p <- ggplot(data = dat2, aes(x = dat2$ratio, y = dat2$nClose)) +
  geom_point(aes(text = paste("test:", dat2$nClose)), size = 4) +
  geom_smooth(aes(colour = dat2$nClose, fill = dat2$nClose)) + facet_wrap(~ dat2$nClose) +
  geom_jitter()

(gg <- ggplotly(p))