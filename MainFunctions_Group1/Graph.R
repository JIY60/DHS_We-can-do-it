generateBoxPlot <- function(finalData, yDataName, title){
  ggplot(data=finalData, aes_string(x = "placement", y = yDataName)) + 
    geom_boxplot() +
    ggtitle(title)
}

# boxplot with facet_grid
generateTFPlot<-function(a,b,c,d,e,f){
  numService<-c(a,b,c,d,e)
  nameService<-c(rep("Total",1549),rep("Housing",1549),rep("Behavior",1549),rep("Nutrition",1549),rep("Mental",1549))
  placed<-rep(f,5)
  datPlot<-data.frame(numService,services=as.factor(nameService),placed)
  p<-ggplot(datPlot, aes(services, numService)) 
  p+ geom_boxplot(varwidth = TRUE, outlier.colour = "grey")+
    facet_grid(.~placed)+
    ylab("numServiceFamily")+
    ggtitle("Number of Services and Children Placement Comparison Graph")
}

# organize multiple plot in one layout
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}