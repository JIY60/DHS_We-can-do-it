#get ready
install.packages("dplyr")
library("dplyr")
install.packages("stringi")
library("stringi")
dat<-read.table("ShortenClientsMerged.txt")
dat<-arrange(dat,CaseID)

# 1 FamilyID and number of people in each family
PeopleinCase<-function(datasetname,groupname){
  datasetname<-group_by_(datasetname,groupname)
  newdata<-summarise(datasetname, nClients=n())
}

nFamily<-PeopleinCase(dat,"CaseID")

# 2 number of people recieving MH service in each family
lengthnoNA<-function(x){
  length(x[-which(is.na(x))])
}
nService<-function(x,group){
  output<-tapply(x,group,lengthnoNA)
}

nMH<-nService(dat$MH1,dat$CaseID)

# 3 number of times a family receiving CYF service (close times include NA)
nClosedate<-function(group,client,closedates){
  a<-stri_count_fixed(closedates, ",")+1
  a[which(is.na(a))]<-1
  index<-ave(client,group,FUN=seq_along)
  output<-a[which(index==1)]
}

nClose<-nClosedate(dat$CaseID,dat$CrossID,dat$CloseDate)

# cbind the result 1,2 and 3 to create a new data frame. 
newdat<-cbind(nFamily,nMH,nClose)
newdat<-mutate(newdat,ratio=round(nMH/nClients,2))

write.csv(newdat,file = "newdat.csv")

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

