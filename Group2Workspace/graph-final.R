library(readr)
library(ggplot2)
library(plyr)
FamilyFinalData <- read_csv("~/DHS_We-can-do-it/FamilyFinalData.csv")

#######Three graphs of closetimes#######Alberto########
##
mu <- ddply(FamilyFinalData, "Housing", summarise, grp.mean=mean(CloseTimes))

ggplot(FamilyFinalData, aes(x=CloseTimes, fill=Housing,color=Housing)) + 
  geom_histogram(aes(y = ..density..),binwidth=.5, position="dodge", alpha=0.5)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Housing),
             linetype="dashed") +
  stat_function(fun = dnorm, colour = "Black",
                args = list(mean = mean(FamilyFinalData$CloseTimes, na.rm = TRUE),
                           sd = sd(FamilyFinalData$CloseTimes, na.rm = TRUE)))+
  scale_fill_discrete(breaks=c("FALSE", "TRUE"),
                      labels=c("Post-CYF Service & NA", "Pre-CYF Service")) +
  guides(color=FALSE)

##
mu <- ddply(FamilyFinalData, "BasicNeeds", summarise, grp.mean=mean(CloseTimes))

ggplot(FamilyFinalData, aes(x=CloseTimes, fill=BasicNeeds,color=BasicNeeds)) + 
  geom_histogram(aes(y = ..density..),binwidth=.5, position="dodge", alpha=0.5)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=BasicNeeds),
             linetype="dashed") +
  stat_function(fun = dnorm, colour = "Black",
                args = list(mean = mean(FamilyFinalData$CloseTimes, na.rm = TRUE),
                            sd = sd(FamilyFinalData$CloseTimes, na.rm = TRUE)))+
  scale_fill_discrete(breaks=c("FALSE", "TRUE"),
                      labels=c("Post-CYF Service & NA", "Pre-CYF Service")) +
  guides(color=FALSE)

##
mu <- ddply(FamilyFinalData, "FSC", summarise, grp.mean=mean(CloseTimes))

ggplot(FamilyFinalData, aes(x=CloseTimes, fill=FSC,color=FSC)) + 
  geom_histogram(aes(y = ..density..),binwidth=.5, position="dodge", alpha=0.5)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=FSC),
             linetype="dashed") +
  stat_function(fun = dnorm, colour = "Black",
                args = list(mean = mean(FamilyFinalData$CloseTimes, na.rm = TRUE),
                            sd = sd(FamilyFinalData$CloseTimes, na.rm = TRUE)))+
  scale_fill_discrete(breaks=c("FALSE", "TRUE"),
                      labels=c("Post-CYF Service & NA", "Pre-CYF Service")) +
  guides(color=FALSE)


############Three graphs of duration######Xiaoya##########
###
newdat<-group_by(FamilyFinalData,Housing)
housingmean<-summarise(newdat,housingmean=mean(Duration))

ggplot(FamilyFinalData, aes(x=Duration,color=Housing)) + 
  geom_density(size=1)+
  guides(color=FALSE)+
  geom_vline(data=housingmean,aes(xintercept = housingmean,color=Housing),linetype="dashed",size=1)+
  theme(legend.position = "top")+
  ggtitle("Housing")+
  theme_bw()
###
newdat1<-group_by(FamilyFinalData,BasicNeeds)
basicneedsmean<-summarise(newdat1,basicneedsmean=mean(Duration))

ggplot(FamilyFinalData, aes(x=Duration,color=BasicNeeds)) + 
  geom_density(size=1)+
  guides(color=FALSE)+
  geom_vline(data=basicneedsmean,aes(xintercept = basicneedsmean,color=BasicNeeds),linetype="dashed",size=1)+
  theme(legend.position = "top")+
  ggtitle("Basic Needs")+
  theme_bw()


newdat2<-group_by(FamilyFinalData,FSC)
FSCmean<-summarise(newdat2,FSCmean=mean(Duration))

ggplot(FamilyFinalData, aes(x=Duration,color=FSC)) + 
  geom_density(size=1)+
  guides(color=FALSE)+
  geom_vline(data=FSCmean,aes(xintercept = FSCmean,color=FSC),linetype="dashed",size=1)+
  theme(legend.position = "top")+
  ggtitle("Basic Needs")+
  theme_bw()


t.test(FamilyFinalData$Duration~FamilyFinalData$Housing)
t.test(FamilyFinalData$Duration~FamilyFinalData$BasicNeeds)
t.test(FamilyFinalData$Duration~FamilyFinalData$FSC)
