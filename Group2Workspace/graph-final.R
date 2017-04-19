library(readr)
library(ggplot2)
library(dplyr)
library(plyr)
FamilyFinalData <- read_csv("~/DHS_We-can-do-it/FamilyFinalData.csv")



#######Three graphs of closetimes#######Alberto########
##
mu <- ddply(FamilyFinalData, "Housing", summarise, grp.mean=mean(CloseTimes))

ggplot(FamilyFinalData, aes(x=CloseTimes, fill=Housing,color=Housing)) + 
  geom_density(aes(y = ..density..),binwidth=.5, position="dodge", alpha=0.5)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Housing),
             linetype="dashed") +
  stat_function(fun = dnorm, colour = "Black",
                args = list(mean = mean(FamilyFinalData$CloseTimes, na.rm = TRUE),
                           sd = sd(FamilyFinalData$CloseTimes, na.rm = TRUE)))+
  scale_fill_discrete(breaks=c("FALSE", "TRUE"),
                      labels=c("Post-CYF Service & NA", "Pre-CYF Service")) +
  guides(color=FALSE)

ggplot(FamilyFinalData, aes(x=Housing, y = CloseTimes, color=Housing)) + 
  geom_boxplot() +
  guides(color=FALSE) +
  theme_bw()



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


ggplot(FamilyFinalData, aes(x=BasicNeeds, y = CloseTimes, color=BasicNeeds)) + 
  geom_boxplot() +
  guides(color=FALSE) +
  theme_bw()


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

ggplot(FamilyFinalData, aes(x=FSC, y = CloseTimes, color=FSC)) + 
  geom_boxplot() +
  guides(color=FALSE) +
  theme_bw()


############Three graphs of duration######Xiaoya##########
###
newdat<-group_by(FamilyFinalData,Housing)
housingmean<-summarise(newdat,housingmean=mean(Duration))

ggplot(FamilyFinalData, aes(x=Duration,color=Housing)) + 
  geom_density(size=1)+
  geom_vline(data=housingmean,aes(xintercept = housingmean,color=Housing),linetype="dashed",size=1)+
  geom_vline(xintercept = 147.102,size=1,color="blue")+
  annotate("text", x=170, y=0.0045, label="16",size=5)+
  theme_bw()+
  theme(legend.position = "top")+
  theme(legend.title=element_blank())+
  ggtitle("Housing")+
  theme(plot.title = element_text(size=22))


###
newdat1<-group_by(FamilyFinalData,BasicNeeds)
basicneedsmean<-summarise(newdat1,basicneedsmean=mean(Duration))

ggplot(FamilyFinalData, aes(x=Duration,color=BasicNeeds)) + 
  geom_density(size=1)+
  geom_vline(data=basicneedsmean,aes(xintercept = basicneedsmean,color=BasicNeeds),linetype="dashed",size=1)+
  geom_vline(xintercept = 147.102,size=1,color="blue")+
  annotate("text", x=170, y=0.0045, label="18",size=5)+
  theme_bw()+
  theme(legend.position = "top")+
  ylim(0.000,0.005)+
  theme(legend.title=element_blank())+
  ggtitle("Basic Needs")+
  theme(plot.title = element_text(size=22))
  

###
newdat2<-group_by(FamilyFinalData,FSC)
FSCmean<-summarise(newdat2,FSCmean=mean(Duration))

ggplot(FamilyFinalData, aes(x=Duration,color=FSC)) + 
  geom_density(size=1)+
  geom_vline(data=FSCmean,aes(xintercept = FSCmean,color=FSC),linetype="dashed",size=1)+
  geom_vline(xintercept = 147.102,size=1,color="blue")+
  annotate("text", x=170, y=0.0045, label="10",size=5)+
  theme_bw()+
  theme(legend.position = "top")+ 
  ylim(0.000,0.005)+
  theme(legend.title=element_blank())+
  ggtitle("FSC")+
  theme(plot.title = element_text(size=22))
  

################
t.test(FamilyFinalData$Duration~FamilyFinalData$Housing)
t.test(FamilyFinalData$Duration~FamilyFinalData$BasicNeeds)
t.test(FamilyFinalData$Duration~FamilyFinalData$FSC)


#non parametric version 
wilcox.test(FamilyFinalData$Duration~FamilyFinalData$Housing) 

###############
housingmean<-mutate(housingmean,Service="Housing")
colnames(housingmean)[1]<-"Status"
colnames(housingmean)[2]<-"Duration"
basicneedsmean<-mutate(basicneedsmean,Service="BasicNeeds")
colnames(basicneedsmean)[1]<-"Status"
colnames(basicneedsmean)[2]<-"Duration"
meanduration<-rbind(housingmean,basicneedsmean)
meanduration$Service<-factor(meanduration$Service, levels=c("Housing","BasicNeeds")) 
meanduration$Duration<-round(meanduration$Duration,2)

ggplot(meanduration,aes(x=Service,y=Duration,fill=Status))+
  geom_col(position="dodge",alpha=0.6,width = 0.5)+
  ylim(0,160)+
  geom_text(aes(label = Duration,vjust = -0.5, hjust = 0.5, color = "Status", size=3), show.legend  = FALSE)+
  theme(legend.title=element_blank())+
  ggtitle("Mean of duration")+
  theme(plot.title = element_text(size=22))

#needs to be made into two 
  
data2<-read.csv("TypeCountsFinalData.csv")

ggplot(data2, aes(x=TypeCounts, y=CloseTimes,colour=PlacementAsY))+
  geom_jitter(shape=1)+
  geom_smooth(method=lm, se=TRUE) +
  labs(color='Placement') +
  theme_bw()

ggplot(data2, aes(x=TypeCounts, y=Duration,colour=PlacementAsY))+
  geom_jitter(shape=1)+
  geom_smooth(method=lm, se=TRUE) +
  labs(color='Placement') +
  theme_bw()



