########### Data Cleaning By JIA ###############
clientdat<-read.csv("ServiceBAData.csv")
library("dplyr")
library("stringi")
library(plyr)

# Task1: Aggreagate Client level-->Case level. 
# count as -1 if before, 1 if any after, 0 if all NA

# Function groupvalue: create variable that assign value1 if any value1, else: value2 if any value2, else: value 3. 
groupvalue<-function(var,value1=-1,value2=1,value3=0){
  output<-ifelse(any(var==value1), value1,ifelse(any(var==value2),value2,value3))
}

case_group<-group_by(clientdat,CASE_ID)
casedat<-summarise_each(case_group,funs(groupvalue))
print(casedat)

casedat<-select(casedat,-(CLIENT_ID))
write_csv(casedat,"Caselevel_service.csv")

#### Task2: close times
dat<-read.table("ShortenClientsMerged.txt")

nClosedate<-function(closedates){
  a<-stri_count_fixed(closedates, ",")+1
  a[which(is.na(a))]<-1
  output<-a
}

# Client level
dat$nClose<-nClosedate(dat$CloseDate)
# Case level
case_group<-group_by(dat,CaseID)
dat2<-summarise(case_group,nClose=max(nClose)) # caseID and nClose table
# table(dat2$nClose)

### Task3: duration
# 1) last close date
class(dat$CloseDate)
# length(which(is.na(dat$CloseDate))) is 2046
s_close<-strsplit(as.character(dat$CloseDate),split=",")
s_close<-sapply(s_close,sort)
s_close[which(s_close=="character(0)")]<-"NA" # 2046 characters have been changed

dat$lastClose<-sapply(s_close, tail, 1)
# table(dat$lastClose)
dat$lastClose[dat$lastClose=="NA"]<-"2017-02-01"
# 2) Client level duration
dat$AcceptDate<-as.Date(dat$AcceptDate)
dat$lastClose<-as.Date(dat$lastClose)
dat$duration=dat$lastClose-dat$AcceptDate
# table(dat$duration)

# 3) Case level duration
case_group<-group_by(dat,CaseID)
dat2<-summarise(case_group,nClose=max(nClose),Duration=max(duration))

# Task 4 merge x and y variables
FamilyData<-merge(casedat,dat2,by.x="CASE_ID", by.y="CaseID")
write.csv(FamilyData,"FamilyData.csv")


# Task 5 Graph: Placement before and duration
mu <- ddply(FamilyFinalData, "Placement", summarise, grp.mean=mean(Duration))
head(mu)
ggplot(FamilyFinalData, aes(x=Duration, fill=Placement,color=Placement)) + 
  geom_density(alpha=.6)+
  guides(color=FALSE)+
  ggtitle("Service Duration Affected by Pre-placement")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_discrete(labels = c("Not pre-placed", "Pre-placed"))+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Placement),
             linetype="dashed")


library(readr)
library(ggplot2)
library(plyr)
FamilyFinalData <- read_csv("~/DHS_We-can-do-it/FamilyFinalData.csv")

#######Three graphs of closetimes####### Alberto ########
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


############Three graphs of duration###### Xiaoya ##########
###
newdat<-group_by(FamilyFinalData,Housing)
housingmean<-summarise(newdat,housingmean=mean(Duration))

ggplot(FamilyFinalData, aes(x=Duration, fill=Housing,color=Housing)) + 
  geom_density(alpha=.6)+
  guides(color=FALSE)+
  geom_vline(data=housingmean,aes(xintercept = housingmean,color=Housing),linetype="dashed")+
  theme(legend.position = "top")+
  scale_fill_discrete(name = "Housing",labels = c("Post-CYF Servie and NA", "Pre-CYF Service"))
###
newdat1<-group_by(FamilyFinalData,BasicNeeds)
basicneedsmean<-summarise(newdat1,basicneedsmean=mean(Duration))

ggplot(FamilyFinalData, aes(x=Duration, fill=BasicNeeds,color=BasicNeeds)) + 
  geom_density(alpha=.6)+
  guides(color=FALSE)+
  geom_vline(data=basicneedsmean,aes(xintercept = basicneedsmean,color=BasicNeeds),linetype="dashed")+
  theme(legend.position = "top")+
  scale_fill_discrete(name = "Basic Needs",labels = c("Post-CYF Servie and NA", "Pre-CYF Service"))
###
newdat2<-group_by(FamilyFinalData,FSC)
FSCmean<-summarise(newdat2,FSCmean=mean(Duration))

ggplot(FamilyFinalData, aes(x=Duration, fill=FSC,color=FSC)) + 
  geom_density(alpha=.6)+
  guides(color=FALSE)+
  geom_vline(data=FSCmean,aes(xintercept = FSCmean,color=FSC),linetype="dashed")+
  theme(legend.position = "top")+
  scale_fill_discrete(name = "FSC",labels = c("Post-CYF Servie and NA", "Pre-CYF Service"))
