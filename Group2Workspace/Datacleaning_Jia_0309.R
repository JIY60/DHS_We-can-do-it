#Data Cleaning
rm(list=ls())
setwd("/Users/yangjia/Desktop/R/week12/DHS_We-can-do-it")

## Team2--JIA
clientdat<-read.csv("ServiceBAData.csv")
library("dplyr")

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
library("stringi")

nClosedate<-function(closedates){
  a<-stri_count_fixed(closedates, ",")+1
  a[which(is.na(a))]<-1
  output<-a
}
# Client level
dat$nClose<-nClosedate(dat$CloseDate)
clientdat<-cbind(clientdat,dat$nClose)
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
clientdat<-cbind(clientdat,dat$duration)
# 3) Case level duration
case_group<-group_by(dat,CaseID)
dat2<-summarise(case_group,nClose=max(nClose),Duration=max(duration))

# Task 4 merge x and y variables
FamilyData<-merge(casedat,dat2,by.x="CASE_ID", by.y="CaseID")
write.csv(FamilyData,"FamilyFinalData.csv")

## Duration without NA

# 1) last close date
class(dat$CloseDate)
# length(which(is.na(dat$CloseDate))) is 2046
s_close<-strsplit(as.character(dat$CloseDate),split=",")
s_close<-sapply(s_close,sort)
s_close[which(s_close=="character(0)")]<-"NA" # 2046 characters have been changed

dat$lastClose<-sapply(s_close, tail, 1)
# table(dat$lastClose)
dat$lastClose[dat$lastClose=="NA"]<-"2017-03-01"
table(dat$lastClose)
# 2) Client level duration
dat$AcceptDate<-as.Date(dat$AcceptDate)
dat$lastClose<-as.Date(dat$lastClose)
dat$duration2=dat$lastClose-dat$AcceptDate

dat$duration2[which(dat$lastClose=="2017-03-01")]<-NA
clientdat<-cbind(clientdat,dat$duration2)
write.csv(clientdat,"ClientLeveldata.csv")

#case level
case_group<-group_by(dat,CaseID)
dat2<-summarise(case_group,nClose=max(nClose),Duration2=max(duration))
FamilyFinalData<-cbind(FamilyFinalData,duration2=dat2$Duration2)
FamilyFinalData<-select(FamilyFinalData,-(10:11))
FamilyFinalData<-rename(FamilyFinalData,duration2=dat2.Duration)
#head(FamilyFinalData)
write.csv(FamilyFinalData,"FamilyFinalData.csv",row.names = FALSE)


# Task 5 Graph: Placement before and duration
library(plyr)
library(ggplot2)
mu <- ddply(FamilyFinalData, "Placement", summarise, grp.mean=mean(Duration))
head(mu)
ggplot(FamilyFinalData, aes(x=Duration, fill=FSC,color=FSC)) + 
  geom_density(alpha=.6)+
  guides(color=FALSE)+
  ggtitle("Service Duration Affected by Pre-placement")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_discrete(labels = c("Not pre-placed", "Pre-placed"))+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Placement),
             linetype="dashed")

# Q2
FamilyFinalData<-read.csv("FamilyFinalData.csv")

df1<-cbind("housing",data.matrix(aggregate(CloseTimes ~ Housing, FamilyFinalData , mean )))
df2<-cbind("basic needs",data.matrix(aggregate(CloseTimes ~ BasicNeeds, FamilyFinalData , mean )))
df3<-cbind("FSC",data.matrix(aggregate(CloseTimes ~ FSC, FamilyFinalData , mean )))
means<-data.frame(rbind(df1,df2,df3))

ggplot(means,aes(x=V1,y=CloseTimes,fill=factor(Housing)))+
  geom_col(position="dodge",alpha=0.8)+
  scale_fill_discrete(name="Received Service Before",
                      breaks=c(0, 1),
                      labels=c("False", "True"))+
  xlab("Services")+ylab("Mean")+ggtitle("Average Close Times and Pre-Services")

df4<-cbind("Housing",data.matrix(aggregate(Duration ~ Housing, FamilyFinalData , mean )))
df5<-cbind("Basic needs",data.matrix(aggregate(Duration ~ BasicNeeds, FamilyFinalData , mean )))
df6<-cbind("FSC",data.matrix(aggregate(Duration ~ FSC, FamilyFinalData , mean )))
meansduration<-data.frame(rbind(df4,df5,df6))

meansduration$Duration<-round(as.numeric(meansduration$Duration),2)
ggplot(meansduration,aes(x=V1,y=Duration,fill=factor(Housing)))+
  geom_col(position="dodge",alpha=0.6)+
  scale_fill_discrete(name="Received Service Before",
                      breaks=c(0, 1),
                      labels=c("False", "True"))+
  xlab("Services")+ylab("Mean")+ggtitle("Average Duration and Pre-Services")

# Q2 experiments
library(ggplot2)
data<-read.csv("FamilyFinalData.csv")
ggplot(data, aes(x=Housing, y=Duration,colour=BasicNeeds))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point",show_guide=TRUE)+
  ggtitle("Conditional effect on duration")
###conditional result, result not obvious

# no condition
library(reshape2)
dat1<-select(data,c(CASE_ID,Housing,BasicNeeds,Duration,CloseTimes))
dat2<-melt(dat1,id=c("CASE_ID","Duration","CloseTimes"))

ggplot(dat2, aes(x=variable, y=Duration, colour=value))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point",show_guide=TRUE)+
  ggtitle("Service effect on duration")

data2<-read.csv("TypeCountsFinalData.csv")
ggplot(data2, aes(x=TypeCounts, y=CloseTimes,colour=PlacementAsY))+
  geom_jitter(shape=1)+
  geom_smooth(method=lm, se=TRUE)

ggplot(data, aes(x=Housing, y=CloseTimes, colour=Housing))+
  geom_boxplot()

# own graph
predata<-read.csv("Group2Workspace/group2data.csv")
familysize<-select(predata,c(CASE_ID,nClients))
mydata<-merge(data,predata,by="CASE_ID")

ggplot(mydata,aes(x=nClients,y=CloseTimes,colour=Placement))+
  geom_jitter(shape=1)+
  geom_smooth(method=lm, se=FALSE)
ggplot(mydata,aes(x=nClients,y=Duration,colour=Placement))+
  geom_jitter(shape=1)+
  geom_smooth(method=lm, se=FALSE)

###0418# Dig deeper
#36
table(casedat$DPW_FS)
table(casedat$DPW_GA)
table(casedat$DPW_SSI)
table(casedat$DPW_TANF)

a<-filter(casedat,DPW_TANF==-1)
length(which(a$HH==-1)) # 122
length(which(a$HACP==-1)) #67
length(which(a$ACHA==-1)) #184
length(which(a$DPW_FS==-1)) #191
length(which(a$DPW_GA==-1)) #317
length(which(a$DPW_SSI==-1)) #476
length(which(a$FSC==-1)) #250
#37
data<-read.csv("FamilyFinalData.csv")
data2<-read.csv("FamilyPreCYFData.csv")
deepData<-merge(data,data2,by="CASE_ID")
mean(deepData$Duration[which(deepData$DPW_FS==1)])
mean(deepData$Duration[which(deepData$DPW_GA==1)])
mean(deepData$Duration[which(deepData$DPW_SSI==1)])
mean(deepData$Duration[which(deepData$DPW_TANF==1)])
mean(deepData$Duration[which(deepData$BasicNeeds==1)])

services<-c("deepData$DPW_FS","deepData$DPW_GA","deepData$DPW_SSI","deepData$DPW_TANF","deepData$BasicNeeds")
for (i in services){
  a<-round(mean(deepData$Duration[which(i == -1)]),2)
  print(paste(i,a))}

mean(deepData$CloseTimes[which(deepData$DPW_FS==1)])
mean(deepData$CloseTimes[which(deepData$DPW_GA==1)])
mean(deepData$CloseTimes[which(deepData$DPW_SSI==1)])
mean(deepData$CloseTimes[which(deepData$DPW_TANF==1)])
mean(deepData$CloseTimes[which(deepData$BasicNeeds==1)])

group_by(deepData, BasicNeeds) %>%
summarise(percent = round(length(which(PlacementAsY == TRUE)) / n() * 100, 1))

a<-select(mydata,c(CASE_ID,nClients))
deepData<-merge(deepData,a,by="CASE_ID")

mean(deepData$nClients[which(deepData$DPW_FS==1)])
#
raw<-read.table("/Users/yangjia/Desktop/R/week12/DHS/ShortenClientsMerged.txt")

library("magrittr")
nChild<-raw %>%
  group_by(CaseID)%>%
  summarize(nChildren=length(CrossID[Role=="C"]))

deepData<-merge(deepData,nChild,by.x="CASE_ID",by.y="CaseID")
mean(deepData$nChildren[which(deepData$DPW_TANF==1)])
write.csv(deepData,"deepData.csv")