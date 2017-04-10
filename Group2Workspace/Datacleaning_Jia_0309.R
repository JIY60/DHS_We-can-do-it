#Data Cleaning
rm(list=ls())
setwd("/Users/yangjia/Desktop/R/week12/DHS")

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