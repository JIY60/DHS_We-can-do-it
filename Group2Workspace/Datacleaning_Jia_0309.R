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

# Task2: duration
