library(readr)
dat<- read_csv("~/DHS_We-can-do-it/ConcurrencyDataFrame.csv")
dat1<-read.table("ShortenClientsMerged.txt")
library("dplyr")
short<-select(dat,CASE_ID,CLIENT_ID,MH)
dat<-arrange(dat,CASE_ID,CLIENT_ID)
dat1<-arrange(dat1,CaseID,CrossID)
short<-arrange(short,CASE_ID,CLIENT_ID)

bigdat<-merge(dat1,short,by.x = "CrossID",by.y = "CLIENT_ID")
small<-select(bigdat,CASE_ID,CrossID,MH1,MH)

bigbigdat<-merge(dat1,dat,by.x = "CrossID",by.y = "CLIENT_ID")
write.table(bigbigdat,file = "MergedWithCurrency")
