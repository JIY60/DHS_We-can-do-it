setwd("/Users/yangjia/Desktop/R/week12/DHS/Group2Workspace") 
dat<-DHS_Case_Clients_2016EntryCohort

library("dplyr")
dim(dat)
#Create Duration variable
#check: table(dat$CLOSE_DT)
dat$close_dt<-as.Date(paste("01-",as.character(dat$CLOSE_DT),sep=""), format="%d-%B-%Y")
dat$start_dt<-as.Date(paste("01-",as.character(dat$CL_INLV_START),sep=""), format="%d-%B-%Y")

#check: table(dat$close_dt)
#check: length(which(is.na(dat$close_dt)))
dat$close_dt[is.na(dat$close_dt)]<-as.Date("2017-04-01")
dat<-arrange(dat,CASE_ID,CLIENT_ID,desc(close_dt))

dat$index<-ave(dat$CLIENT_ID,dat$CASE_ID,FUN=seq_along)
dat$duration<-dat$close_dt-dat$start_dt
dat_short<-filter(dat,index==1)
dat_short<-select(dat_short,c(CASE_ID,close_dt,start_dt,duration))

dat_old<-read.csv("newdat.csv")
newduration<-merge(dat_short,dat_old,by.x = "CASE_ID", by.y ="CaseID",all=FALSE)
write.csv(newduration, "newduration.csv")

# Merge with currency variable
setwd("/Users/yangjia/Desktop/R/week12/DHS/Group2Workspace") 
dat2<-read.table("/Users/yangjia/Desktop/R/week12/DHS/MergedWithCurrency")
by_case<- group_by(dat2, CASE_ID)
MHt <- summarise(by_case, MHt = min(MH,na.rm=TRUE))
MHt<-arrange(MHt,CASE_ID)
group2data<-data.frame(datnew,MHt)

write.csv(group2data,"group2data.csv")

# New duration variable
group2data$duration_week<-round(group2data$duration/7,2)
group2data$duration_week[group2data$duration_week<=0]<-1

#clean dataset
group2data<-select(group2data,c(CASE_ID:duration,nClients:ratio,MHt:duration_week))
group2data[,c(1:10)]
group2data<-group2data[,c(1,5,7,2,3,4,10,6,8,9)]
# Data summary

