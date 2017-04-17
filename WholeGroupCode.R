# Read in MergedData 
mergedData <- read.csv("Data/MergedData.csv")

# Group 1 Shuning, Ziyi, Xiaoying 

########### Data Cleaning By Ziyi ###############
#calculate before and after for merged data (client level)
calServiceBA <- function(newMergedData){
  rows <- dim(newMergedData)[1]
  cols <- dim(newMergedData)[2]
  
  idxMax <- regexpr("MAX", names(newMergedData)[seq(5, length(newMergedData), by = 2)]) 
  idxMax <- idxMax - 2
  serviceNames <- substr(names(newMergedData)[seq(5, length(newMergedData), by = 2)], 1, idxMax)
  l <- length(serviceNames)
  concurrencyDataFrame <- data.frame(matrix(ncol = l + 2, nrow = 0))
  colnames(concurrencyDataFrame) <- c("CLIENT_ID", "CASE_ID", serviceNames)
  
  for (i in 1:rows){
    client_id = newMergedData[i, 1]
    case_id = newMergedData[i, 2]
    A <- as.Date(newMergedData[i, 3])
    concurrencyVector <- c(client_id, case_id)
    for (j in seq(5,cols,by = 2)){
      MAX <- as.Date(newMergedData[i, j])
      concurrency <- NA
      
      if (is.na(MAX)) {
        concurrency<-0
      } else if (MAX>=A) {
        concurrency <- 1
      } else if (MAX<A) {
        concurrency<--1
      }
      concurrencyVector[length(concurrencyVector) + 1] <- concurrency
    }
    concurrencyDataFrame[nrow(concurrencyDataFrame)+1,] <- concurrencyVector
  }
  return(concurrencyDataFrame)
}

# write the ServiceBAData.csv
write.csv(concurrencyDataFrame, "~/DHS_We-can-do-it/ServiceBAData.csv", row.names=FALSE)

########### Data Cleaning By Shunning ###############
### CalFamilyPreCYF.R
# convert client-level-before-after-data into family-level-before-data

preCYFIsTrue <- function(x){
  if (any(x == -1)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

calFamilyPreCYF <- function(serviceBAData){
  familyPreCYFData <- serviceBAData %>%
    group_by(CASE_ID) %>%
    summarise_each(funs(preCYFIsTrue), -CLIENT_ID)
  return(familyPreCYFData)
}

### AggrFamilyPreCYFCat.R
library(dplyr)

aggrFamilyPreCYFCat <- function(familyPreCYFData){
  names <- c("CASE_ID", "Placement", "Housing", "BasicNeeds", "FSC")
  placementTrue <- Reduce("|", select(familyPreCYFData, CYF_KPL, CYF_PL_O))
  housingTrue <- Reduce("|", select(familyPreCYFData, ACHA, HH, HACP))
  basicNeedsTrue <-  Reduce("|", select(familyPreCYFData, DPW_FS, DPW_GA, DPW_SSI, DPW_TANF))
  familyPreCYFAggrData <- cbind.data.frame(familyPreCYFData$CASE_ID, placementTrue, housingTrue, basicNeedsTrue, familyPreCYFData$FSC)
  colnames(familyPreCYFAggrData) <- names
  return(familyPreCYFAggrData)
}

### CalFamilyPlacePostCYF.R
# convert client-level-before-after-data into family-level-after-data for placement

postCYFIsTrue <- function(x){
  if (any(x == 1)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

calFamilyPlacePostCYF <- function(serviceBAData){
  familyPostCYFData <- serviceBAData %>%
    group_by(CASE_ID) %>%
    summarise(CYF_KPL = postCYFIsTrue(CYF_KPL), CYF_PL_O = postCYFIsTrue(CYF_PL_O))
  
  placementTrue <- Reduce("|", select(familyPostCYFData, CYF_KPL, CYF_PL_O))
  familyPlacePostCYFData <- cbind.data.frame(familyPostCYFData$CASE_ID, placementTrue)
  colnames(familyPlacePostCYFData) <- c("CASE_ID", "Placement")
  return(familyPlacePostCYFData)
}

### MergeXYVars.R
mergeXYVars <- function(xVars, placeAsY, durationAndCloseTimes){
  familyFinalData <- cbind.data.frame(xVars, placeAsY[,2], durationAndCloseTimes[,2:3])
  colnames(familyFinalData) <- c(colnames(xVars), "PlacementAsY", "CloseTimes", "Duration")
  return(familyFinalData)
}

### Binary X Data Cleaning

setwd("~/Desktop/Capstone/DHSTeam1-Ziyi-Shuning-Xiaoying") # change to where you put DHSTeam1 folder
rm(list = ls())
library(dplyr)
# mergedData <- read.csv("Data/MergedData.csv")
# 
# source("Functions/KeepOnlyService.R")
# cleanedData <- keepOnlyService(mergedData)
# 
# source("Functions/ConvertDate.R")
# convertedData <- convertDate(cleanedData)

# source("Functions/CalServiceBA.R")
# serviceBAData <- calServiceBA(convertedData)
# 
# write.csv(serviceBAData, "Data/ServiceBAData.csv", row.names=FALSE)

serviceBAData <- read.csv("Data/ServiceBAData.csv")

# source("Functions/CalFamilyPreCYF.R")
# familyPreCYFData <- calFamilyPreCYF(serviceBAData)
# write.csv(familyPreCYFData, "Data/FamilyPreCYFData.csv", row.names=FALSE)
# 
# source("Functions/AggrFamilyPreCYFCat.R")
# familyPreCYFCatData <- aggrFamilyPreCYFCat(familyPreCYFData)
# write.csv(familyPreCYFCatData, "Data/FamilyPreCYFCatData.csv", row.names=FALSE)

familyPreCYFCatData <- read.csv("Data/FamilyPreCYFCatData.csv")

# source("Functions/CalFamilyPlacePostCYF.R")
# familyPlacePostCYFData <- calFamilyPlacePostCYF(serviceBAData)
# 
# write.csv(familyPlacePostCYFData, "Data/FamilyPlacePostCYFData.csv", row.names=FALSE)

familyPlacePostCYFData <- read.csv("Data/FamilyPlacePostCYFData.csv")


durationAndCloseTimes <- read.csv("Data/DurationAndCloseTimes.csv")

source("Functions/MergeXYVars.R")
familyFinalData <- mergeXYVars(familyPreCYFCatData, familyPlacePostCYFData, durationAndCloseTimes)

# write.csv(familyFinalData, "Data/FamilyFinalData.csv", row.names=FALSE)

### CalTypeCounts.R
# calculate how many types of pre-CYF services one family receives.
# Housing: ACHA, HH, HACP
# Basic Needs: DPW........
# FSC
library(dplyr)
calTypeCounts <- function(familyPreCYFData){
  test <- familyPreCYFData %>%
    select(CASE_ID:ACHA, DPW_FS:DPW_TANF, FSC:HH)
  typeCounts <- NULL
  for (i in 1:dim(test)[1]){
    typeCounts <- c(typeCounts, length(which(test[i,-1] == TRUE)))
  }
  typeCountsDataFrame <- data.frame(test$CASE_ID, typeCounts)
  colnames(typeCountsDataFrame) <- c("CASE_ID", "TypeCounts")
  return(typeCountsDataFrame)
}


### Type Counts X Data Cleaning and Plotting

setwd("~/Desktop/Capstone/DHSTeam1-Ziyi-Shuning-Xiaoying") # change to where you put DHSTeam1 folder
rm(list = ls())
library(dplyr)

## familyPreCYFData <- read.csv("Data/Data/FamilyPreCYFData.csv")
## 
## source("Functions/CalTypeCounts.R")
## typeCountsData <- calTypeCounts(familyPreCYFData)

# typeCountsData <- read.csv("Data/TypeCountsData.csv")
# 
# familyPlacePostCYFData <- read.csv("Data/FamilyPlacePostCYFData.csv")
# durationAndCloseTimes <- read.csv("Data/DurationAndCloseTimes.csv")
# 
# source("Functions/MergeXYVars.R")
# typeCountsFinalData <- mergeXYVars(typeCountsData, familyPlacePostCYFData, durationAndCloseTimes)

typeCountsFinalData <- read.csv("Data/TypeCountsFinalData.csv")

## Temporarily put graph codes here

# bar plot about types of services and placement
library(ggplot2)
ggplot(typeCountsFinalData, aes(TypeCounts, fill = PlacementAsY, order = -as.numeric(PlacementAsY))) + 
  geom_bar(stat = "bin", position = "fill", alpha=0.6) + 
  xlab("Types of Services") +
  ylab("Place / Not Place Percentages") +
  ggtitle("Types of Services and Placement")


# bar plot about types of services and close times
# change CloseTimes to factor format
typeCountsFinalData$CloseTimes <- as.factor(typeCountsFinalData$CloseTimes)

ggplot(typeCountsFinalData, aes(TypeCounts, fill = CloseTimes, order = -as.numeric(CloseTimes))) + 
  geom_bar(stat = "bin", position = "fill", alpha=0.6) + 
  scale_fill_brewer(palette = "Blues") + 
  xlab("Types of Services") +
  ylab("CloseTimes") +
  ggtitle("Types of Services and CloseTimes")

# kernel density plot about types of services and duration
# change TypeCounts to factor format
typeCountsFinalData$TypeCounts <- as.factor(typeCountsFinalData$TypeCounts)

ggplot(typeCountsFinalData, aes(TypeCounts, Duration)) + 
  geom_boxplot()

ggplot(typeCountsFinalData, aes(Duration, fill = TypeCounts)) + 
  geom_density(alpha=0.3)+
  scale_fill_brewer(palette = "Blues") + 
  ggtitle("Types of Services and Duration")+
  theme_bw() +
  facet_wrap(~TypeCounts)

########### Data Cleaning By Xiaoying ###############
#plot the bar graph 
library("ggplot2")
g<-ggplot(plotData,aes(y=percent, x=serviceName, fill=serviceStatus))
g+geom_bar(stat = "identity",position = "dodge",alpha=0.6, width = 0.5) +coord_flip() +
  geom_text(aes(label=percent), position=position_dodge(width=0.5),hjust=-0.4, vjust=0) + 
  scale_fill_discrete(name="Service Status", labels=c("Post-CYF Service & NA","Pre-CYF Service")) +
  ylab("Percentage of Children Placed") +
  xlab("Services")+
  ggtitle("Pre-CYF Services and Placement") +
  theme(axis.text=element_text(size=14,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size = 20, face = "bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.title=element_text(size=14,face="bold"))


# Group 2 Jia, Xiaoya, Alberto 

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


# Group 3 Duo, Zhehan, Andrew

install.packages("plotrix")
install.packages("readr")
MergedData <- read.csv("MergedData.csv")
install.packages("data.table")
library(plotrix)
library(readr)
install.packages("RColorBrewer")
library(RColorBrewer)
MergedData <- read.csv("MergedData.csv")
install.packages("data.table")
library("data.table")

###################################
###     Service Type Count      ###  Duo Yu
###################################
#Distribution of service Type Counts on family level
dat <- MergedData[,c(grep("CASE_ID", colnames(MergedData)),
                     grep("ACHA_MAX_ACTIVE", colnames(MergedData)),
                     grep("HH_MAX_ACTIVE", colnames(MergedData)),
                     grep("HACP_MAX_ACTIVE", colnames(MergedData)),
                     grep("DPW_FS_MAX_ACTIVE", colnames(MergedData)),
                     grep("DPW_GA_MAX_ACTIVE", colnames(MergedData)),
                     grep("DPW_SSI_MAX_ACTIVE", colnames(MergedData)),
                     grep("DPW_TANF_MAX_ACTIVE", colnames(MergedData)),
                     grep("FSC_MAX_ACTIVE", colnames(MergedData)))]
caseID <- unique(dat$CASE_ID)

numOfType <- NULL
i <- 1
# For each caseID, count how many service columns with entire NA
for (c in caseID){
  df <- dat[which(dat$CASE_ID==c), ]
  x <- 0   # x: how many columns with entire NA
  for (j in 2:9){ #col2 to col9: columns of the 8 services
    if(all(is.na(df[,j]))){ # check if this column is with entire NA
      x <- x+1
    }
  }
  y <- 8-x # y : how many types of services does this caseID have
  numOfType[i] <- y
  i <- i+1
}

countType <- data.frame(caseID, numOfType)

x <- table(countType$numOfType)
piepercent <- round(100*x/sum(x), 1) 
piepercent <-paste(piepercent, "%", sep = "")
par(mfrow=c(1,1),mar=c(2,1.5,1,0.5),oma=c(2,1.5,1,0.5))
pie(x, labels = piepercent, radius = 0.8,main="Pie Chart of Service Type Count", col=brewer.pal(9,"Blues"), clockwise = FALSE,
    
    density = NULL, angle = 45, lty = NULL, border = NULL, edges = 200)

#pie3D(x,labels=piepercent, explode=0.1, 
#    main="Pie Chart of Service Type Count")
legend("bottom", legend=c("0", "1", "2", "3", "4", "5", "6", "7", "8"), 
       cex = 0.6, horiz = T, fill =brewer.pal(9,"Blues") )

#pie(x, labels = piepercent, radius = 0.8,main="Pie Chart of Service Type Count", col=brewer.pal(9,"Blues"), clockwise = FALSE,
#  density = NULL, angle = 45, lty = NULL, border = NULL, edges = 200)

###################################
###     Mental Health      ###      Zhehan  
###################################
#The relationship between role and child placement: the distribution of child from family which their mother, father or both received mental health service.
datMH <- MergedData[ ,c("CLIENT_ID","CASE_ID", "ROLE", "MH_MAX_ACTIVE", "ACCEPT_REASON")]
datMH <- subset(datMH, (ROLE=="Mother" | ROLE=="Father") & ACCEPT_REASON=="Child placed" & !is.na(MH_MAX_ACTIVE),
                select=CLIENT_ID:ACCEPT_REASON) #118 families

roleCount <- as.data.frame(tapply(datMH$ROLE, datMH$CASE_ID, function(x) length(unique(x))))
setDT(roleCount, keep.rownames = TRUE)[]
colnames(roleCount) <- c("CASE_ID","Count of role")
caseID_MorF <- roleCount$CASE_ID[which(roleCount$`Count of role`==1)] #58 families
# Subset of cases of which only the mother or the father has accepted MH service
datMH_MorF <- subset(datMH, CASE_ID %in% caseID_MorF, CLIENT_ID:ACCEPT_REASON)
numM <- length(which(datMH_MorF$ROLE=="Mother"))
numF <- length(which(datMH_MorF$ROLE=="Father"))

numP <- length(which(tapply(datMH$ROLE, datMH$CASE_ID, function(x) length(unique(x)))==2))
numChildPlaceTotal <- nrow(subset(datMH, ACCEPT_REASON=="Child placed",select=CLIENT_ID:ACCEPT_REASON))

role <- c("Mother accepted MH", "Father accepted MH", "Mother&Father accepted MH")
countOfFamily <- c(numM, numF, numP)
datMH_Parent <- data.frame(role, countOfFamily)

x <- datMH_Parent$countOfFamily
piepercent <- c(round(100*x/802, 2), round(100*(802-sum(x))/802, 2))

y <- c(x, 802-sum(x))

pie(y, labels = piepercent, radius = 0.8,main="Pie Chart of Child Placed", col=brewer.pal(4,"Blues"), clockwise = FALSE,
    density = NULL, angle = 45, lty = NULL, border = NULL, edges = 200)
#pie3D(y,labels=piepercent, explode=0.1, main="Pie Chart of Child Placed")
par(mfrow=c(1,1),mar=c(3,2,2,2),oma=c(3,2,2,2))
legend("bottom", legend=c("Mother accepted MH", "Father accepted MH", "Mother & Father accepted MH", "Other"), 
       cex = 0.6, horiz = F, fill =brewer.pal(4,"Blues") )




###################################
###     Data Description      ###  Andrew
###################################

cohort <- read_excel("DHS_Case_Clients_2016EntryCohort.xlsx",
                     sheet = "DHS_Case_Clients_2016EntryCohor") #16639 obs.

cross <- read_excel("DHS_CrossSystem.xlsx",
                    sheet = "SystemInvolvement_EC2016")

lenunique <- function(x) {
  length(unique(x))
}

lenunique(cohort$CASE_ID) #1562
lenunique(cohort$CLIENT_ID) #8866
lenunique(cross$CLIENT_ID) #8206
sumTab <- data.frame(uniqueCases = lenunique(cohort$CASE_ID), clients = lenunique(cohort$CLIENT_ID), CrossClients = lenunique(cross$CLIENT_ID)  )
sumTab # Summary Data created for Data Description Slide 

library(lubridate)
casebyMonth <- tapply(cohort$CASE_ID, cohort$SRVC_ACPT_DT_MY, lenunique)
casebyMonth1 <- data.frame(month = names(casebyMonth), count=casebyMonth, row.names = NULL)

casebyMonth1$month <- dmy(paste("01-", casebyMonth1$month , sep =""))
casebyMonth2 <- casebyMonth1[order(as.Date(casebyMonth1$month)),]

m <- ggplot(data = casebyMonth2, aes(x = month, y = count )) 
m + geom_bar(stat = "identity", width = 12) + 
  #theme(axis.text.x=element_text(size = 10, angle=35, hjust=1)) +
  xlab("Month") +
  ylab("Count") +
  theme(plot.background = element_rect(fill = 'azure1')) +
  ggtitle("Number of CYF Cases accepting services by Month 2016")
