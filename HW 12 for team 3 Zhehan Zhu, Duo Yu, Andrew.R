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
###     Mental Health      ###        
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




