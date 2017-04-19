library(ggplot2)
library(RColorBrewer)
library(dplyr)

##############################################
############# Close  Time  Count #############
##############################################

CloseTimesDF <- as.data.frame(table(FamilyFinalData$CloseTimes))
colnames(CloseTimesDF) <- c("CloseTimes", "Count")

ggplot(CloseTimesDF, aes(CloseTimes, Count)) +
  geom_bar(position="dodge", stat="identity", fill="#66CDAA") +
  geom_label(aes(label=Count), color="#008080", size=4) + 
  ggtitle("Close Times Count") +
  geom_vline(aes(xintercept=1.691414), colour="red", linetype=5, size=1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) # Move the title to the center


##############################################
#############   Duration Count   #############
##############################################

DurationDF <- as.data.frame(table(FamilyFinalData$Duration))
colnames(DurationDF) <- c("Duration", "Count")

ggplot(DurationDF, aes(Duration, Count)) +
  geom_bar(position="dodge", stat="identity", fill="#708090") +
  geom_label(aes(label=Count), color="#2F4F4F", size=4) + 
  ggtitle("Duration Days Count") +
  geom_vline(aes(xintercept=14.9), colour="red", linetype=5, size=1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


##############################################
############# Service Type Count #############
##############################################

onlyHousing <- length(which(FamilyFinalData$Housing==T & FamilyFinalData$BasicNeeds==F & FamilyFinalData$FSC==F))
onlyBN <- length(which(FamilyFinalData$Housing==F & FamilyFinalData$BasicNeeds==T & FamilyFinalData$FSC==F))
onlyFSC <- length(which(FamilyFinalData$Housing==F & FamilyFinalData$BasicNeeds==F & FamilyFinalData$FSC==T))
Housing_BN <- length(which(FamilyFinalData$Housing==T & FamilyFinalData$BasicNeeds==T & FamilyFinalData$FSC==F))
Housing_FSC <- length(which(FamilyFinalData$Housing==T & FamilyFinalData$BasicNeeds==F & FamilyFinalData$FSC==T))
BN_FSC <- length(which(FamilyFinalData$Housing==F & FamilyFinalData$BasicNeeds==T & FamilyFinalData$FSC==F))
Housing_BN_FSC <- length(which(FamilyFinalData$Housing==T & FamilyFinalData$BasicNeeds==T & FamilyFinalData$FSC==T))

Type <- c("only Housing", "only BN", "only FSC", "Housing & BN", "Housing & FSC", "BN & FSC", "Housing & BN & FSC")
Count <- c(onlyHousing, onlyBN, onlyFSC, Housing_BN, Housing_FSC, BN_FSC, Housing_BN_FSC)
serviceTypeCount <- data.frame(Type, Count)

serviceTypeCount$Type <- factor(serviceTypeCount$Type, levels = unique(serviceTypeCount$Type)) #keep the order of types

ggplot(serviceTypeCount, aes(Type, Count)) + 
  geom_bar(position="dodge", stat="identity", fill="#ADD8E6") +
  geom_label(aes(label=Count), color="#000080", size=4) + 
  ggtitle("Service Type Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 







