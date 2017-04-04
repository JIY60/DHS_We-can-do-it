rm(list=ls()) 
install.packages("readxl")
install.packages("ggplot2")
library(ggplot2)
library(readxl)
library(tibble)
#####################################################
##########     Step 1 - Data Cleaning       #########
#####################################################
cohort <- DHS_Case_Clients_2016EntryCohort #16639 obs.
lenunique <- function(x) {
  length(unique(x))
}
numCases <- tapply(cohort$CASE_ID, cohort$CLIENT_ID, lenunique)
listOfClient=names(numCases)
multicaseID <- which(numCases>1)
multicaseclient <- listOfClient[multicaseID]
tapply(multi_cohort$CASE_ID, multi_cohort$CLIENT_ID, lenunique)#check
multicaseIdxCohort <- which(cohort$CLIENT_ID %in% multicaseclient)
multi_cohort <- cohort[multicaseIdxCohort,]
write.csv(multi_cohort, "CohortCrossMerged.csv")
# remove datasets and variables not needed 
rm(DHS_Case_Clients_2016EntryCohort)
rm(listOfClient, multicaseclient, multicaseID, multicaseIdxCohort, numCases)
rm(lenunique)

#####################################################
############     Step 2 - Plotting       ############
#####################################################

# Plot for different role groups
child <- multi_cohort$ACCEPT_REASON[multi_cohort$ROLE=="Child"]
parent <- multi_cohort$ACCEPT_REASON[multi_cohort$ROLE=="Mother" || multi_cohort$ROLE=="Father"]
other <- multi_cohort$ACCEPT_REASON[multi_cohort$ROLE=="Other"]

barplot(table(child), main="When involed as a child", xlab="Case Accept Reason",las = 2, ylab="Numbers")
barplot(table(parent), main="When involed as a mother/father", xlab="Case Accept Reason",las = 2, ylab="Numbers")
barplot(table(other), main="When involed as others", xlab="Case Accept Reason", las = 2, ylab="Numbers")

par(mai=c(1.02,0.82,0.82,0.42))
child2<-replace(child,"Child behavior and parent inability to cope/control,  Family is in need of service only offered through CYS or they cannot afford needed services",1)
child3<-replace(child2," Child placed,  Family is in need of service only offered through CYS or they cannot afford needed services",2)
child4<-replace(child3," Family is in need of service only offered through CYS or they cannot afford needed services",3)

