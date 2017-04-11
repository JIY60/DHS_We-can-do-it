

library(readr)
FamilyFinalData <- read_csv("~/DHS_We-can-do-it/FamilyFinalData.csv")
View(FamilyFinalData)

library(ggplot2)


library(plyr)
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



ggplot(FamilyFinalData, aes(x=CloseTimes, fill=BasicNeeds)) + 
  geom_density(alpha=.6)

ggplot(FamilyFinalData, aes(x=CloseTimes, fill=FSC)) + 
  geom_density(alpha=.6)






ggplot(FamilyFinalData, aes(x=Duration, fill=Housing)) + 
  geom_density(alpha=.6)

ggplot(FamilyFinalData, aes(x=Duration, fill=BasicNeeds)) + 
  geom_density(alpha=.6)

ggplot(FamilyFinalData, aes(x=Duration, fill=FSC)) + 
  geom_density(alpha=.6)

