ggplot(FamilyFinalData, aes(x=CloseTimes, fill=Housing,color=Housing)) + 
  geom_density(alpha=.6)

ggplot(FamilyFinalData, aes(x=CloseTimes, fill=BasicNeeds)) + 
  geom_density(alpha=.6)

ggplot(FamilyFinalData, aes(x=CloseTimes, fill=FSC)) + 
  geom_density(alpha=.6)

ggplot(FamilyFinalData, aes(x=Duration, fill=Housing,color=Housing)) + 
  geom_density(alpha=.6)+
  guides(color=FALSE)+
  ggtitle("Duration and Housing")+
  scale_fill_discrete(name = "Housing",labels = c("Post-CYF Servie and NA", "Pre-CYF Service"))

ggplot(FamilyFinalData, aes(x=Duration, fill=BasicNeeds,color=BasicNeeds)) + 
  geom_density(alpha=.6)+
  guides(color=FALSE)+
  ggtitle("Duration and Basic Needs")+
  scale_fill_discrete(name = "Basic Needs",labels = c("Post-CYF Servie and NA", "Pre-CYF Service"))

ggplot(FamilyFinalData, aes(x=Duration, fill=FSC,color=FSC)) + 
  geom_density(alpha=.6)+
  guides(color=FALSE)+
  ggtitle("Duration and FSC")+
  scale_fill_discrete(name = "FSC",labels = c("Post-CYF Servie and NA", "Pre-CYF Service"))