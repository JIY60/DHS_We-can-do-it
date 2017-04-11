ggplot(FamilyFinalData, aes(x=CloseTimes, fill=Housing,color=Housing)) + 
  geom_density(alpha=.6)

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
