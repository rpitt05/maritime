#Biostats project RDA
#Rachel Pitt
#12/6/25


#Library
library(tidyverse)
library(vegan)

#Data taken from all the other biostats project R files

mush_mat<-mushroom_presence_absence%>%
  group_by(site, plot_number)%>%
  summarise(across(-survey, max))%>%
  ungroup()%>%
  select(-site,-plot_number)
plant_mat<-plant_presence_absence%>%
  select(-site,-plot_number)
envi_df<-envi%>%
  select(-site, -plot_number)%>%
  mutate(across(everything(), as.numeric))



#mush dbRDA predicted by envi-----------------
mush_dbrda<-capscale(
  mush_mat~VG+LN+PO+CC+TD+ND,
  data=envi_df,
  distance="jaccard"
)

anova(mush_dbrda)
anova(mush_dbrda, by="term")
summary(mush_dbrda)
plot(mush_dbrda)

#Check dbRDA significance
anova(mush_dbrda, permutations=9999)
anova(mush_dbrda, by="axis", permutations=9999)

#Visualize dbRDA
plot(mush_dbrda, display=c("sites","dp"), scaling=2)


#mush dbRDA predicted by plants----------------
mush_vs_plant_dbrda<-capscale(
  mush_mat~.,
  data=plant_mat,
  distance="jaccard"
)

#Remove rare/redundant species
plant_mat_filtered <- plant_mat[, colSums(plant_mat) > 2]%>%
  select(-loblolly,-holly)

mush_vs_plant_dbrda<-capscale(
  mush_mat~.,
  data=plant_mat_filtered,
  distance="jaccard", permutations=9999
)

anova(mush_vs_plant_dbrda)
anova(mush_vs_plant_dbrda, by="term")
summary(mush_vs_plant_dbrda)
plot(mush_vs_plant_dbrda)

#Check dbRDA significance
anova(mush_vs_plant_dbrda, permutations=9999)
anova(mush_vs_plant_dbrda, by="axis", permutations=9999)

#Visualize dbRDA
plot(mush_vs_plant_dbrda, display=c("sites","dp"), scaling=2)


