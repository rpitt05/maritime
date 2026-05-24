#Biostats project comparing mushroom and plant dist matrix
#Rachel Pitt
#12/4/25


#Library
library(tidyverse)
library(vegan)

#Data is taken from biostats project plant pcoa and mushroom pcoa and biostats project variables

#Subset mush and plant into 7 dist matrices due to 7 surveys
family_mat_1<-mushroom_presence_absence%>%
  filter(survey=="1")%>%
  select(-site,-plot_number, -survey)
mush_dist_1<-vegdist(family_mat_1, method="jaccard", binary=TRUE)
plant_mat_1<-mushroom_presence_absence%>%
  filter(survey=="1")%>%
  left_join(plant_presence_absence)%>%
  select(-ends_with("eae"))%>%
  select(-site,-plot_number, -survey)
plant_dist_1<-vegdist(plant_mat_1, method="jaccard", binary=TRUE)


family_mat_2<-mushroom_presence_absence%>%
  filter(survey=="2")%>%
  select(-site,-plot_number, -survey)
mush_dist_2<-vegdist(family_mat_2, method="jaccard", binary=TRUE)
plant_mat_2<-mushroom_presence_absence%>%
  filter(survey=="2")%>%
  left_join(plant_presence_absence)%>%
  select(-ends_with("eae"))%>%
  select(-site,-plot_number, -survey)
plant_dist_2<-vegdist(plant_mat_2, method="jaccard", binary=TRUE)


family_mat_3<-mushroom_presence_absence%>%
  filter(survey=="3")%>%
  select(-site,-plot_number, -survey)
mush_dist_3<-vegdist(family_mat_3, method="jaccard", binary=TRUE)
plant_mat_3<-mushroom_presence_absence%>%
  filter(survey=="3")%>%
  left_join(plant_presence_absence)%>%
  select(-ends_with("eae"))%>%
  select(-site,-plot_number, -survey)
plant_dist_3<-vegdist(plant_mat_3, method="jaccard", binary=TRUE)


family_mat_4<-mushroom_presence_absence%>%
  filter(survey=="4")%>%
  select(-site,-plot_number, -survey)
mush_dist_4<-vegdist(family_mat_4, method="jaccard", binary=TRUE)
plant_mat_4<-mushroom_presence_absence%>%
  filter(survey=="4")%>%
  left_join(plant_presence_absence)%>%
  select(-ends_with("eae"))%>%
  select(-site,-plot_number, -survey)
plant_dist_4<-vegdist(plant_mat_4, method="jaccard", binary=TRUE)


family_mat_5<-mushroom_presence_absence%>%
  filter(survey=="5")%>%
  select(-site,-plot_number, -survey)
mush_dist_5<-vegdist(family_mat_5, method="jaccard", binary=TRUE)
plant_mat_5<-mushroom_presence_absence%>%
  filter(survey=="5")%>%
  left_join(plant_presence_absence)%>%
  select(-ends_with("eae"))%>%
  select(-site,-plot_number, -survey)
plant_dist_5<-vegdist(plant_mat_5, method="jaccard", binary=TRUE)


family_mat_6<-mushroom_presence_absence%>%
  filter(survey=="6")%>%
  select(-site,-plot_number, -survey)
mush_dist_6<-vegdist(family_mat_6, method="jaccard", binary=TRUE)
plant_mat_6<-mushroom_presence_absence%>%
  filter(survey=="6")%>%
  left_join(plant_presence_absence)%>%
  select(-ends_with("eae"))%>%
  select(-site,-plot_number, -survey)
plant_dist_6<-vegdist(plant_mat_6, method="jaccard", binary=TRUE)


family_mat_7<-mushroom_presence_absence%>%
  filter(survey=="7")%>%
  select(-site,-plot_number, -survey)
mush_dist_7<-vegdist(family_mat_7, method="jaccard", binary=TRUE)
plant_mat_7<-mushroom_presence_absence%>%
  filter(survey=="7")%>%
  left_join(plant_presence_absence)%>%
  select(-ends_with("eae"))%>%
  select(-site,-plot_number, -survey)
plant_dist_7<-vegdist(plant_mat_7, method="jaccard", binary=TRUE)


family_mat_overall<-mushroom_presence_absence%>%
  group_by(site, plot_number)%>%
  summarise(across(-survey, max))%>%
  ungroup()%>%
  select(-site,-plot_number)
mush_dist_overall<-vegdist(family_mat_overall, method="jaccard", binary=TRUE)


#Mantel test:
mantel(mush_dist_3,plant_dist_3, method="pearson", permutations=9999)
mantel(mush_dist_4,plant_dist_4, method="pearson", permutations=9999)
mantel(mush_dist_5,plant_dist_5, method="pearson", permutations=9999)
mantel(mush_dist_6,plant_dist_6, method="pearson", permutations=9999)
mantel(mush_dist_7,plant_dist_7, method="pearson", permutations=9999)
mantel(mush_dist_overall,plant_dist, method="pearson", permutations=9999)


#Ohhh what if you just include the supposed ectomycorrhizal families and see if
#those one group with plants better? But on other hand saprobes may group with certain ones
#if they break down certain materials
#Regardless would be good to separate bc maybe they have distinct groupings with plants