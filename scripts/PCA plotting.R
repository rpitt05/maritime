#Rachel Pitt
#10/30/25
#Plotting against the successful PCA (from attempt 3)


#Library
library(tidyverse)
library(ggbiplot)


ggplot(pca_data, aes(PC16, mushroom_richness, color=site))+geom_point()+geom_smooth(method="lm")

ggplot(pca_data, aes(PC16, mushroom_richness))+geom_point()+geom_smooth(method="lm")


#Surveys 4-7
ggplot(most_everything, aes(tree_species_richness, tree_density))+geom_point()+
  geom_smooth(method="lm")+
  labs(x="Tree Species Richness", y="Mushroom Richness")+
  theme_classic()

ggplot(most_everything, aes(organic_layer_depth, mushroom_richness, fill=site))+geom_point()+geom_smooth(method="lm")+labs(title="surveys 4-7 by site")

#All surveys (1-7)
ggplot(complete_everything, aes(strata_veg_species_richness_3, mushroom_richness))+geom_point()+geom_smooth(method="lm")+labs(title="surveys 1-7")

ggplot(complete_everything, aes(percent_organic_matter_10cm, mushroom_richness, fill=site))+geom_point()+geom_smooth(method="lm")+labs(title="surveys 1-7 by site")





ggplot(complete_everything, aes(site, organic_layer_depth))+geom_boxplot()


pc$rotation

pc


#Oh shit when i merged spreadsheets we didn't get mushroom richess as 0 and soil is probably messed
#up bc for 25cm we didn't measure the first 3 surveys