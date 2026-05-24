#Biostats project variation partitioning
#Rachel Pitt
#12/6/25



#Library
library(tidyverse)
library(vegan)

#Data taken from all other biostats project R files


varpart(
  Y=mush_mat,
  X=plant_mat_filtered,
  W=envi_df[,c("VG","LN","PO","CC","TD","ND")],
  Z=as.data.frame(coords[,c("lat","lon")])
)

plant_only<-capscale(
  mush_mat~ . + Condition(VG+LN+PO+CC+TD+ND),
  data=cbind(plant_mat_filtered, envi_df[,c("VG","LN","PO","CC","TD","ND")]),
  distance="jaccard"
)
anova(plant_only, permutations=9999)

#TO BE CONTINUED
