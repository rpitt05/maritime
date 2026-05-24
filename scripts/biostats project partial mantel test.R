#Biostats project partial mantel test
#Rachel Pit
#12/6/25



#Library
library(tidyverse)
library(vegan)
library(geosphere)

#Data taken from all the other biostats project R files
#Organize the data we are controlling for into a distance matrix
GPS<-read.csv("GPS_points.csv")

#Geo data into dist mat
coords<-as.matrix(GPS[,c("lon","lat")])
geo_m<-distm(coords, fun=distHaversine)
geo_dist<-as.dist(geo_m)

#Mantel test with geo data
mantel.partial(mush_dist_overall,plant_dist,geo_dist, permutations=9999)


#Environmental data to control for and turn into a distance matrix
envi<-all%>%
  rename(
    VG=vegetation_cover,
    LN=needle_coverage,
    PO=percent_organic_matter_10cm,
    CC=canopy_coverage,
    TD=tree_density,
    ND=needle_depth
  )%>%
  filter(survey==4)%>%
  select(site, plot_number, VG, LN, PO, CC, TD, ND)
#Check for colinearity
cor(envi%>%select(VG, LN, PO, CC, TD, ND),
             use="pairwise.complete.obs")

#PCA on scaled variables and determine how many PCs to include
envi_pca<-prcomp(envi%>%
                   select(-site, -plot_number)%>%
                   mutate(across(everything(), as.numeric)),
                   center=TRUE, scale.=TRUE)

envi_pc_scores<-as.data.frame(envi_pca$x)

#Create envi distance matrix
envi_pca_dist<-dist(envi_pc_scores, method="euclidean")


#Partial mantel test
mantel(mush_dist_overall, envi_pca_dist, permutations=9999)
mantel.partial(mush_dist_overall, envi_pca_dist,geo_dist, permutations=9999)


plot(envi_pca_dist, mush_vec_overall,
     xlab= "Envi distance",
     ylab="Mushroom distance")
abline(lm(mush_vec_overall~envi_pca_dist), lwd=2)

cor(envi_pca_dist, mush_vec_overall)

