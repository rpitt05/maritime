#Biostats project graphing dist matrices
#Rachel Pitt
#12/5/25


#Library
library(tidyverse)
library(vegan)

#Data taken from all other biostats project R files.


#Goal:Attempting to visualize similarities in community structure across diff
#sampling days for mushrooms and to compare mushroom and plant structure

mush_vec_3<-as.vector(mush_dist_3)
mush_vec_4<-as.vector(mush_dist_4)
mush_vec_5<-as.vector(mush_dist_5)
mush_vec_6<-as.vector(mush_dist_6)
mush_vec_7<-as.vector(mush_dist_7)
mush_vec_overall<-as.vector(mush_dist_overall)
plant_vec_3<-as.vector(plant_dist_3)
plant_vec_4<-as.vector(plant_dist_4)
plant_vec_5<-as.vector(plant_dist_5)
plant_vec_6<-as.vector(plant_dist_6)
plant_vec_7<-as.vector(plant_dist_7)
plant_vec_overall<-as.vector(plant_dist)


#Compare vectors
plot(plant_vec_3, mush_vec_3)
abline(lm(mush_vec_3~plant_vec_3), lwd=2)
plot(plant_vec_4, mush_vec_4)
abline(lm(mush_vec_4~plant_vec_4), lwd=2)
plot(plant_vec_5, mush_vec_5)
abline(lm(mush_vec_5~plant_vec_5), lwd=2)
plot(plant_vec_6, mush_vec_6)
abline(lm(mush_vec_6~plant_vec_6), lwd=2)
plot(plant_vec_7, mush_vec_7)
abline(lm(mush_vec_7~plant_vec_7), lwd=2)
plot(plant_vec_overall, mush_vec_overall,
     xlab= "Plant distance",
     ylab="Mushroom distance")
abline(lm(mush_vec_overall~plant_vec_overall), lwd=2)

cor(plant_vec_overall, mush_vec_overall)
