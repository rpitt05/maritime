#biostats project procustes analyses to compare mushroom and plant communities
#Rachel Pitt
#12/4/25


#Library
library(tidyverse)
library(vegan)

#Data taken from biostats project mush and plant pcoa and biostats project variables


plant_pcoa_1 <- cmdscale(plant_dist_1, k = 1)
mush_pcoa_1 <- cmdscale(mush_dist_1, k = 1)

plant_pcoa_1 <- cmdscale(plant_dist_1, k = 1)
mush_pcoa_1 <- cmdscale(mush_dist_1, k = 1)

plant_pcoa_3 <- cmdscale(plant_dist_3, k = 3)
mush_pcoa_3<- cmdscale(mush_dist_3, k = 3)

plant_pcoa_4 <- cmdscale(plant_dist_4, k = 3)
mush_pcoa_4 <- cmdscale(mush_dist_4, k = 3)

plant_pcoa_5 <- cmdscale(plant_dist_5, k = 3)
mush_pcoa_5 <- cmdscale(mush_dist_5, k = 3)

plant_pcoa_6 <- cmdscale(plant_dist_6, k = 3)
mush_pcoa_6 <- cmdscale(mush_dist_6, k = 3)

plant_pcoa_7 <- cmdscale(plant_dist_7, k = 3)
mush_pcoa_7 <- cmdscale(mush_dist_7, k = 3)

plant_pcoa_overall <- cmdscale(plant_dist, k = 3)
mush_pcoa_overall <- cmdscale(mush_dist_overall, k = 3)

protest(mush_pcoa_3, plant_pcoa_3, permutations = 9999)
protest(mush_pcoa_4, plant_pcoa_4, permutations = 9999)
protest(mush_pcoa_5, plant_pcoa_5, permutations = 9999)
protest(mush_pcoa_6, plant_pcoa_6, permutations = 9999)
protest(mush_pcoa_7, plant_pcoa_7, permutations = 9999)
protest(mush_pcoa_overall, plant_pcoa_overall, permutations = 9999)
