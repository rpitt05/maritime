#PCA attempt 1
#Rachel Pitt
#10/30/25

#Library + helpful string
library(tidyverse)
library(ggbiplot)
location<-c("site","plot","plot_number")


#OMGOMG! lets make a pca!!!

#Ok get rid of mushroom data and location data (site, plot, plot_number)
pc<-prcomp(all_variables %>%
  select(-all_of(location),-contains("mushroom")) %>%
  mutate(across(everything(), as.numeric)),
  center=TRUE,
  scale.=TRUE)

print(pc)

plot<-ggbiplot(pc,
                obs.scale=1,
                var.scale=1,
                groups=all_variables$site,
                ellipse=TRUE,
                circle=TRUE,
                ellipse.prob=0.68
) 

plot

  