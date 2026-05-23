#Simplified PCA?
#Rachel Pitt
#11/3/25


#For this group of variables lets do:
#organic content 10cm, canopy coverage, needle depth, tree density, total veg species richness, 10 cm soil salinity
all_variables<-organic_content %>%
  mutate(canopy_coverage=canopy_coverage$canopy_coverage_avg,
         needle_depth=needle_depth$needle_depth_avg,
         organic_layer_depth=organic_layer_depth$organic_layer_avg,
         tree_density=tree_density$tree_density) %>%
  left_join(total_species_richness, by=location)%>%
  select(-percent_organic_matter_25cm)


pc<-prcomp(all_variables %>%
             select(-site, -plot, -plot_number) %>%
             mutate(across(everything(), as.numeric)),
           center=TRUE,
           scale.=TRUE)

print(pc)

ggbiplot(pc,
         obs.scale=1.5,
         var.scale=1,
         groups=all_variables$site,
         ellipse=TRUE,
         circle=TRUE,
         ellipse.prob=0.68
) 

pc_scores<-as.data.frame(pc$x)
pc_data<-cbind(most_everything, pc_scores)
