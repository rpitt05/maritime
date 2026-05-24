#PCA attempt 6 minus first three survey days
#11/1/25
#Rachel Pitt


#Library
library(tidyverse)
library(ggbiplot)
location<-c("site","plot","plot_number")

#Lets import all the data that is pre structured by site/plot and calculate avgs if appropriate
canopy_coverage<-read.csv("canopy_coverage_v1.csv")%>%
  mutate(canopy_coverage_avg=(canopy_coverage_1+canopy_coverage_2+canopy_coverage_3+canopy_coverage_4)/4) %>%
  mutate(across(all_of(location),as.character))

ground_vegetation_ratio<-read.csv("ground_vegetation_ratio_v1.csv")%>%
  select(-ground_cover)%>%
  mutate(across(all_of(location),as.character))

needle_depth<-read.csv("needle_depth_v1.csv")%>%
  mutate(needle_depth_avg=(needle_depth_1+needle_depth_2+needle_depth_3+needle_depth_4)/4)%>%
  mutate(across(all_of(location),as.character))

needle_leaf_ratio<-read.csv("needle_leaf_ratio_v1.csv")%>%
  select(-leaf_coverage)%>%
  mutate(across(all_of(location),as.character))


organic_layer_depth<-read.csv("organic_layer_depth_v1.csv")%>%
  mutate(organic_layer_avg=(organic_layer_1+organic_layer_2+organic_layer_3)/3)%>%
  mutate(across(all_of(location),as.character))

#Now lets deal with the tree data
tree_counts<-read.csv("tree_counts_v1.csv")%>%
  mutate(across(all_of(location),as.character))

tree_density<-tree_counts%>%
  mutate(tree_count=(loblolly_count+cherry_count+water_oak_count+maple_count+sweetgum_count+sassafras_count+holly_count+willow_oak_count),
         tree_density=tree_count/100)

tree_species_richness<-tree_counts%>%
  mutate(species_richness=(rowSums(tree_counts!=0)-3))

#Now lets deal with the vegetation data
strata_vegetation<-read.csv("strata_vegetation_v1.csv")%>%
  mutate(across(all_of(location),as.character))

strata_species_richness<-strata_vegetation%>%
  group_by(strata, site, plot, plot_number)%>%
  summarise(strata_species_richness=n_distinct(species))%>%
  ungroup()%>%
  pivot_wider(
    names_from=strata,
    values_from=strata_species_richness,
    names_prefix="strata_veg_species_richness_"
  )



#soil
soil_raw<-read.csv("new_soil_v2.csv")%>%
  mutate(across(all_of(location),as.character))

soil_calcs<-soil_raw %>%
  mutate(wet_soil_weight_g=(combined_wet_weight_g-beaker_weight_g),
         dry_soil_weight_g=(combined_dry_weight_g-beaker_weight_g),
         water_volume_mL=(wet_soil_weight_g-dry_soil_weight_g),
         percent_water_content=(water_volume_mL/wet_soil_weight_g)*100,
         soil_salinity_ppt=(rehydrated_water_volume_mL*supernatent_salinity_ppt/water_volume_mL))


soil10cm<-data.frame(
  survey=as.character(soil_calcs$survey),
  site=soil_calcs$site,
  plot=soil_calcs$plot,
  plot_number=soil_calcs$plot_number,
  depth=soil_calcs$soil_depth_cm,
  soil_percent_water_10cm=soil_calcs$percent_water_content,
  soil_salinity_ppt_10cm=soil_calcs$soil_salinity_ppt
)%>%
  filter(depth=="10")%>%
  select(-depth)

soil25cm<-data.frame(
  survey=as.character(soil_calcs$survey),
  site=soil_calcs$site,
  plot=soil_calcs$plot,
  plot_number=soil_calcs$plot_number,
  depth=soil_calcs$soil_depth_cm,
  soil_percent_water_25cm=soil_calcs$percent_water_content,
  soil_salinity_ppt_25cm=soil_calcs$soil_salinity_ppt
)%>%
  filter(depth=="25")%>%
  select(-depth)


#mushrooms
mushroom_inat_data<-read.csv("mushroom_data_v1.csv")%>%
  separate(field.field.site.id, into=c("survey", "site", "plot"), sep="_")%>%
  mutate(half_id=paste(site, plot, sep="_"))%>%
  left_join(data.frame(half_id=c("1_1","1_2","1_3","2_1","2_2","2_3","3_1","3_2","3_3","4_1","4_2","4_3","5_1","5_2","5_3","6_1","6_2","6_3"),
                       plot_number=as.character(1:18)), by="half_id")%>%
  mutate(across(all_of(location),as.character))

mushroom_data_clean<-data.frame(
  site=mushroom_inat_data$site,
  plot=mushroom_inat_data$plot,
  plot_number=mushroom_inat_data$plot_number,
  survey=mushroom_inat_data$survey,
  species=mushroom_inat_data$scientific_name
)

mushroom_richness<-mushroom_inat_data%>%
  group_by(survey,site,plot_number)%>%
  summarise(mushroom_richness=n_distinct(scientific_name))%>%
  ungroup()





all_variables<-organic_content %>%
  mutate(canopy_coverage=canopy_coverage$canopy_coverage_avg,
         needle_depth=needle_depth$needle_depth_avg,
         organic_layer_depth=organic_layer_depth$organic_layer_avg,
         tree_density=tree_density$tree_density,
         tree_species_richness=tree_species_richness$species_richness) %>%
  left_join(ground_vegetation_ratio, by=location)%>%
  left_join(needle_leaf_ratio, by=location)%>%
  left_join(strata_species_richness, by=location)



#Join the data
most_everything<-soil10cm%>%
  left_join(soil25cm, by=c("survey","site","plot","plot_number"))%>%
  left_join(mushroom_richness, by=c("survey","site", "plot_number"))%>%
  left_join(all_variables, by=location)%>%
  mutate(across(everything(), ~replace_na(.x, 0)))



#Graph:
pc<-prcomp(most_everything %>%
             select(-survey, -site, -plot, -plot_number, -mushroom_richness) %>%
             mutate(across(everything(), as.numeric)),
           center=TRUE,
           scale.=TRUE)

print(pc)

ggbiplot(pc,
         obs.scale=1.5,
         var.scale=1,
         groups=most_everything$site,
         ellipse=TRUE,
         circle=TRUE,
         ellipse.prob=0.68
) 

pc_scores<-as.data.frame(pc$x)
pc_data<-cbind(most_everything, pc_scores)


#Chat GPT code:

pc_data <- bind_cols(most_everything, as.data.frame(pc$x))

  
cor_results <- pc_data %>%
  select(starts_with("PC"), mushroom_richness) %>%
  summarise(across(starts_with("PC"), ~ cor(.x, mushroom_richness, use = "complete.obs"))) %>%
  pivot_longer(everything(), names_to = "PC", values_to = "correlation") %>%
  arrange(desc(abs(correlation)))

cor_results
