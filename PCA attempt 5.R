#PCA attempt 5
#10/31/25
#Rachel Pitt


#I think calculating mushroom species richness by site would be more accurate than by plot because
#on surveys I had a tendency to collect outside of the plot boundaries and so the species richness is
#less applicable per plot but rather by the site as a whole. 

library(tidyverse)
library(ggbiplot)

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
  group_by(survey,site)%>%
  summarise(mushroom_richness=n_distinct(scientific_name))%>%
  ungroup()




#Join the data
complete_everything<-soil10cm%>%
  left_join(mushroom_richness, by=c("survey","site"))%>%
  left_join(all_variables, by=location)%>%
  mutate(across(everything(), ~replace_na(.x, 0)))



#Graph:
pc<-prcomp(complete_everything %>%
             select(-survey, -site, -plot, -plot_number, -mushroom_richness) %>%
             mutate(across(everything(), as.numeric)),
           center=TRUE,
           scale.=TRUE)

print(pc)

ggbiplot(pc,
         obs.scale=1,
         var.scale=1,
         groups=complete_everything$survey,
         ellipse=TRUE,
         circle=TRUE,
         ellipse.prob=0.68
) 

pca_scores<-as.data.frame(pc$x)
pca_data<-cbind(complete_everything, pca_scores)

pc_data <- bind_cols(complete_everything, as.data.frame(pc$x))


cor_results <- pc_data %>%
  select(starts_with("PC"), mushroom_richness) %>%
  summarise(across(starts_with("PC"), ~ cor(.x, mushroom_richness, use = "complete.obs"))) %>%
  pivot_longer(everything(), names_to = "PC", values_to = "correlation") %>%
  arrange(desc(abs(correlation)))

cor_results


