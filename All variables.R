#Rachel Pitt
#10/28/25-10/30/25
#This is how I combined all my data to make a master datasheet of all variables

#Library
library(tidyverse)
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


#Ok now we can deal with the soil data- although how do I deal with this since measurments occured on each survey day?
soil<-read.csv("new_soil_v2.csv")%>%
  mutate(across(all_of(location),as.character))

percent_water_content<-soil %>%
  mutate(wet_soil_weight_g=(combined_wet_weight_g-beaker_weight_g),
        dry_soil_weight_g=(combined_dry_weight_g-beaker_weight_g),
        water_volume_mL=(wet_soil_weight_g-dry_soil_weight_g),
        percent_water_content=(water_volume_mL/wet_soil_weight_g)*100)


soil_salinity<-percent_water_content%>%
  mutate(soil_salinity_ppt=(rehydrated_water_volume_mL*supernatent_salinity_ppt)/water_volume_mL)

LOI<-read.csv("LOI_v1.csv")%>%
  mutate(across(all_of(location),as.character))

LOI_calcs<-LOI%>%
  mutate(wet_weight=combined_wet_weight_g-pan_weight_g,
         dry_weight=combined_dry_weight_g-pan_weight_g,
         burnt_weight=LOI_weight_g-pan_weight_g,
         water_content=wet_weight-dry_weight,
         organic_weight=dry_weight-burnt_weight,
         percent_organic_matter=organic_weight/dry_weight*100)

organic_content_10cm<- LOI_calcs %>%
  group_by(soil_depth_cm, site, plot, plot_number)%>%
  summarise(percent_organic_matter_avg=mean(percent_organic_matter))%>%
  ungroup()%>%
  filter(soil_depth_cm=="10")

organic_content_25cm<- LOI_calcs %>%
  group_by(soil_depth_cm, site, plot, plot_number)%>%
  summarise(percent_organic_matter_avg=mean(percent_organic_matter))%>%
  ungroup()%>%
  filter(soil_depth_cm=="25")

  
organic_content<-data.frame(
  site=organic_content_10cm$site,
  plot=organic_content_10cm$plot,
  plot_number=organic_content_10cm$plot_number,
  percent_organic_matter_10cm=organic_content_10cm$percent_organic_matter_avg,
  percent_organic_matter_25cm=organic_content_25cm$percent_organic_matter_avg
)


#Time for mushrooms using data downloaded from the inat project: maritime-forest-macrofungi-chincoteague-nwr
mushroom_inat_data<-read.csv("mushroom_data_v1.csv")%>%
  separate(field.field.site.id, into=c("survey", "site", "plot"), sep="_")%>%
  mutate(half_id=paste(site, plot, sep="_"))%>%
  left_join(data.frame(half_id=c("1_1","1_2","1_3","2_1","2_2","2_3","3_1","3_2","3_3","4_1","4_2","4_3","5_1","5_2","5_3","6_1","6_2","6_3"),
                       plot_number=as.character(1:18)), by="half_id")%>%
  mutate(across(all_of(location),as.character))



mushroom_richness_overall<-mushroom_inat_data%>%
  group_by(site, plot, plot_number)%>%
  summarise(mushroom_richness_overall=n_distinct(scientific_name))%>%
  ungroup()

mushroom_richness_survey<-mushroom_inat_data%>%
  group_by(survey,site,plot,plot_number)%>%
  summarise(mushroom_richness=n_distinct(scientific_name))%>%
  ungroup()%>%
  pivot_wider(
    names_from=survey,
    values_from=mushroom_richness,
    names_prefix="survey_mushroom_richness_"
  )

mushroom_richness<-mushroom_richness_overall%>%
  left_join(mushroom_richness_survey, by=location)%>%
  mutate(across(everything(), ~replace_na(.x,0)))



#Ok now we need to make a master sheet just consisting of the relevant data but don't include percent water content and soil salinity for now since
#you measured that each survey day?

all_variables<-organic_content %>%
  mutate(canopy_coverage=canopy_coverage$canopy_coverage_avg,
         needle_depth=needle_depth$needle_depth_avg,
         organic_layer_depth=organic_layer_depth$organic_layer_avg,
         tree_density=tree_density$tree_density,
         tree_species_richness=tree_species_richness$species_richness) %>%
  left_join(ground_vegetation_ratio, by=location)%>%
  left_join(needle_leaf_ratio, by=location)%>%
  left_join(strata_species_richness, by=location)


