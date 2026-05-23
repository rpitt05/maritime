#Alternate PCA plotting
#Rachel Pitt
#11/1/25

#I want to find a way to graph mushroom richness against variable I measured but for the ones that were measured
#once and don't vary per survey it means you can't exactly see trends when you plot them against
#each other so I'm going to make columns of mushroom richness per survey and create graphs of those
#vs each variable to hopefully uncover patterns

#I also want to consider calculating the mushroom richness vs plot again rather than per site?


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

total_veg_species_richness<-strata_vegetation%>%
  group_by(site, plot, plot_number)%>%
  summarise(veg_species_richness=n_distinct(species))%>%
  ungroup()
  

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

#clean up mushroom data
mushroom_data_clean<-data.frame(
  site=mushroom_inat_data$site,
  plot=mushroom_inat_data$plot,
  plot_number=mushroom_inat_data$plot_number,
  survey=mushroom_inat_data$survey,
  species=mushroom_inat_data$scientific_name
)%>%
  mutate(days_since_first_survey=recode(survey,
                                        `1`=1,
                                        `2`=7,
                                        `3`=13,
                                        `4`=20,
                                        `5`=30,
                                        `6`=35,
                                        `7`=83))

#calc mushroom richness overall by plot
plot_mushroom_richness_overall<-mushroom_data_clean%>%
  group_by(site,plot_number)%>%
  summarise(plot_mushroom_richness_overall=n_distinct(species))%>%
  ungroup()

#calc mushroom richness per survey day by plot
plot_mushroom_richness_survey<-mushroom_data_clean%>%
  group_by(survey,site, plot_number)%>%
  summarise(plot_mushroom_richness=n_distinct(species))%>%
  ungroup()%>%
  pivot_wider(
    names_from=survey,
    values_from=plot_mushroom_richness,
    names_prefix="plot_survey_mushroom_richness_"
  )

#join the overall richness by plot to each of the survey days
plot_mushroom_richness<-plot_mushroom_richness_overall%>%
  left_join(plot_mushroom_richness_survey, by=c("site","plot_number"))%>%
  mutate(across(everything(), ~replace_na(.x,0)))

#Same as above but by site rather than by plot as this one ay be more representative given the way
#mushroomda ta colelction was handeled

site_mushroom_richness_overall<-mushroom_data_clean%>%
  group_by(site)%>%
  summarise(site_mushroom_richness_overall=n_distinct(species))%>%
  ungroup()

site_mushroom_richness_survey<-mushroom_data_clean%>%
  group_by(site, survey)%>%
  summarise(site_mushroom_richness=n_distinct(species))%>%
  ungroup()%>%
  pivot_wider(
    names_from=survey,
    values_from=site_mushroom_richness,
    names_prefix="site_survey_mushroom_richness_"
  )

site_mushroom_richness<-site_mushroom_richness_overall%>%
  left_join(site_mushroom_richness_survey, by=c("site"))%>%
  mutate(across(everything(), ~replace_na(.x,0)))

mushroom_richness<-plot_mushroom_richness%>%
  left_join(site_mushroom_richness, by="site")



#Join constant data-------------------------
alt_all_vars<-organic_content%>%
  mutate(canopy_coverage=canopy_coverage$canopy_coverage_avg,
         needle_depth=needle_depth$needle_depth_avg,
         organic_layer_depth=organic_layer_depth$organic_layer_avg,
         tree_density=tree_density$tree_density,
         tree_species_richness=tree_species_richness$species_richness) %>%
  left_join(ground_vegetation_ratio, by=location)%>%
  left_join(needle_leaf_ratio, by=location)%>%
  left_join(strata_species_richness, by=location)%>%
  left_join(total_veg_species_richness, by=location)
  


#Join data for survey 1
survey_1<-data.frame(
  site=mushroom_richness$site,
  plot_number=mushroom_richness$plot_number,
  plot_mushroom_richness_survey=mushroom_richness$plot_survey_mushroom_richness_1,
  site_mushroom_richness_survey=mushroom_richness$site_survey_mushroom_richness_1
)%>%
  left_join(alt_all_vars, by=c("site","plot_number"))%>%
  left_join(filter(soil10cm, survey=="1"), by=c("site","plot","plot_number"))%>%
  select(-survey)



#Join data for survey 2
survey_2<-data.frame(
  site=mushroom_richness$site,
  plot_number=mushroom_richness$plot_number,
  plot_mushroom_richness_survey=mushroom_richness$plot_survey_mushroom_richness_2,
  site_mushroom_richness_survey=mushroom_richness$site_survey_mushroom_richness_2
)%>%
  left_join(alt_all_vars, by=c("site","plot_number"))%>%
  left_join(filter(soil10cm, survey=="2"), by=c("site","plot","plot_number"))%>%
  select(-survey)


#Join data for survey 3
survey_3<-data.frame(
  site=mushroom_richness$site,
  plot_number=mushroom_richness$plot_number,
  plot_mushroom_richness_survey=mushroom_richness$plot_survey_mushroom_richness_3,
  site_mushroom_richness_survey=mushroom_richness$site_survey_mushroom_richness_3
)%>%
  left_join(alt_all_vars, by=c("site","plot_number"))%>%
  left_join(filter(soil10cm, survey=="3"), by=c("site","plot","plot_number"))%>%
  select(-survey)


#Join data for survey 4
survey_4<-data.frame(
  site=mushroom_richness$site,
  plot_number=mushroom_richness$plot_number,
  plot_mushroom_richness_survey=mushroom_richness$plot_survey_mushroom_richness_4,
  site_mushroom_richness_survey=mushroom_richness$site_survey_mushroom_richness_4
)%>%
  left_join(alt_all_vars, by=c("site","plot_number"))%>%
  left_join(filter(soil10cm, survey=="4"), by=c("site","plot","plot_number"))%>%
  select(-survey)%>%
  left_join(filter(soil25cm, survey=="4"), by=c("site","plot","plot_number"))%>%
  select(-survey)

#Join data for survey 5
survey_5<-data.frame(
  site=mushroom_richness$site,
  plot_number=mushroom_richness$plot_number,
  plot_mushroom_richness_survey=mushroom_richness$plot_survey_mushroom_richness_5,
  site_mushroom_richness_survey=mushroom_richness$site_survey_mushroom_richness_5
)%>%
  left_join(alt_all_vars, by=c("site","plot_number"))%>%
  left_join(filter(soil10cm, survey=="5"), by=c("site","plot","plot_number"))%>%
  select(-survey)%>%
  left_join(filter(soil25cm, survey=="5"), by=c("site","plot","plot_number"))%>%
  select(-survey)

#Join data for survey 6
survey_6<-data.frame(
  site=mushroom_richness$site,
  plot_number=mushroom_richness$plot_number,
  plot_mushroom_richness_survey=mushroom_richness$plot_survey_mushroom_richness_6,
  site_mushroom_richness_survey=mushroom_richness$site_survey_mushroom_richness_6
)%>%
  left_join(alt_all_vars, by=c("site","plot_number"))%>%
  left_join(filter(soil10cm, survey=="6"), by=c("site","plot","plot_number"))%>%
  select(-survey)%>%
  left_join(filter(soil25cm, survey=="6"), by=c("site","plot","plot_number"))%>%
  select(-survey)

#Join data for survey 7
survey_7<-data.frame(
  site=mushroom_richness$site,
  plot_number=mushroom_richness$plot_number,
  plot_mushroom_richness_survey=mushroom_richness$plot_survey_mushroom_richness_7,
  site_mushroom_richness_survey=mushroom_richness$site_survey_mushroom_richness_7
)%>%
  left_join(alt_all_vars, by=c("site","plot_number"))%>%
  left_join(filter(soil10cm, survey=="7"), by=c("site","plot","plot_number"))%>%
  select(-survey)%>%
  left_join(filter(soil25cm, survey=="7"), by=c("site","plot","plot_number"))%>%
  select(-survey)


#Survey 1 graph
ggplot(survey_7, aes(strata_veg_species_richness_3, site_mushroom_richness_survey))+geom_point()+geom_smooth(method="lm")+
  labs(title="survey 7")


#Days since first survey-------------------------------------
ggplot(mushroom_richness, aes(days_since_first_survey, site_mushroom_richness_overall, color=days_since_first_survey))+geom_boxplot()



#mushrooms
mushroom_inat_data<-read.csv("mushroom_data_v1.csv")%>%
  separate(field.field.site.id, into=c("survey", "site", "plot"), sep="_")%>%
  mutate(half_id=paste(site, plot, sep="_"))%>%
  left_join(data.frame(half_id=c("1_1","1_2","1_3","2_1","2_2","2_3","3_1","3_2","3_3","4_1","4_2","4_3","5_1","5_2","5_3","6_1","6_2","6_3"),
                       plot_number=as.character(1:18)), by="half_id")%>%
  mutate(across(all_of(location),as.character))

#clean up mushroom data
mushroom_data_clean<-data.frame(
  site=mushroom_inat_data$site,
  plot=mushroom_inat_data$plot,
  plot_number=mushroom_inat_data$plot_number,
  survey=mushroom_inat_data$survey,
  species=mushroom_inat_data$scientific_name
)%>%
  mutate(days_since_first_survey=recode(survey,
                                        `1`=1,
                                        `2`=7,
                                        `3`=13,
                                        `4`=20,
                                        `5`=30,
                                        `6`=35,
                                        `7`=83))

mushroom_richness_kinda<-mushroom_data_clean%>%
  group_by(survey,site,plot_number, days_since_first_survey)%>%
  summarise(plot_mushroom_richness=n_distinct(species))%>%
  ungroup()%>%
  left_join(alt_all_vars)%>%
  mutate(across(everything(), ~replace_na(.x,0)))


ggplot(mushroom_richness_kinda, aes(days_since_first_survey,plot_mushroom_richness))+geom_point()+geom_smooth(method="lm")
  
ggplot(mushroom_richness_kinda, aes(veg_species_richness, plot_mushroom_richness))+geom_point()+geom_smooth(method="lm")+
  labs(title="surveys 1-7")

