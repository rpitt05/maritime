#Ecological Modeling project cleaning data
#Rachel Pitt
#4/15/26

#Below code adapted from biostats project project variables R file

#Library
library(tidyverse)

#Data
location<-c("site","plot","plot_number")

#Fixed data:

#Canopy coverag was measured with 4 replicates per plot so this takes the average between all of them. Then converted to a proportion because measured as percentage.
canopy_coverage<-read.csv("canopy_coverage_v1.csv")%>%
  mutate(canopy_coverage_avg=(canopy_coverage_1+canopy_coverage_2+canopy_coverage_3+canopy_coverage_4)/4/100) %>%
  mutate(across(all_of(location),as.character))

#Ground cover was a measure of percent herbaceous cover compared to just the ground at each plot and now converted as a proportion
vegetation_cover<-read.csv("ground_vegetation_ratio_v1.csv")%>%
  select(-ground_cover)%>%
  mutate(across(all_of(location),as.character))%>%
  mutate(vegetation_cover=vegetation_cover/100)

#Needle depth measured with a ruller with 4 replicates per plot. This averages them together
needle_depth<-read.csv("needle_depth_v1.csv")%>%
  mutate(needle_depth_avg=(needle_depth_1+needle_depth_2+needle_depth_3+needle_depth_4)/4)%>%
  mutate(across(all_of(location),as.character))

#The ratio of needles to leaves on the ground at each plot was measured and is represented by the percentage of needle coverage which is then turned into a proportion
needle_coverage<-read.csv("needle_leaf_ratio_v1.csv")%>%
  select(-leaf_coverage)%>%
  mutate(across(all_of(location),as.character))%>%
  mutate(needle_coverage=needle_coverage/100)

#Organic layer depth was measured with a ruler by seeing how long until you hit straight up sand. It was taken in 3 replicates per plot and this takes the average
organic_layer_depth<-read.csv("organic_layer_depth_v1.csv")%>%
  mutate(organic_layer_depth_avg=(organic_layer_1+organic_layer_2+organic_layer_3)/3)%>%
  mutate(across(all_of(location),as.character))

#Number of each type of tree found per plot and also a column for overall number of trees per plot
tree_counts<-read.csv("tree_counts_v1.csv")%>%
  mutate(across(all_of(location),as.character))

#Calculate tree density per plot (which are each 100m^2 so just deviding by 100 gets density)
tree_density<-tree_counts%>%
  mutate(tree_count=(loblolly_count+cherry_count+water_oak_count+maple_count+sweetgum_count+sassafras_count+holly_count+willow_oak_count),
         tree_density=tree_count/100)

#Calculate tree species richness per plot (number of tree species per plot)
tree_species_richness<-tree_counts%>%
  mutate(species_richness=(rowSums(tree_counts!=0)-3))

#So at each plot plant species present were recorded at three stratas. The first strata is just the ground, sencond strata is mid level, and third strata is canopy
strata_vegetation<-read.csv("strata_vegetation_v1.csv")%>%
  mutate(across(all_of(location),as.character))

#This calculates plant species richness at each of the three stratas
strata_species_richness<-strata_vegetation%>%
  group_by(strata, site, plot, plot_number)%>%
  summarise(strata_species_richness=n_distinct(species))%>%
  ungroup()%>%
  pivot_wider(
    names_from=strata,
    values_from=strata_species_richness,
    names_prefix="strata_veg_species_richness_"
  )

#This calculates plant species richness overall ignoring strata
overall_veg_species_richness<-strata_vegetation%>%
  group_by(site, plot, plot_number)%>%
  summarise(overall_species_richness=n_distinct(species))%>%
  ungroup()


#Loss on ignition data from using a muffle furnace. This was done for each plot from two different survey days. 
LOI<-read.csv("LOI_v1.csv")%>%
  mutate(across(all_of(location),as.character))

#This actually calculates percent organic matter but I think I'll keep it as a proportion
LOI_calcs<-LOI%>%
  mutate(wet_weight=combined_wet_weight_g-pan_weight_g,
         dry_weight=combined_dry_weight_g-pan_weight_g,
         burnt_weight=LOI_weight_g-pan_weight_g,
         water_content=wet_weight-dry_weight,
         organic_weight=dry_weight-burnt_weight,
         percent_organic_matter=organic_weight/dry_weight)

#This just creates an average organic content from the two measurments and also filters out 10cm depth
organic_content_10cm<- LOI_calcs %>%
  group_by(soil_depth_cm, site, plot, plot_number)%>%
  summarise(percent_organic_matter_avg=mean(percent_organic_matter))%>%
  ungroup()%>%
  filter(soil_depth_cm=="10")

#This alsoc reates an average organic content form teh two measurments but filters out 25cm depth
organic_content_25cm<- LOI_calcs %>%
  group_by(soil_depth_cm, site, plot, plot_number)%>%
  summarise(percent_organic_matter_avg=mean(percent_organic_matter))%>%
  ungroup()%>%
  filter(soil_depth_cm=="25")

#This combines the two 10cm and 25cm measurments into one data fram
organic_content<-data.frame(
  site=organic_content_10cm$site,
  plot=organic_content_10cm$plot,
  plot_number=organic_content_10cm$plot_number,
  percent_organic_matter_10cm=organic_content_10cm$percent_organic_matter_avg,
  percent_organic_matter_25cm=organic_content_25cm$percent_organic_matter_avg
)

#Soil data:
#This is my raw soil salinity data that I measured per plot on each survey day. Every survey as 10cm but only 3-7 have 25cm depth
soil_raw<-read.csv("new_soil_v2.csv")%>%
  mutate(across(all_of(location),as.character))

#This calcualtes soil salinity and percent water content from supernatent salinity and the wet weight
soil_calcs<-soil_raw %>%
  mutate(wet_soil_weight_g=(combined_wet_weight_g-beaker_weight_g),
         dry_soil_weight_g=(combined_dry_weight_g-beaker_weight_g),
         water_volume_mL=(wet_soil_weight_g-dry_soil_weight_g),
         percent_water_content=(water_volume_mL/wet_soil_weight_g)*100,
         soil_salinity_ppt=(rehydrated_water_volume_mL*supernatent_salinity_ppt/water_volume_mL))

#This subsets out the 10cm percent water content and soil salinity
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
  dplyr::select(-depth)

#This subsets 25cm percent water content and soil salinity (doesn't have all of the survey days represented)
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
  dplyr::select(-depth)

#Combines both soil datasets
soil<-soil10cm%>%
  left_join(soil25cm, by=c(location, "survey"))


#Mushroom data
#This takes my inat project data
mushroom_inat_data<-read.csv("mushroom_data_v2.csv")%>%
  separate(field.field.site.id, into=c("survey", "site", "plot"), sep="_")%>%
  mutate(half_id=paste(site, plot, sep="_"))%>%
  left_join(data.frame(half_id=c("1_1","1_2","1_3","2_1","2_2","2_3","3_1","3_2","3_3","4_1","4_2","4_3","5_1","5_2","5_3","6_1","6_2","6_3"),
                       plot_number=as.character(1:18)), by="half_id")%>%
  mutate(across(all_of(location),as.character))

#This cleans up the inat project data to just keep the things that we would need
mushroom_data_clean<-data.frame(
  site=mushroom_inat_data$site,
  plot=mushroom_inat_data$plot,
  plot_number=mushroom_inat_data$plot_number,
  survey=mushroom_inat_data$survey,
  species=mushroom_inat_data$scientific_name,
  family=mushroom_inat_data$taxon_family_name
)

#This calculates mushroom richness per plot
mushroom_richness<-mushroom_data_clean%>%
  group_by(survey,site,plot_number)%>%
  summarise(plot_mushroom_richness=n_distinct(species))%>%
  ungroup()


#Now create a full dataset with all of the cleaned measurments
half_data<-data.frame(
  site=canopy_coverage$site,
  plot=canopy_coverage$plot,
  plot_number=canopy_coverage$plot_number,
  canopy_coverage=canopy_coverage$canopy_coverage_avg,
  vegtation_cover=vegetation_cover$vegetation_cover,
  needle_depth=needle_depth$needle_depth_avg,
  needle_coverage=needle_coverage$needle_coverage,
  organic_layer_depth=organic_layer_depth$organic_layer_depth_avg,
  tree_density=tree_density$tree_density,
  tree_species_richness=tree_species_richness$species_richness,
  plant_species_richness=overall_veg_species_richness$overall_species_richness,
  organic_content_10cm=organic_content_10cm$percent_organic_matter_avg,
  organic_content_25cm=organic_content_25cm$percent_organic_matter_avg
)

#Join to soil data which makes it per survey day now but lets change survey day to julian date
all_data<-half_data%>%
  left_join(soil, by=c("site", "plot", "plot_number"))%>%
  mutate(
  julian_date=dplyr::recode(survey,
                      `1`=180,
                      `2`=187,
                      `3`=193,
                      `4`=200,
                      `5`=210,
                      `6`=215,
                      `7`=263))%>%
  left_join(mushroom_richness, by=c("site","survey","plot_number"), relationship="many-to-many")%>%
        mutate(plot_mushroom_richness = ifelse(
            is.na(plot_mushroom_richness),
            0,
            plot_mushroom_richness
          ))%>%
  mutate(block = ifelse(site %in% c(1, 2, 3), 1, 2))

write.csv(all_data,"ecological_modeling_maritime_v3.csv")



