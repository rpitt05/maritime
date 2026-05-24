#Soil PCA
#Rachel Pitt
#11/9/25


library(tidyverse)


#Soil data:
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
  filter(survey>2)%>%
  dplyr::select(-depth)

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

#Merge soil data
soil<-soil25cm%>%
  left_join(soil10cm, by=c(location, "survey"))

#Mushroom data:
mushroom_inat_data<-read.csv("mushroom_data_v2.csv")%>%
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
  species=mushroom_inat_data$scientific_name,
  family=mushroom_inat_data$taxon_family_name
)

mushroom_richness<-mushroom_data_clean%>%
  group_by(survey,site,plot_number)%>%
  summarise(mushroom_richness=n_distinct(species))%>%
  ungroup()%>%
  mutate(across(everything(), ~replace_na(.x,0)))%>%
  filter(survey>2)

#merge soil and mushroom richness
soil_mush<-soil%>%
  left_join(mushroom_richness)%>%
  mutate(across(everything(), ~replace_na(.x,0)))


#Create soil PCA
library(ggbiplot)
pc<-prcomp(soil_mush %>%
             dplyr::select(-survey, -site, -plot, -plot_number, -mushroom_richness) %>%
             mutate(across(everything(), as.numeric)),
           center=TRUE,
           scale.=TRUE)

print(pc)

ggbiplot(pc,
         obs.scale=1.5,
         var.scale=1,
         groups=soil_mush$site,
         ellipse=TRUE,
         circle=TRUE,
         ellipse.prob=0.68
) 

pc_scores<-as.data.frame(pc$x)
pc_data<-cbind(soil_mush, pc_scores)

#Graphing against the PCs
ggplot(pc_data, aes(PC1, mushroom_richness, color=site))+geom_point(position="jitter")+
  geom_smooth(aes(color=NULL),method="lm")+
  labs(x="PC1", y="Mushroom Richness")+
  theme_classic()

cor(pc_data$PC1, pc_data$mushroom_richness)








