#PCA attempt 3
#Rachel Pitt
#10/30/25

#library
library(tidyverse)
library(ggbiplot)

#OR YOU CAN LEFT JOIN THEM BY SITE PLOT AND PLOT NUMBER AND HAVE IT IN THE LONGER SOIL 
#FORMAT AND ALSO HVE MUSHROOMS OCRRESPOND TO THE SURVEY DAYS

soil_raw<-read.csv("new_soil_v2.csv")%>%
  mutate(across(all_of(location),as.character))

soil_calcs<-soil_raw %>%
  mutate(wet_soil_weight_g=(combined_wet_weight_g-beaker_weight_g),
         dry_soil_weight_g=(combined_dry_weight_g-beaker_weight_g),
         water_volume_mL=(wet_soil_weight_g-dry_soil_weight_g),
         percent_water_content=(water_volume_mL/wet_soil_weight_g)*100,
         soil_salinity_ppt=(rehydrated_water_volume_mL*supernatent_salinity_ppt/water_volume_mL))

soil<-data.frame(
  site=soil_calcs$site,
  plot=soil_calcs$plot,
  plot_number=soil_calcs$plot_number,
  survey=soil_calcs$survey,
  percent_water_content=soil_calcs$percent_water_content,
  soil_salinity=soil_calcs$soil_salinity_ppt)%>%
  mutate(across(all_of(c("site","plot","plot_number","survey")),as.character
))

#And also you should be treating mushroom survey richness differently in this format

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
  group_by(survey,site, plot, plot_number)%>%
  summarise(mushroom_richness=n_distinct(scientific_name))%>%
  ungroup()


#Join mushroom data to the rest of the data:
overall_everything<-all_variables%>%
  select(-contains("mushroom"))%>%
  left_join(mushroom_richness, by=location)%>%
  left_join(soil, by=c("survey","site","plot","plot_number")
)%>%
  mutate(days_since_first_survey=recode(survey,
                                        `1`=1,
                                        `2`=7,
                                        `3`=13,
                                        `4`=20,
                                        `5`=30,
                                        `6`=35,
                                        `7`=83))


#Plot
pc<-prcomp(overall_everything %>%
             select(-all_of(location)) %>%
             select(-survey)%>%
             select(-mushroom_richness)%>%
             mutate(across(everything(), as.numeric)),
           center=TRUE,
           scale.=TRUE)

print(pc)

ggbiplot(pc,
               obs.scale=1,
               var.scale=1,
               groups=overall_everything$site,
               ellipse=TRUE,
               circle=TRUE,
               ellipse.prob=0.68
) 


#Now save the pca scores to a variable and then join them back to groupings
pca_scores<-as.data.frame(pc$x)
pca_data<-cbind(overall_everything, pca_scores)

#OMG YAYAYAYAYYA IT WORKED!!!!!!!!
