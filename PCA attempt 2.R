#10/30/25
#Rachel Pitt
#This is attempt 2 to make  PCA but this time I am going to try and incorporate the soil data


#Library + helpful string
library(tidyverse)
library(ggbiplot)
location<-c("site","plot","plot_number")


#Deal with soil data and rest of our variables-------------------------- 
soil_raw<-read.csv("new_soil_v2.csv")%>%
  mutate(across(all_of(location),as.character))

soil_calcs<-soil_raw %>%
  mutate(wet_soil_weight_g=(combined_wet_weight_g-beaker_weight_g),
         dry_soil_weight_g=(combined_dry_weight_g-beaker_weight_g),
         water_volume_mL=(wet_soil_weight_g-dry_soil_weight_g),
         percent_water_content=(water_volume_mL/wet_soil_weight_g)*100,
         soil_salinity_ppt=(rehydrated_water_volume_mL*supernatent_salinity_ppt/water_volume_mL))


soil10cm<-data.frame(
  survey=soil_calcs$survey,
  site=soil_calcs$site,
  plot=soil_calcs$plot,
  plot_number=soil_calcs$plot_number,
  depth=soil_calcs$soil_depth_cm,
  soil_percent_water=soil_calcs$percent_water_content,
  soil_salinity_ppt=soil_calcs$soil_salinity_ppt
)%>%
  filter(depth=="10")%>%
  pivot_wider(
    names_from=survey,
    values_from=c(soil_percent_water, soil_salinity_ppt),
    names_glue="10cm_{.value}_survey_{survey}")%>%
  select(-depth)

soil25cm<-data.frame(
  survey=soil_calcs$survey,
  site=soil_calcs$site,
  plot=soil_calcs$plot,
  plot_number=soil_calcs$plot_number,
  depth=soil_calcs$soil_depth_cm,
  soil_percent_water=soil_calcs$percent_water_content,
  soil_salinity_ppt=soil_calcs$soil_salinity_ppt
)%>%
  filter(depth=="25")%>%
   pivot_wider(
     names_from=survey,
     values_from=c(soil_percent_water, soil_salinity_ppt),
     names_glue="25cm_{.value}_survey_{survey}")%>%
  select(-depth)
 
soil<-soil10cm %>%
  left_join(soil25cm, by=location)


#Join soil with all other variables:
actually_all<-left_join(all_variables,soil, by=location)


pc<-prcomp(actually_all %>%
             select(-all_of(location)) %>%
             mutate(across(everything(), as.numeric)),
           center=TRUE,
           scale.=TRUE)

print(pc)

plot<-ggbiplot(pc,
               obs.scale=1,
               var.scale=1,
               groups=actually_all$site,
               ellipse=TRUE,
               circle=TRUE,
               ellipse.prob=0.68
) 

plot

