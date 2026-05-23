#PCA of just the per survey measurments so soil moisture, salinity, and mushroom richness
#Rachel Pitt
#11/3/25


nonindependent<-soil10cm%>%
  left_join(soil25cm, by=c(location,"survey"))%>%
  left_join(mushroom_richness_overall, by=c(location, "survey"))%>%
  filter(survey>3)%>%
  mutate(across(everything(), ~replace_na(.x, 0)))%>%
  select(-soil_percent_water_25cm, -soil_salinity_ppt_25cm)

pc<-prcomp(nonindependent %>%
             select(-site, -plot, -plot_number, -plot_mushroom_richness_overall, -survey) %>%
             mutate(across(everything(), as.numeric)),
           center=TRUE,
           scale.=TRUE)

print(pc)

ggbiplot(pc,
         obs.scale=1.5,
         var.scale=1,
         groups=nonindependent$site,
         ellipse=TRUE,
         circle=TRUE,
         ellipse.prob=0.68
) 

pc_scores<-as.data.frame(pc$x)
pc_data<-cbind(nonindependent, pc_scores)

ggplot(pc_data, aes(PC1, plot_mushroom_richness_overall))+geom_point()+geom_smooth(method="lm")
ggplot(pc_data, aes(PC2, plot_mushroom_richness_overall))+geom_point()+geom_smooth(method="lm")
