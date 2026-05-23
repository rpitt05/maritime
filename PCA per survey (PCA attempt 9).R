#PCA per survey to show mushroom richness against nonindependent and independent variables
#Rachel Pitt
#11/3/25



#Survey 1:
str(survey_1)

survey1<-survey_1 %>%
  select(-vegetation_cover, -strata_veg_species_richness_1, -strata_veg_species_richness_2, -strata_veg_species_richness_3,
         -tree_species_richness,
         -site_mushroom_richness_survey,
         -percent_organic_matter_25cm, -needle_coverage,-organic_layer_depth)


survey2<-survey_2 %>%
  select(-vegetation_cover, -strata_veg_species_richness_1, -strata_veg_species_richness_2, -strata_veg_species_richness_3,
         -tree_species_richness,
         -site_mushroom_richness_survey,
         -percent_organic_matter_25cm, -needle_coverage,-organic_layer_depth)

survey3<-survey_3 %>%
  select(-vegetation_cover, -strata_veg_species_richness_1, -strata_veg_species_richness_2, -strata_veg_species_richness_3,
         -tree_species_richness,
         -site_mushroom_richness_survey,
         -percent_organic_matter_25cm, -needle_coverage,-organic_layer_depth)

survey4<-survey_4 %>%
  select(-vegetation_cover, -strata_veg_species_richness_1, -strata_veg_species_richness_2, -strata_veg_species_richness_3,
         -tree_species_richness,
         -site_mushroom_richness_survey,
         -percent_organic_matter_25cm, -needle_coverage,-organic_layer_depth,
         -soil_percent_water_25cm, -soil_salinity_ppt_25cm)

survey5<-survey_5 %>%
  select(-vegetation_cover, -strata_veg_species_richness_1, -strata_veg_species_richness_2, -strata_veg_species_richness_3,
         -tree_species_richness,
         -site_mushroom_richness_survey,
         -percent_organic_matter_25cm, -needle_coverage,-organic_layer_depth,
         -soil_percent_water_25cm, -soil_salinity_ppt_25cm)

survey6<-survey_6 %>%
  select(-vegetation_cover, -strata_veg_species_richness_1, -strata_veg_species_richness_2, -strata_veg_species_richness_3,
         -tree_species_richness,
         -site_mushroom_richness_survey,
         -percent_organic_matter_25cm, -needle_coverage,-organic_layer_depth,
         -soil_percent_water_25cm, -soil_salinity_ppt_25cm)

survey7<-survey_7 %>%
  select(-vegetation_cover, -strata_veg_species_richness_1, -strata_veg_species_richness_2, -strata_veg_species_richness_3,
         -tree_species_richness,
         -site_mushroom_richness_survey,
         -percent_organic_matter_25cm, -needle_coverage,-organic_layer_depth,
         -soil_percent_water_25cm, -soil_salinity_ppt_25cm)


#Graphing
pc<-prcomp(survey7 %>%
             select(-site, -plot, -plot_number, -plot_mushroom_richness_survey) %>%
             mutate(across(everything(), as.numeric)),
           center=TRUE,
           scale.=TRUE)

print(pc)

ggbiplot(pc,
         obs.scale=1.5,
         var.scale=1,
         groups=survey7$site,
         ellipse=TRUE,
         circle=TRUE,
         ellipse.prob=0.68
) 

pc_scores<-as.data.frame(pc$x)
pc_data<-cbind(nonindependent, pc_scores)