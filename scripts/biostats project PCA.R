#Biostats project ordinations
#Rachel Pitt
#12/2/25

#Library:
library(tidyverse)
library(ggbiplot)
library(vegan)

#Data taken from "biostats project variables.R"
#"all"= all variables (fixed and per survey)
#location= site, plot, plot number

all_renamed<-all%>%
  rename(
    NL=needle_coverage,
    VG=vegetation_cover,
    SS10=soil_salinity_ppt_10cm,
    SS25=soil_salinity_ppt_25cm,
    PW10=soil_percent_water_10cm,
    PW25=soil_percent_water_25cm,
    PO10=percent_organic_matter_10cm,
    PO25=percent_organic_matter_25cm,
    OD=organic_layer_depth,
    OV=overall_species_richness,
    MSR=plot_mushroom_richness,
    JD=julian_date,
    ND=needle_depth,
    TD=tree_density,
    TSR=tree_species_richness,
    V1=strata_veg_species_richness_1,
    V2=strata_veg_species_richness_2,
    V3=strata_veg_species_richness_3,
    CC=canopy_coverage
  )

#Overall ordination that includes each variable I measured myself 
overall_pc<-prcomp(all_renamed %>%
             select(-all_of(c(location, "survey"))) %>%
             mutate(across(everything(), as.numeric)),
           center=TRUE,
           scale.=TRUE)

overall_plot<-ggbiplot(overall_pc,
               obs.scale=1,
               var.scale=1,
               groups=all$site,
               ellipse=TRUE,
               circle=TRUE,
               ellipse.prob=0.68
) 

overall_plot+
  labs(title="PCA biplot grouped by site")+
  theme_classic()

overall_pc_scores<-as.data.frame(overall_pc$x)
overall_pc_data<-cbind(all_renamed, overall_pc_scores)

#Now by reducing the dimensionality of the data based upon patterns visible in the overall_plot
#ordination we can create the ordination below:

reduced_all<-all%>%
  select(-strata_veg_species_richness_1,-strata_veg_species_richness_2,-strata_veg_species_richness_3,
         -percent_organic_matter_25cm, -soil_percent_water_25cm, -soil_salinity_ppt_25cm, -needle_depth,
         -organic_layer_depth)%>%
  rename(
    SS=soil_salinity_ppt_10cm,
    PW=soil_percent_water_10cm,
    PO=percent_organic_matter_10cm,
    MSR=plot_mushroom_richness,
    CC=canopy_coverage,
    TSR=tree_species_richness,
    TD=tree_density,
    VG=vegetation_cover,
    NL=needle_coverage,
    VO=overall_species_richness,
    JD=julian_date
  )

reduced_pc<-prcomp(reduced_all %>%
                     select(-all_of(c(location, "survey"))) %>%
                     mutate(across(everything(), as.numeric)),
                   center=TRUE,
                   scale.=TRUE)

reduced_plot<-ggbiplot(reduced_pc,
                       obs.scale=1,
                       var.scale=1,
                       groups=reduced_all$site,
                       ellipse=TRUE,
                       circle=TRUE,
                       ellipse.prob=0.68
) 

reduced_plot+
  labs(title="Reduced PCA biplot grouped by site")+
  theme_classic()

reduced_pc_scores<-as.data.frame(reduced_pc$x)
reduced_pc_data<-cbind(reduced_all, reduced_pc_scores)


