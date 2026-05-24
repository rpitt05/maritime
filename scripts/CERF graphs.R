#Graphs for CERF2025
#Rachel Pitt
#11/6/25


library(tidyverse)


#Tree stuff
tree_counts<-read.csv("tree_counts_v1.csv")%>%
  mutate(across(all_of(location),as.character))

tree_density<-tree_counts%>%
  mutate(tree_count=(loblolly_count+cherry_count+water_oak_count+maple_count+sweetgum_count+sassafras_count+holly_count+willow_oak_count),
         tree_density=tree_count/100)

tree_species_richness<-tree_counts%>%
  mutate(species_richness=(rowSums(tree_counts!=0)-3))

#Mushroom richness
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
  summarise(plot_mushroom_richness=n_distinct(species))%>%
  ungroup()%>%
  mutate(across(everything(), ~replace_na(.x,0)))

#Soil
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

#merge data

veg<-data.frame(
  site=tree_density$site,
  plot=tree_density$plot,
  plot_number=tree_density$plot_number,
  tree_density=tree_density$tree_density,
  tree_species_richness=tree_species_richness$species_richnes,
  veg_species_richness=total_veg_species_richness$veg_species_richness,
  organic_content_10cm=organic_content$percent_organic_matter_10cm,
  organic_content_25cm=organic_content$percent_organic_matter_25cm
)


data<-soil10cm%>%
  left_join(veg, by=location)%>%
  left_join(mushroom_richness, by=c("site","plot_number","survey"))%>%
  mutate(julian_date=recode(survey,
                                   `1`=180,
                                   `2`=187,
                                   `3`=193,
                                   `4`=200,
                                   `5`=210,
                                   `6`=215,
                                   `7`=263))%>%
  mutate(across(everything(), ~replace_na(.x,0)))



#Graph stuff:
library(MASS)
library(performance)

#Tree density and mushroom richness
ggplot(filter(data, survey>3), aes(tree_density, plot_mushroom_richness, color=site))+geom_point(position="jitter")+
  geom_smooth(aes(color=NULL), method="lm")+
  labs(x="Tree Density", y="Mushroom Richness")+
  theme_classic()

#Tree density for summer mushrooms
ggplot(filter(data, survey<7 & survey>3), aes(tree_density, plot_mushroom_richness, color=site))+geom_point(position="jitter")+
  geom_smooth(aes(color=NULL), method="lm")+
  labs(x="Tree Density", y="Mushroom Richness")+
  theme_classic()

#Tree density for fall mushrooms
ggplot(filter(data, survey=="7"), aes(tree_density, plot_mushroom_richness, color=site))+geom_point(position="jitter")+
  geom_smooth(aes(color=NULL), method="lm")+
  labs(x="Tree Density", y="Mushroom Richness")+
  theme_classic()

#Soil salinity and mushroom richness
ggplot(filter(data, survey>3), aes(soil_salinity_ppt_10cm, plot_mushroom_richness, color=site))+geom_point(position="jitter")+
  geom_smooth(aes(color=NULL), method="lm")+
  labs(x="Soil Salinity (ppt)", y="Mushroom Richness")+
  theme_classic()


#Organic content vs mushroom richness
ggplot(filter(data, survey>3), aes(organic_content_10cm, plot_mushroom_richness, color=site))+geom_point(position="jitter")+
  geom_smooth(aes(color=NULL),method="lm")+
  labs(x="Percent Organic Matter", y="Mushroom Richness")+
  theme_classic()

#Organic content vs mushroom richness JUST from survey 7
ggplot(filter(data, survey=="7"),aes(organic_content_10cm, plot_mushroom_richness, color=site))+geom_point(position="jitter")+
  geom_smooth(aes(color=NULL),method="lm")+
  labs(x="Percent Organic Matter", y="Mushroom Richness")+
  theme_classic()

#Organic content vs mushroom richness JUST summer (surveys 4-6)
ggplot(filter(data, survey>3 & survey!=7),aes(organic_content_10cm, plot_mushroom_richness, color=site))+geom_point(position="jitter")+
  geom_smooth(aes(color=NULL),method="lm")+
  labs(x="Percent Organic Matter", y="Mushroom Richness")+
  theme_classic()

#Final soil moisture plot: -Note I did not include the first two surveys for lack of mushrooms
ggplot(filter(data, survey>3), aes(soil_percent_water_10cm, plot_mushroom_richness, color=site))+geom_point(position="jitter")+
  geom_smooth(aes(color=NULL),method="lm")+
  labs(x="Soil Moisture", y="Mushroom Richness")+
  theme_classic()


#Final Mushrooms over time plot:
ggplot(data, aes(julian_date, plot_mushroom_richness, color=site))+geom_point(position="jitter")+
  geom_smooth(aes(color=NULL),method="lm")+
  labs(x="Julian Date", y="Mushroom Richness")+
  theme_classic()
  



#Diversity graph
diversity <- mushroom_data_clean %>%
  mutate(season = if_else(as.numeric(survey) < 7, "Summer", "fall")) %>%
  group_by(site, season, family) %>%
  summarise(mushroom_richness = n_distinct(species), .groups = "drop") %>%
  mutate(site_season = paste(season, "site", site, sep = " "))%>%
  arrange(as.numeric(site), season) %>%
  mutate(x_pos = as.numeric(site) + ifelse(season == "Summer", -0.2, 0.2))
family_colors <- setNames(
  colorRampPalette(brewer.pal(8, "Paired"))(length(unique(mushroom_data_clean$family))),
  sort(unique(mushroom_data_clean$family))
)

guild <- data.frame(
  family = c("Agaricaceae", "Amanitaceae", "Boletaceae", "Marasmiaceae", "Porotheleaceae",
             "Russulaceae", "Sclerodermataceae", "Dacrymycetaceae", "Hymenogastraceae",
             "Mycenaceae", "Omphalotaceae", "Suillaceae", "Gyroporaceae", "Pleurotaceae",
             "Irpicaceae", "Pluteaceae", "Entolomataceae", "Phallaceae", "Callistosporiaceae",
             "Cortinariaceae", "Cyphellaceae", "Hydnaceae"),
  guild = c("Saprobic", "Ectomycorrhizal", "Ectomycorrhizal", "Saprobic", "Saprobic",
            "Ectomycorrhizal", "Ectomycorrhizal", "Saprobic", "Saprobic",
            "Saprobic", "Saprobic", "Ectomycorrhizal", "Ectomycorrhizal", "Saprobic",
            "Saprobic", "Saprobic", "Saprobic", "Saprobic", "Saprobic",
            "Ectomycorrhizal", "Saprobic", "Ectomycorrhizal"))
diversity<-diversity%>%
  left_join(guild, by="family")%>%
  mutate(guild = replace_na(guild, "Unknown"))


ggplot(diversity, aes(x = x_pos, y = mushroom_richness, fill = family)) +
  geom_bar(stat = "identity", width = 0.35) +  # narrower bars
  scale_fill_manual(values = family_colors) +
  scale_x_continuous(
    breaks = 1:6,
    labels = paste("Site", 1:6)
  ) +
  theme_classic() +
  labs(
    x = "Site by Season",
    y = "Mushroom Richness",
    fill = "Family"
  )
#+
  theme(legend.position="none")



#Above graph but by guilds
ggplot(diversity, aes(x = x_pos, y = mushroom_richness, fill = guild)) +
  geom_bar(stat = "identity", width = 0.35) +
  scale_fill_manual(values = c(
    "Ectomycorrhizal" = "paleturquoise3",
    "Saprobic" = "sienna2",
    "Unknown"="gray40"
  )) +
  scale_x_continuous(
    breaks = 1:6,
    labels = paste("Site", 1:6)
  ) +
  theme_classic() +
  labs(
    x = "Site by Season",
    y = "Mushroom Richness",
    fill = "Functional Guild"
  )

+
  theme(legend.position="none")




#Stats stuff:

#PC1 vs mushroom richness
mod1<-lm(PC1 ~ mushroom_richness, data=pc_data)
summary(mod1)
#PC2 vs mushroom richness
mod2<-lm(PC2 ~ mushroom_richness, data=pc_data)
summary(mod2)
#Organic matter vs mushroom richness
mod3<-lm(organic_content_10cm ~ plot_mushroom_richness, data=data)
summary(mod3)
#Organic matter Summer
mod4<-lm(organic_content_10cm ~ plot_mushroom_richness, data=(filter(data, survey>3 & survey!=7)))
summary(mod4)
#Organic matter Fall
mod5<-lm(organic_content_10cm ~ plot_mushroom_richness, data=(filter(data, survey=="7")))
summary(mod5)
#Tree density vs mushroom richness
mod6<-lm(tree_density ~ plot_mushroom_richness, data=data)
summary(mod6)
#Tree density Summer
mod7<-lm(tree_density ~ plot_mushroom_richness, data=(filter(data, survey>3 & survey!=7)))
summary(mod7)
#Tree density Fall
mod8<-lm(tree_density ~ plot_mushroom_richness, data=(filter(data, survey=="7")))
summary(mod8)
#Time vs Mushroom richness
mod9<-lm(julian_date ~ plot_mushroom_richness, data=data)
summary(mod9)
