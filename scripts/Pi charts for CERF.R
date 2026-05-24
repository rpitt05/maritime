#Diversity Pi Charts for CERF presentation 
#Rachel Pitt
#11/6/25



library(tidyverse)

#Ok the goal with this code is to create 6 pi charts per survey day so 42 pi charts total

#So first I need to calculate mushroom richness PER SITE for each of the survey days
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
)%>%
  mutate(julian_date=recode(survey,
                                        `1`=180,
                                        `2`=187,
                                        `3`=193,
                                        `4`=200,
                                        `5`=210,
                                        `6`=215,
                                        `7`=263))%>%
  mutate(family = if_else(family == "", "unknown", family))

mushroom_richness<-mushroom_data_clean%>%
  group_by(survey,site,julian_date)%>%
  summarise(site_mushroom_richness=n_distinct(species))%>%
  ungroup()%>%
  mutate(across(everything(), ~replace_na(.x,0)))


#Create pi charts-------------------------

library(RColorBrewer)

family_colors <- setNames(
  colorRampPalette(brewer.pal(8, "Paired"))(length(unique(mushroom_data_clean$family))),
  sort(unique(mushroom_data_clean$family))
)
ggplot(filter(mushroom_data_clean, survey=="4"), aes("", fill=family))+
  geom_bar(width=1, stat="count", color="white")+
  theme_void()+
  labs(fill="family")+
  scale_fill_manual(values=family_colors)

#+
  theme(legend.position="none")















