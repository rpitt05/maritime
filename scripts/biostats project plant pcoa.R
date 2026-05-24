#Biostats project plant pcoa
#Rachel Pitt
#12/3/25


#Library
library(tidyverse)
library(vegan)

#Create presence absence data with vegetation data
strata_vegetation<-read.csv("strata_vegetation_v1.csv")%>%
  mutate(across(all_of(location),as.character))

plant_presence_absence<-strata_vegetation %>%
  select(-location_id,-strata)%>%
  filter(species!=""& !is.na(species))%>%
  distinct(site, plot_number, species)%>%
  mutate(PA=1)%>%
  pivot_wider(
    names_from=species,
    values_from=PA,
    values_fill=list(PA=0)
  )

#Set up the distance matrix by getting rid of location data
plant_matrix<-plant_presence_absence%>%
  select(-site,-plot_number)

plant_dist_mat<-vegdist(plant_matrix, method="jaccard", binary=TRUE)

#Now do the PCoA
plant_pcoa<-cmdscale(plant_dist_mat, eig=TRUE, k=2)
plant_pcoa_scores<-plant_presence_absence%>%
  mutate(PC1=plant_pcoa$points[,1],
         PC2=plant_pcoa$points[,2])

#Original plotting of the pcoa
ggplot(plant_pcoa_scores, aes(PC1, PC2, group=plot_number, color=factor(site)))+
  geom_point(size=3)+
  geom_path(arrow=arrow(type="closed", length=unit(0.15,"inches")))+
  theme_classic()


#Plotting with clusters:
plant_clust<-kmeans(plant_pcoa_scores[,c("PC1","PC2")], centers=3)
plant_pcoa_scores$cluster<-factor(plant_clust$cluster)

ggplot(plant_pcoa_scores, aes(PC1, PC2, color=cluster))+
  geom_point(size=2)+
  theme_classic()

#Which plants are influencing PC1 and PC2?
plant_cor_PC1<-cor(plant_matrix, plant_pcoa_scores$PC1)

plant_cor_PC2<-cor(plant_matrix, plant_pcoa_scores$PC2)

plant_loadings<-data.frame(
  species=colnames(plant_matrix),
  PC1=plant_cor_PC1,
  PC2=plant_cor_PC2
)

plant_loadings%>%arrange(desc(abs(PC1)))


#Which plants are most common in each cluster?
pcoa_plant<-left_join(plant_pcoa_scores, plant_matrix)
cluster_plant<-pcoa_plant%>%
  group_by(cluster)%>%
  summarize(across(colnames(plant_matrix),mean))



