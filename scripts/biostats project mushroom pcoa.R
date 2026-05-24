#Biostats project PCOA
#12/2/25
#Rachel Pitt


#Library
library(tidyverse)
library(vegan)

#Data taken from biostats project variables and biostats project ordinations
#Dataframe of each possible sampling location on each survey day:



#Setting up presence absence with mushroom data:
mushroom_inat_data<-read.csv("mushroom_data_v2.csv")%>%
  separate(field.field.site.id, into=c("survey", "site", "plot"), sep="_")%>%
  mutate(half_id=paste(site, plot, sep="_"))%>%
  left_join(data.frame(half_id=c("1_1","1_2","1_3","2_1","2_2","2_3","3_1","3_2","3_3","4_1","4_2","4_3","5_1","5_2","5_3","6_1","6_2","6_3"),
                       plot_number=as.character(1:18)), by="half_id")%>%
  mutate(across(all_of(location),as.character))

mushroom_data_clean<-data.frame(
  site=mushroom_inat_data$site,
  plot_number=mushroom_inat_data$plot_number,
  survey=mushroom_inat_data$survey,
  family=mushroom_inat_data$taxon_family_name
)%>%
  filter(survey>3)

mushroom_presence_absence<-mushroom_data_clean %>%
  filter(family!=""& !is.na(family))%>%
  distinct(site, plot_number, survey, family)%>%
  mutate(PA=1)%>%
  pivot_wider(
    names_from=family,
    values_from=PA,
    values_fill=list(PA=0)
  )


#Set up the distance matrix by getting rid of location/survey data
family_matrix<-mushroom_presence_absence%>%
  select(-site,-plot_number, -survey)

dist_mat<-vegdist(family_matrix, method="jaccard", binary=TRUE)

#Now do the PCoA
pcoa<-cmdscale(dist_mat, eig=TRUE, k=2)
pcoa_scores<-mushroom_presence_absence%>%
  mutate(PC1=pcoa$points[,1],
         PC2=pcoa$points[,2])

#Original plotting of the pcoa
ggplot(pcoa_scores, aes(PC1, PC2, group=site, color=factor(survey)))+
  geom_point(size=3)+
  geom_path(arrow=arrow(type="closed", length=unit(0.15,"inches")))+
  theme_classic()


#Plotting with clusters:
clust<-kmeans(pcoa_scores[,c("PC1","PC2")], centers=3)
pcoa_scores$cluster<-factor(clust$cluster)


ggplot(pcoa_scores, aes(PC1, PC2, color=cluster))+
  geom_point(size=2)+
  theme_classic()+
  labs(x="               Amanitas <----                  PC1                      ----> Russulas/Boletes   ", 
       y="         Boletes <-------              PC2                  -------> Saprotrophs")
 
#Which mushrooms are influencing PC1 and PC2?
cor_PC1<-cor(family_matrix, pcoa_scores$PC1)
cor_PC2<-cor(family_matrix, pcoa_scores$PC2)

family_loadings<-data.frame(
  family=colnames(family_matrix),
  PC1=cor_PC1,
  PC2=cor_PC2
)

family_loadings%>%arrange(desc(abs(PC2)))


#Which mushrooms are most common in each cluster?
pcoa_family<-left_join(pcoa_scores, family_matrix)
cluster_family<-pcoa_family%>%
  group_by(cluster)%>%
  summarize(across(colnames(family_matrix),mean))

clust_1<-subset(cluster_family, cluster==1)
clust_1<-sort(unlist(clust_1[1, ]), decreasing = TRUE)
clust_2<-subset(cluster_family, cluster==2)
clust_2<-sort(unlist(clust_2[1, ]), decreasing = TRUE)
clust_3<-subset(cluster_family, cluster==3)
clust_3<-sort(unlist(clust_3[1, ]), decreasing = TRUE)
view(clust_1)
view(clust_2)
view(clust_3)
