#Biostats project plant mushroom combined pcoa
#Rachel Pitt
#12/4/25


#Library
library(tidyverse)
library(vegan)

#Data taken from biostats project variables

total_presence_absence<-left_join(mushroom_presence_absence, plant_presence_absence)


total_matrix<-total_presence_absence%>%
  select(-site,-plot_number, -survey)

total_dist_mat<-vegdist(total_matrix, method="jaccard", binary=TRUE)

#Now do the PCoA
total_pcoa<-cmdscale(total_dist_mat, eig=TRUE, k=2)
total_pcoa_scores<-total_presence_absence%>%
  mutate(PC1=total_pcoa$points[,1],
         PC2=total_pcoa$points[,2])



#Plotting with clusters:
total_clust<-kmeans(total_pcoa_scores[,c("PC1","PC2")], centers=3)
total_pcoa_scores$cluster<-factor(total_clust$cluster)

ggplot(total_pcoa_scores, aes(PC1, PC2, color=cluster))+
  geom_point(size=2)+
  theme_classic()

#Which plants are influencing PC1 and PC2?
total_cor_PC1<-cor(total_matrix, total_pcoa_scores$PC1)
total_cor_PC2<-cor(total_matrix, total_pcoa_scores$PC2)

total_loadings<-data.frame(
  species=colnames(total_matrix),
  PC1=total_cor_PC1,
  PC2=total_cor_PC2
)

total_loadings%>%arrange(desc(abs(PC1)))


#Which plants are most common in each cluster?
pcoa_total<-left_join(total_pcoa_scores, total_matrix)
cluster_total<-pcoa_total%>%
  group_by(cluster)%>%
  summarize(across(colnames(total_matrix),mean))

