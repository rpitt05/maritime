#Ecology extra credit
#Rachel Pitt
#12/3/25
#"Principle Cat Analysis

library(tidyverse)
cat<-read.csv("cat_points.csv")%>%
  rename(PC1=x, PC2=y)

seven_cats<-read.csv("seven_cat_points.csv")%>%
  rename(PC1=x, PC2=y)

ggplot(seven_cats, aes(PC1, PC2))+
  geom_point(color="goldenrod")+
  labs(title="Principle Cat Analysis")+
  theme_classic()
