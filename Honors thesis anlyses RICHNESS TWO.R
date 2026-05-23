#Honors thesis analyses RICHNESS TWO (from ultimate datasheet)
#Rachel Pitt
#4/24/26

#Packages
library(tidyverse)

#Data + clean data to keep have only 1 of each species found per substrate (since abundance per gall structure is impossible to determine)
#Keep in mind actually some fungal ids are the same between tree and ground so lets just add 
#an g before each ground for ant nest and a t before each tree one so we avoid this in
#downstream analyses

data<-read.csv("ultimate_datasheet_v11.csv")%>%
  mutate(location=sub("log ","log",location))%>%
  filter(substrate!="adult"&substrate!="seed"&substrate!="larvae")%>%
  mutate(tree_inat=as.factor(tree_inat),
         tree=as.factor(tree))


#Alpha diversity-----------------------------

##1)Species richness---------

#Species richness overall!
richnesstotal <- data %>%
  filter(data=="tree")%>%
  filter(substrate %in% c("kapello", "gall_body")) %>%
  summarise(richness = n_distinct(species[!is.na(species)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )
richnesstotal


#Avg sp richness of a gall (not separated by kapello or gall body)
richnessgall <- data %>%
  filter(data=="tree")%>%
  filter(substrate %in% c("kapello", "gall_body")) %>%
  group_by(gall_id) %>%
  summarise(richness = n_distinct(species[!is.na(species)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

mean(richnessgall$richness)


#Kapello vs gall body (just tree)
#This looks at richness of individual structures
richnesssp1 <- data %>%
  filter(data=="tree")%>%
  filter(substrate %in% c("kapello", "gall_body")) %>%
  group_by(isolation_id, substrate,tree) %>%
  summarise(richness = n_distinct(species[!is.na(species)]),
    .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnesssp1, aes(substrate,richness,fill=substrate))+
  geom_boxplot(width=0.5)+
  geom_jitter(width = 0.2, alpha = 0.5)+
  theme_classic()+
  labs(title="Species Richness of Kapellos and Gall bodies",
       x="Structure",
       y="Richness")


mean(filter(richnesssp1, substrate=="gall_body")$richness)

mean(filter(richnesssp1, substrate=="kapello")$richness)


#Looking per tree separated by substrate:
richnesssp1.1 <- data %>%
  filter(data=="tree")%>%
  filter(substrate %in% c("kapello", "gall_body")) %>%
  group_by(isolation_id, substrate,tree) %>%
  summarise(richness = n_distinct(species[!is.na(species)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnesssp1.1, aes(tree,richness,fill=substrate))+
  geom_boxplot(width=0.5)+
  theme_classic()+
  labs(title="Species Richness of Kapellos and Gall bodies by Tree",
       x="Tree",
       y="Richness")




#Richness of structures in different trees
richnesssp1.25 <- data %>%
  filter(data=="tree")%>%
  filter(substrate %in% c("kapello", "gall_body")) %>%
  group_by(isolation_id, tree) %>%
  summarise(richness = n_distinct(species[!is.na(species)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnesssp1.25, aes(tree,richness,fill=tree))+
  geom_boxplot(width=0.5)+
  theme_classic()+
  theme(legend.position="none")+
  labs(title="Species Richness of Galls by Tree",
       x="Tree",
       y="Richness")


#Combined:
richnesssp1.1$type <- "By substrate"
richnesssp1.25$type <- "By gall"

combined <- bind_rows(
  richnesssp1.1,
  richnesssp1.25 %>% mutate(substrate = "Gall")
)

ggplot(combined, aes(tree, richness, fill = substrate)) +
  geom_boxplot(width = 0.5) +
  facet_wrap(~type) +
  theme_classic() +
  labs(
    title = "Species Richness by Tree",
    x = "Tree",
    y = "Richness"
  )


#Species richness from galls per tree:
richnesssp1.256 <- data %>%
  filter(data=="tree")%>%
  filter(substrate %in% c("kapello", "gall_body")) %>%
  group_by(tree) %>%
  summarise(richness = n_distinct(species[!is.na(species)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

mean(richnesssp1.256$richness)

ggplot(richnesssp1.256, aes(tree,richness,fill="forestgreen"))+
  geom_bar(stat="identity")+
  theme_classic()+
  theme(legend.position="none")+
  labs(title="Species Richness of galls pooled by Tree",
       x="Tree",
       y="Richness")


mean(richnesssp1.256$richness)

#Kapello vs gall body (just tree)
#This looks at species richness overall of kapello vs gall body
richnesssp1.5<-data%>%
  filter(data=="tree")%>%
  filter(substrate=="kapello" | substrate=="gall_body")%>% #Filter just kapello and gall body
  group_by(substrate)%>%
  summarise(richness = n_distinct(species[!is.na(species)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnesssp1.5, aes(substrate,richness,fill=substrate))+
  geom_bar(stat="identity")+
  theme_classic()+
  labs(title="Sp Richness of Kapellos vs Gall bodies overall",
       x="Structure",
       y="Richness")

richnesssp1.5


#Galled leaves vs ungalled leaves (just tree) 
#This measure is also of total richness not like per leaf since that's not how we
#measured them
richnesssp2<-data%>%
  filter(substrate=="galled_leaf" | substrate=="healthy_leaf")%>% #Filter to just leaves
  group_by(substrate)%>%
  summarise(richness = n_distinct(species[!is.na(species)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

richnesssp2

ggplot(richnesssp2, aes(substrate,richness,fill=substrate))+
  geom_bar(stat="identity")+
  theme_classic()+
  labs(title="Sp Richness of galled leaves vs ungalled leaves",
       x="Structure",
       y="Richness")

#Richness of leaves all together
richnesssp2<-data%>%
  filter(substrate=="galled_leaf" | substrate=="healthy_leaf")%>% #Filter to just leaves
  summarise(richness = n_distinct(species[!is.na(species)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )


richnesssp2

#Galls vs leaves (just tree)
#I don't think this comparison should be made in the actual paper because
#galls and leaves didn't receive the same isolation effort
richnesssp3<-data%>%
  filter(data=="tree")%>%
  mutate(substrate = recode(substrate, #Group all leaves together and galls
                            "galled_leaf" = "leaf", "healthy_leaf"="leaf"
                            ,"gall_body"="gall","kapello"="gall"))%>%
  group_by(substrate) %>%
  summarise(richness = n_distinct(species[!is.na(species)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnesssp3, aes(substrate,richness,fill=substrate))+
  geom_bar(stat="identity")+
  theme_classic()+
  labs(title="Sp Richness of leaves vs galls",
       x="Structure",
       y="Richness")




#Ground vs Tree (ground and tree)
richnesssp4<-data%>%
  filter(substrate!="galled_leaf"& substrate!="healthy_leaf")%>%
  mutate(location=recode(location,"log"="ground"))%>%
  group_by(gall_id,location) %>%
  summarise(richness = n_distinct(species[!is.na(species)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnesssp4, aes(location,richness,fill=location))+
  geom_boxplot(width=.5)+
  geom_jitter(width = 0.2, alpha = 0.5)+
  theme_classic()+
  labs(title="Sp Richness of tree galls vs ground galls",
       x="Location",
       y="Richness")

mean(filter(richnesssp4,location=="tree")$richness)
mean(filter(richnesssp4,location=="ground")$richness)


#Total richness of all galls
richnesssp4<-data%>%
  filter(substrate!="galled_leaf"& substrate!="healthy_leaf")%>%
  mutate(location=recode(location,"log"="ground"))%>%
  summarise(richness = n_distinct(species[!is.na(species)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )


richnesssp4






#Total ground richness:

richnesssp6.1<-data%>%
  filter(data=="ground")%>%
  summarise(richness = n_distinct(species[!is.na(species)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )


richnesssp6.1






#Dispersed vs non dispersed (just ground)
#Aka found in logs vs found under trees on ground
richnesssp5<-data%>%
  filter(data=="ground")%>%
  group_by(isolation_id, location,status,age,tree) %>%
  summarise(richness = n_distinct(species[!is.na(species)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnesssp5, aes(location,richness,fill=location))+
  geom_boxplot(width=0.5)+
  geom_jitter(width = 0.2, alpha = 0.5)+
  theme_classic()+
  labs(title="Species Richness of dispersed vs non dispersed galls",
       x="Location",
       y="Richness")

mean(filter(richnesssp5,location=="log")$richness)
mean(filter(richnesssp5,location=="ground")$richness)

ggplot(richnesssp5, aes(as.factor(age),richness,fill=as.factor(age)))+
  geom_boxplot(width=0.5)+
  geom_jitter(width = 0.2, alpha = 0.5)+
  theme_classic()+
  labs(title="Species Richness of ground galls by location",
       x="Age",
       y="Richness")





#Alive vs dead (just ground)
richnesssp6<-data%>%
  filter(data=="ground")%>%
  filter(!is.na(status))%>%
  group_by(status,isolation_id) %>%
  summarise(richness = n_distinct(species[!is.na(species)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnesssp6, aes(status,richness,fill=status))+
  geom_boxplot(width=0.5)+
  geom_jitter(width = 0.2, alpha = 0.5)+
  theme_classic()+
  labs(title="Sp Richness of Live vs Dead galls",
       x="Status",
       y="Richness")

#Gall bodies of tree vs gall bodies of ground (tree and ground)
richnesssp7<-data%>%
  filter(substrate=="gall_body")%>%
  mutate(location=recode(location,"log"="ground"))%>%
  group_by(isolation_id,location) %>%
  summarise(richness = n_distinct(species[!is.na(species)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnesssp7, aes(location,richness,fill=location))+
  geom_boxplot(width=.5)+
  geom_jitter(width = 0.2, alpha = 0.5)+
  theme_classic()+
  labs(title="Sp Tree vs ground gall bodies",
       x="Status",
       y="Richness")



#GENUS RICHNESS-------------------------------------------
#Kapello vs gall body (just tree)
#This looks at richness of individual structures
richnessgen1 <- data %>%
  filter(substrate %in% c("kapello", "gall_body")) %>%
  group_by(isolation_id, substrate) %>%
  summarise(richness = n_distinct(genus[!is.na(genus)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnessgen1, aes(substrate,richness,fill=substrate))+
  geom_boxplot(width=0.5)+
  geom_jitter(width = 0.1, alpha = 0.3,)+
  theme_classic()+
  labs(title="Genus Richness of Kapellos vs Gall bodies",
       x="Structure",
       y="Richness")

#Richness of structures in different trees
richnessgen1.25 <- data %>%
  mutate(tree_inat=as.factor(tree_inat))%>%
  filter(substrate %in% c("kapello", "gall_body")) %>%
  group_by(isolation_id, tree_inat) %>%
  summarise(richness = n_distinct(genus[!is.na(genus)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnessgen1.25, aes(tree_inat,richness,fill=tree_inat))+
  geom_boxplot(width=0.5)+
  geom_jitter(width = 0.1, alpha = 0.3,)+
  theme_classic()+
  labs(title="Genu Richness of structures by different trees",
       x="Structure",
       y="Richness")


#Kapello vs gall body (just tree)
#This looks at genus richness overall of kapello vs gall body
richnessgen1.5<-data%>%
  filter(data=="tree")%>%
  filter(substrate=="kapello" | substrate=="gall_body")%>% #Filter just kapello and gall body
  group_by(substrate)%>%
  summarise(richness = n_distinct(genus[!is.na(genus)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnessgen1.5, aes(substrate,richness,fill=substrate))+
  geom_bar(stat="identity")+
  theme_classic()+
  labs(title="Genus Richness of Kapellos vs Gall bodies overall",
       x="Structure",
       y="Richness")



#Galled leaves vs ungalled leaves (just tree) 
#This measure is also of total richness not like per leaf since that's not how we
#measured them
richnessgen2<-data%>%
  filter(substrate=="galled_leaf" | substrate=="healthy_leaf")%>% #Filter to just leaves
  group_by(substrate)%>%
  summarise(richness = n_distinct(genus[!is.na(genus)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnessgen2, aes(substrate,richness,fill=substrate))+
  geom_bar(stat="identity")+
  theme_classic()+
  labs(title="Genus Richness of galled leaves vs ungalled leaves",
       x="Structure",
       y="Richness")



#Galls vs leaves (just tree)
#I don't think this comparison should be made in the actual paper because
#galls and leaves didn't receive the same isolation effort
richnessgen3<-data%>%
  filter(data=="tree")%>%
  mutate(substrate = recode(substrate, #Group all leaves together and galls
                            "galled_leaf" = "leaf", "healthy_leaf"="leaf"
                            ,"gall_body"="gall","kapello"="gall"))%>%
  group_by(substrate) %>%
  summarise(richness = n_distinct(genus[!is.na(genus)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnessgen3, aes(substrate,richness,fill=substrate))+
  geom_bar(stat="identity")+
  theme_classic()+
  labs(title="Genus Richness of leaves vs galls",
       x="Structure",
       y="Richness")




#Ground vs Tree (ground and tree)
richnessgen4<-data%>%
  filter(substrate!="galled_leaf"& substrate!="healthy_leaf")%>%
  mutate(location=recode(location,"log"="ground"))%>%
  group_by(isolation_id,location) %>%
  summarise(richness = n_distinct(genus[!is.na(genus)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnessgen4, aes(location,richness,fill=location))+
  geom_boxplot(width=.5)+
  geom_jitter(width = 0.2, alpha = 0.5)+
  theme_classic()+
  labs(title="Genus Richness of tree galls vs ground galls",
       x="Location",
       y="Richness")



#Dispersed vs non dispersed (just ground)
#Aka found in logs vs found under trees on ground
richnessgen5<-data%>%
  filter(data=="ground")%>%
  group_by(isolation_id, location) %>%
  summarise(richness = n_distinct(genus[!is.na(genus)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnessgen5, aes(location,richness,fill=location))+
  geom_boxplot(width=0.5)+
  geom_jitter(width = 0.2, alpha = 0.5)+
  theme_classic()+
  labs(title="Genus Richness of dispersed vs non dispersed",
       x="Location",
       y="Richness")


#Alive vs dead (just ground)
richnessgen6<-data%>%
  filter(data=="ground")%>%
  filter(!is.na(status))%>%
  group_by(status,isolation_id) %>%
  summarise(richness = n_distinct(genus[!is.na(genus)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnessgen6, aes(status,richness,fill=status))+
  geom_boxplot(width=0.5)+
  geom_jitter(width = 0.2, alpha = 0.5)+
  theme_classic()+
  labs(title="Genus Richness of Live vs Dead galls",
       x="Status",
       y="Richness")

#Gall bodies of tree vs gall bodies of ground (tree and ground)
richnessgen7<-data%>%
  filter(substrate=="gall_body")%>%
  mutate(location=recode(location,"log"="ground"))%>%
  group_by(isolation_id,location) %>%
  summarise(richness = n_distinct(genus[!is.na(genus)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnessgen7, aes(location,richness,fill=location))+
  geom_boxplot(width=.5)+
  geom_jitter(width = 0.2, alpha = 0.5)+
  theme_classic()+
  labs(title="Tree vs ground gall bodies",
       x="Status",
       y="Richness")






#FAMILY RICHNESS---------------------------------------------
#Kapello vs gall body (just tree)
#This looks at richness of individual structures
richnessfam1 <- data %>%
  filter(substrate %in% c("kapello", "gall_body")) %>%
  group_by(isolation_id, substrate) %>%
  summarise(richness = n_distinct(family[!is.na(family)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnessfam1, aes(substrate,richness,fill=substrate))+
  geom_boxplot(width=0.5)+
  geom_jitter(width = 0.1, alpha = 0.3,)+
  theme_classic()+
  labs(title="Family Richness of Kapellos vs Gall bodies",
       x="Structure",
       y="Richness")

#Richness of structures in different trees
richnessfam1.25 <- data %>%
  mutate(tree_inat=as.factor(tree_inat))%>%
  filter(substrate %in% c("kapello", "gall_body")) %>%
  group_by(isolation_id, tree_inat) %>%
  summarise(richness = n_distinct(family[!is.na(family)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnessfam1.25, aes(tree_inat,richness,fill=tree_inat))+
  geom_boxplot(width=0.5)+
  geom_jitter(width = 0.1, alpha = 0.3,)+
  theme_classic()+
  labs(title="Sp Richness of structures by different trees",
       x="Structure",
       y="Richness")


#Kapello vs gall body (just tree)
#This looks at family richness overall of kapello vs gall body
richnessfam1.5<-data%>%
  filter(data=="tree")%>%
  filter(substrate=="kapello" | substrate=="gall_body")%>% #Filter just kapello and gall body
  group_by(substrate)%>%
  summarise(richness = n_distinct(family[!is.na(family)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnessfam1.5, aes(substrate,richness,fill=substrate))+
  geom_bar(stat="identity")+
  theme_classic()+
  labs(title="Family Richness of Kapellos vs Gall bodies overall",
       x="Structure",
       y="Richness")



#Galled leaves vs ungalled leaves (just tree) 
#This measure is also of total richness not like per leaf since that's not how we
#measured them
richnessfam2<-data%>%
  filter(substrate=="galled_leaf" | substrate=="healthy_leaf")%>% #Filter to just leaves
  group_by(substrate)%>%
  summarise(richness = n_distinct(family[!is.na(family)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnessfam2, aes(substrate,richness,fill=substrate))+
  geom_bar(stat="identity")+
  theme_classic()+
  labs(title="Family Richness of galled leaves vs ungalled leaves",
       x="Structure",
       y="Richness")



#Galls vs leaves (just tree)
#I don't think this comparison should be made in the actual paper because
#galls and leaves didn't receive the same isolation effort
richnessfam3<-data%>%
  filter(data=="tree")%>%
  mutate(substrate = recode(substrate, #Group all leaves together and galls
                            "galled_leaf" = "leaf", "healthy_leaf"="leaf"
                            ,"gall_body"="gall","kapello"="gall"))%>%
  group_by(substrate) %>%
  summarise(richness = n_distinct(family[!is.na(family)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnessfam3, aes(substrate,richness,fill=substrate))+
  geom_bar(stat="identity")+
  theme_classic()+
  labs(title="Family Richness of leaves vs galls",
       x="Structure",
       y="Richness")




#Ground vs Tree (ground and tree)
richnessfam4<-data%>%
  filter(substrate!="galled_leaf"& substrate!="healthy_leaf")%>%
  mutate(location=recode(location,"log"="ground"))%>%
  group_by(isolation_id,location) %>%
  summarise(richness = n_distinct(family[!is.na(family)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnessfam4, aes(location,richness,fill=location))+
  geom_boxplot(width=.5)+
  geom_jitter(width = 0.2, alpha = 0.5)+
  theme_classic()+
  labs(title="Family Richness of tree galls vs ground galls",
       x="Location",
       y="Richness")



#Dispersed vs non dispersed (just ground)
#Aka found in logs vs found under trees on ground
richnessfam5<-data%>%
  filter(data=="ground")%>%
  group_by(isolation_id, location) %>%
  summarise(richness = n_distinct(family[!is.na(family)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnessfam5, aes(location,richness,fill=location))+
  geom_boxplot(width=0.5)+
  geom_jitter(width = 0.2, alpha = 0.5)+
  theme_classic()+
  labs(title="Family Richness of dispersed vs non dispersed",
       x="Location",
       y="Richness")


#Alive vs dead (just ground)
richnessfam6<-data%>%
  filter(data=="ground")%>%
  filter(!is.na(status))%>%
  group_by(status,isolation_id) %>%
  summarise(richness = n_distinct(family[!is.na(family)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnessfam6, aes(status,richness,fill=status))+
  geom_boxplot(width=0.5)+
  geom_jitter(width = 0.2, alpha = 0.5)+
  theme_classic()+
  labs(title="Family Richness of Live vs Dead galls",
       x="Status",
       y="Richness")

#Gall bodies of tree vs gall bodies of ground (tree and ground)
richnessfam7<-data%>%
  filter(substrate=="gall_body")%>%
  mutate(location=recode(location,"log"="ground"))%>%
  group_by(isolation_id,location) %>%
  summarise(richness = n_distinct(family[!is.na(family)]),
            .groups = "drop" #Makes sure 0 isolations are included
  )

ggplot(richnessfam7, aes(location,richness,fill=location))+
  geom_boxplot(width=.5)+
  geom_jitter(width = 0.2, alpha = 0.5)+
  theme_classic()+
  labs(title="Family Tree vs ground gall bodies",
       x="Status",
       y="Richness")



