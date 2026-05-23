#Honors thesis analyses jaccard PCOA (CMDS) SECOND TIME (uses ONE dataset)
#Rachel Pitt
#4/23/26

#Packages
library(tidyverse)
library(vegan)

#Data
data<-read.csv("ultimate_datasheet_v10.csv")%>%
  select(-X,-other_unite_id,-unite_accession,-species_unite,-perc_id,-align_length,-bitscore,-read_type)%>% #Don't need this metadata
  distinct(isolation_id,species, .keep_all=TRUE)%>% #Keep distinct
  mutate(location=sub("log ","log",location))%>%
  mutate(age=as.factor(age),
         tree_inat=as.factor(tree_inat),
         tree=as.factor(tree),
         log=as.factor(log))



#SPECIES LEVEL------------

#Gall body vs Kapello (just tree)
#Convert to presence absence
datsp1<-data%>% 
  filter(data=="tree")%>% #Subset to tree data
  filter(!is.na(species) & species!="")%>% #Omit NAs and blanks
  filter(substrate!="galled_leaf"&substrate!="healthy_leaf")%>% #No leaves
  mutate(presence=1)%>% #Presence
  pivot_wider(
    id_cols=c(isolation_id, substrate,group,tree,lat,lon,age),
    names_from=species,
    values_from=presence,
    values_fill=0 #Absence
  )

#MAT 1
matsp1<-datsp1%>% #Distance matrix without metadata
  select(-substrate,-isolation_id,-group,-tree,-lat,-lon,-age)%>%
  vegdist(method = "jaccard", binary = TRUE) #Jaccard and pres/abs

#PCOA 1
pcoasp1<-cmdscale(matsp1,eig=TRUE,k=2)

#Combine PCs with metadata 1
pcoa_scoressp1<-datsp1%>% 
  mutate(PC1=pcoasp1$points[,1],
         PC2=pcoasp1$points[,2])

#Get eigenvalues
eig<-pcoasp1$eig

#Make sure not negative
eig<-eig[eig>0]

#Variation explained
var_explained<-eig/sum(eig)

#Into percent for axes
pc1_var<-round(100*var_explained[1],1)
pc2_var<-round(100*var_explained[2],1)

#Visualize PCOA 1
ggplot(pcoa_scoressp1,aes(PC1,PC2,color=substrate))+geom_point()+
  theme_classic()+labs(title="Tree gall Communities- CMDS species Jaccard",
                       x = paste0("PC1 (", pc1_var, "%)"),
                       y = paste0("PC2 (", pc2_var, "%)"))




#Jaccard for galls found on ground
#Create presence absence matrix
datsp2<-data%>%
  filter(data=="ground")%>% #Filter to only ground data
  filter(!is.na(species) &species!="")%>% #Get rid of missing data
  filter(substrate!="seed" & substrate!="larvae" & substrate!="adult" & substrate!="uknown")%>% #get rid of seeds, larvae, and adults
  mutate(presence=1)%>% #Present
  pivot_wider(
    id_cols = c(gall_id, substrate,tree_inat,lat,lon,age,log,location,status),
    names_from=species,
    values_from=presence,
    values_fill=0 )#absence

#Distance matrix 2
matsp2<-datsp2%>% #Distance matrix without metadata
  select(-substrate,-gall_id,-tree_inat,-lat,-lon,-age,-log,-location,-status)%>%
  vegdist(method = "jaccard", binary = TRUE) #Jaccard and pres/abs

#PCOA 2
pcoasp2<-cmdscale(matsp2,eig=TRUE,k=2)

#Combine PCs with meta data 2
pcoa_scoressp2<-datsp2%>%
  mutate(PC1=pcoasp2$points[,1],
         PC2=pcoasp2$points[,2])
#Get eigenvalues
eig<-pcoasp2$eig

#Make sure not negative
eig<-eig[eig>0]

#Variation explained
var_explained<-eig/sum(eig)

#Into percent for axes
pc1_var<-round(100*var_explained[1],1)
pc2_var<-round(100*var_explained[2],1)

#Visualize PCOA 2
ggplot(pcoa_scoressp2,aes(PC1,PC2,color=location,shape=status))+geom_point()+
  scale_shape_manual(values=c(15,17))+
  theme_classic()+labs(title="Ground gall Communities- CMDS species Jaccard",
                       x = paste0("PC1 (", pc1_var, "%)"),
                       y = paste0("PC2 (", pc2_var, "%)"))


#Examine all substrates from ground (adult,larvae,seed,gall_body,kapello)
datsp2sub<-data%>%
  filter(data=="ground")%>% #Filter to only ground data
  filter(!is.na(species) &species!="")%>% #Get rid of missing data
  filter (substrate!="uknown")%>%
  mutate(presence=1)%>% #Present
  pivot_wider(
    id_cols = c(isolation_id, substrate,tree_inat,lat,lon,age,log,location,status),
    names_from=species,
    values_from=presence,
    values_fill=0 )#absence

matsp2sub<-datsp2sub%>% #Distance matrix without metadata
  select(-substrate,-isolation_id,-tree_inat,-lat,-lon,-age,-log,-location,-status)%>%
  vegdist(method = "jaccard", binary = TRUE) #Jaccard and pres/abs


#PCOA 2sub
pcoasp2sub<-cmdscale(matsp2sub,eig=TRUE,k=2)

#Combine PCs with meta data 2sub
pcoa_scoressp2sub<-datsp2sub%>%
  mutate(PC1=pcoasp2sub$points[,1],
         PC2=pcoasp2sub$points[,2])
#Get eigenvalues
eig<-pcoasp2sub$eig

#Make sure not negative
eig<-eig[eig>0]

#Variation explained
var_explained<-eig/sum(eig)

#Into percent for axes
pc1_var<-round(100*var_explained[1],1)
pc2_var<-round(100*var_explained[2],1)

#Visualize PCOA 2sub
ggplot(pcoa_scoressp2sub,aes(PC1,PC2,color=location))+geom_point()+
  theme_classic()+labs(title="Ground gall Communities- CMDS species Jaccard",
                       x = paste0("PC1 (", pc1_var, "%)"),
                       y = paste0("PC2 (", pc2_var, "%)"))




#Jaccard between tree fungi/ground fungi  3
datsp3<-data%>% #Create presence absence matrix
  filter(!is.na(species) &species!="")%>% #Filter missing
  filter(substrate!="seed" & substrate!="larvae" & substrate!="adult" & substrate!="uknown"&substrate!="galled_leaf"&substrate!="healthy_leaf")%>% #Filter out substrates
  mutate(presence=1)%>% #present
  pivot_wider(
    id_cols = c(gall_id, substrate,tree_inat,lat,lon,age,log,location,status,group,data),
    names_from=species,
    values_from=presence,
    values_fill=0 #absent
  )

#Distance  matrix 3
matsp3<-datsp3%>%
  select(-substrate,-gall_id,-location,-status,-tree_inat,-lat,-lon,-age,-log,-group,-data)%>%
  vegdist(method = "jaccard", binary = TRUE)

#PCOA 3
pcoasp3<-cmdscale(matsp3,eig=TRUE,k=2)

#Join PCs to metadata 3
pcoa_scoressp3<-datsp3%>%
  mutate(PC1=pcoasp3$points[,1],
         PC2=pcoasp3$points[,2])
#Get eigenvalues
eig<-pcoasp3$eig

#Make sure not negative
eig<-eig[eig>0]

#Variation explained
var_explained<-eig/sum(eig)

#Into percent for axes
pc1_var<-round(100*var_explained[1],1)
pc2_var<-round(100*var_explained[2],1)

ggplot(pcoa_scoressp3,aes(PC1,PC2,color=data))+geom_point()+
  theme_classic()+labs(title="All gall Communities- CMDS species Jaccard",
                       x = paste0("PC1 (", pc1_var, "%)"),
                       y = paste0("PC2 (", pc2_var, "%)"))






#GENUS LEVEL------------

#Gall body vs Kapello (just tree)
#Convert to presence absence
datgen1<-data%>% 
  filter(data=="tree")%>% #Subset to tree data
  distinct(isolation_id,genus, .keep_all=TRUE)%>% #DISTINCT BY GENUS
  filter(!is.na(genus) & genus!="")%>% #Omit NAs and blanks
  filter(substrate!="galled_leaf"&substrate!="healthy_leaf")%>% #No leaves
  mutate(presence=1)%>% #Presence
  pivot_wider(
    id_cols=c(isolation_id, substrate,group,tree_inat,lat,lon,age),
    names_from=genus,
    values_from=presence,
    values_fill=0 #Absence
  )

#MAT 1
matgen1<-datgen1%>% #Distance matrix without metadata
  select(-substrate,-isolation_id,-group,-tree_inat,-lat,-lon,-age)%>%
  vegdist(method = "jaccard", binary = TRUE) #Jaccard and pres/abs

#PCOA 1
pcoagen1<-cmdscale(matgen1,eig=TRUE,k=2)

#Combine PCs with metadata 1
pcoa_scoresgen1<-datgen1%>% 
  mutate(PC1=pcoagen1$points[,1],
         PC2=pcoagen1$points[,2])
#Get eigenvalues
eig<-pcoagen1$eig

#Make sure not negative
eig<-eig[eig>0]

#Variation explained
var_explained<-eig/sum(eig)

#Into percent for axes
pc1_var<-round(100*var_explained[1],1)
pc2_var<-round(100*var_explained[2],1)

#Visualize PCOA 1
ggplot(pcoa_scoresgen1,aes(PC1,PC2,color=substrate))+geom_point()+
  theme_classic()+labs(title="Tree gall Communities- CMDS genus Jaccard",
                       x = paste0("PC1 (", pc1_var, "%)"),
                       y = paste0("PC2 (", pc2_var, "%)"))




#Jaccard for galls found on ground
#Create presence absence matrix
datgen2<-data%>%
  filter(data=="ground")%>% #Filter to only ground data
  distinct(isolation_id,genus, .keep_all=TRUE)%>%
  filter(!is.na(genus) &genus!="")%>% #Get rid of missing data
  filter(substrate!="seed" & substrate!="larvae" & substrate!="adult" & substrate!="uknown")%>% #get rid of seeds, larvae, and adults
  mutate(presence=1)%>% #Present
  pivot_wider(
    id_cols = c(isolation_id, substrate,tree_inat,lat,lon,age,log,location,status),
    names_from=genus,
    values_from=presence,
    values_fill=0 )#absence

#Distance matrix 2
matgen2<-datgen2%>% #Distance matrix without metadata
  select(-substrate,-isolation_id,-tree_inat,-lat,-lon,-age,-log,-location,-status)%>%
  vegdist(method = "jaccard", binary = TRUE) #Jaccard and pres/abs

#PCOA 2
pcoagen2<-cmdscale(matgen2,eig=TRUE,k=2)

#Combine PCs with meta data 2
pcoa_scoresgen2<-datgen2%>%
  mutate(PC1=pcoagen2$points[,1],
         PC2=pcoagen2$points[,2])

#Get eigenvalues
eig<-pcoagen2$eig

#Make sure not negative
eig<-eig[eig>0]

#Variation explained
var_explained<-eig/sum(eig)

#Into percent for axes
pc1_var<-round(100*var_explained[1],1)
pc2_var<-round(100*var_explained[2],1)

#Visualize PCOA 2
ggplot(pcoa_scoresgen2,aes(PC1,PC2,color=location))+geom_point()+
  theme_classic()+labs(title="Ground gall Communities- CMDS genus Jaccard",
                       x = paste0("PC1 (", pc1_var, "%)"),
                       y = paste0("PC2 (", pc2_var, "%)"))


#Examine all substrates from ground (adult,larvae,seed,gall_body,kapello)
datgen2sub<-data%>%
  filter(data=="ground")%>% #Filter to only ground data
  filter(!is.na(genus) &genus!="")%>% #Get rid of missing data
  distinct(isolation_id, genus, .keep_all = TRUE) %>%
  filter (substrate!="uknown")%>%
  mutate(presence=1)%>% #Present
  pivot_wider(
    id_cols = c(isolation_id, substrate,tree_inat,lat,lon,age,log,location,status),
    names_from=genus,
    values_from=presence,
    values_fill=list(presence=0) )#absence

matgen2sub<-datgen2sub%>% #Distance matrix without metadata
  select(-substrate,-isolation_id,-tree_inat,-lat,-lon,-age,-log,-location,-status)%>%
  vegdist(method = "jaccard", binary = TRUE) #Jaccard and pres/abs


#PCOA 2sub
pcoagen2sub<-cmdscale(matgen2sub,eig=TRUE,k=2)

#Combine PCs with meta data 2sub
pcoa_scoresgen2sub<-datgen2sub%>%
  mutate(PC1=pcoasp2sub$points[,1],
         PC2=pcoasp2sub$points[,2])

#Get eigenvalues
eig<-pcoagen2sub$eig

#Make sure not negative
eig<-eig[eig>0]

#Variation explained
var_explained<-eig/sum(eig)

#Into percent for axes
pc1_var<-round(100*var_explained[1],1)
pc2_var<-round(100*var_explained[2],1)

#Visualize PCOA 2sub
ggplot(pcoa_scoressp2sub,aes(PC1,PC2,color=substrate))+geom_point()+
  theme_classic()+labs(title="Ground gall (all substrates)- CMDS genus Jaccard",
                       x = paste0("PC1 (", pc1_var, "%)"),
                       y = paste0("PC2 (", pc2_var, "%)"))



#Jaccard between tree fungi/ground fungi  3
datgen3<-data%>% #Create presence absence matrix
  distinct(isolation_id,genus, .keep_all=TRUE)%>%
  filter(!is.na(genus) &genus!="")%>% #Filter missing
  filter(substrate!="seed" & substrate!="larvae" & substrate!="adult" & substrate!="uknown"&substrate!="galled_leaf"&substrate!="healthy_leaf")%>% #Filter out substrates
  mutate(presence=1)%>% #present
  pivot_wider(
    id_cols = c(gall_id, substrate,tree_inat,lat,lon,age,log,location,status,group,data),
    names_from=genus,
    values_from=presence,
    values_fill=0 #absent
  )

#Distance  matrix 3
matgen3<-datgen3%>%
  select(-substrate,-gall_id,-location,-status,-tree_inat,-lat,-lon,-age,-log,-group,-data)%>%
  vegdist(method = "jaccard", binary = TRUE)

#PCOA 3
pcoagen3<-cmdscale(matgen3,eig=TRUE,k=2)

#Join PCs to metadata 3
pcoa_scoresgen3<-datgen3%>%
  mutate(PC1=pcoagen3$points[,1],
         PC2=pcoagen3$points[,2])

#Get eigenvalues
eig<-pcoagen3$eig

#Make sure not negative
eig<-eig[eig>0]

#Variation explained
var_explained<-eig/sum(eig)

#Into percent for axes
pc1_var<-round(100*var_explained[1],1)
pc2_var<-round(100*var_explained[2],1)

ggplot(pcoa_scoresgen3,aes(PC1,PC2,color=data))+geom_point()+
  theme_classic()+labs(title="All gall Communities- CMDS genus Jaccard",
                       x = paste0("PC1 (", pc1_var, "%)"),
                       y = paste0("PC2 (", pc2_var, "%)"))








#FAMILY LEVEL----------------
#Gall body vs Kapello (just tree)
#Convert to presence absence
datfam1<-data%>% 
  filter(data=="tree")%>% #Subset to tree data
  distinct(isolation_id,family, .keep_all=TRUE)%>% #DISTINCT BY FAMILY
  filter(!is.na(family) & family!="")%>% #Omit NAs and blanks
  filter(substrate!="galled_leaf"&substrate!="healthy_leaf")%>% #No leaves
  mutate(presence=1)%>% #Presence
  pivot_wider(
    id_cols=c(isolation_id, substrate,group,tree_inat,lat,lon,age),
    names_from=family,
    values_from=presence,
    values_fill=0 #Absence
  )

#MAT 1
matfam1<-datfam1%>% #Distance matrix without metadata
  select(-substrate,-isolation_id,-group,-tree_inat,-lat,-lon,-age)%>%
  vegdist(method = "jaccard", binary = TRUE) #Jaccard and pres/abs

#PCOA 1
pcoafam1<-cmdscale(matfam1,eig=TRUE,k=2)

#Combine PCs with metadata 1
pcoa_scoresfam1<-datfam1%>% 
  mutate(PC1=pcoafam1$points[,1],
         PC2=pcoafam1$points[,2])
#Get eigenvalues
eig<-pcoafam1$eig

#Make sure not negative
eig<-eig[eig>0]

#Variation explained
var_explained<-eig/sum(eig)

#Into percent for axes
pc1_var<-round(100*var_explained[1],1)
pc2_var<-round(100*var_explained[2],1)

#Visualize PCOA 1
ggplot(pcoa_scoresfam1,aes(PC1,PC2,color=substrate))+geom_point()+
  theme_classic()+labs(title="Tree gall Communities- CMDS family Jaccard",
                       x = paste0("PC1 (", pc1_var, "%)"),
                       y = paste0("PC2 (", pc2_var, "%)"))




#Jaccard for galls found on ground
#Create presence absence matrix
datfam2<-data%>%
  filter(data=="ground")%>% #Filter to only ground data
  distinct(isolation_id,family, .keep_all=TRUE)%>%
  filter(!is.na(family) &family!="")%>% #Get rid of missing data
  filter(substrate!="seed" & substrate!="larvae" & substrate!="adult" & substrate!="uknown")%>% #get rid of seeds, larvae, and adults
  mutate(presence=1)%>% #Present
  pivot_wider(
    id_cols = c(isolation_id, substrate,tree_inat,lat,lon,age,log,location,status),
    names_from=family,
    values_from=presence,
    values_fill=0 )#absence

#Distance matrix 2
matfam2<-datfam2%>% #Distance matrix without metadata
  select(-substrate,-isolation_id,-tree_inat,-lat,-lon,-age,-log,-location,-status)%>%
  vegdist(method = "jaccard", binary = TRUE) #Jaccard and pres/abs

#PCOA 2
pcoafam2<-cmdscale(matfam2,eig=TRUE,k=2)

#Combine PCs with meta data 2
pcoa_scoresfam2<-datfam2%>%
  mutate(PC1=pcoafam2$points[,1],
         PC2=pcoafam2$points[,2])

#Get eigenvalues
eig<-pcoafam2$eig

#Make sure not negative
eig<-eig[eig>0]

#Variation explained
var_explained<-eig/sum(eig)

#Into percent for axes
pc1_var<-round(100*var_explained[1],1)
pc2_var<-round(100*var_explained[2],1)

#Visualize PCOA 2
ggplot(pcoa_scoresfam2,aes(PC1,PC2,color=location))+geom_point()+
  theme_classic()+labs(title="Ground gall Communities- CMDS family Jaccard",
                       x = paste0("PC1 (", pc1_var, "%)"),
                       y = paste0("PC2 (", pc2_var, "%)"))




#Jaccard between tree fungi/ground fungi  3
datfam3<-data%>% #Create presence absence matrix
  distinct(isolation_id,family, .keep_all=TRUE)%>%
  filter(!is.na(family) &family!="")%>% #Filter missing
  filter(substrate!="seed" & substrate!="larvae" & substrate!="adult" & substrate!="uknown"&substrate!="galled_leaf"&substrate!="healthy_leaf")%>% #Filter out substrates
  mutate(presence=1)%>% #present
  pivot_wider(
    id_cols = c(isolation_id, substrate,tree_inat,lat,lon,age,log,location,status,group,data),
    names_from=family,
    values_from=presence,
    values_fill=0 #absent
  )

#Distance  matrix 3
matfam3<-datfam3%>%
  select(-substrate,-isolation_id,-location,-status,-tree_inat,-lat,-lon,-age,-log,-group,-data)%>%
  vegdist(method = "jaccard", binary = TRUE)

#PCOA 3
pcoafam3<-cmdscale(matfam3,eig=TRUE,k=2)

#Join PCs to metadata 3
pcoa_scoresfam3<-datfam3%>%
  mutate(PC1=pcoafam3$points[,1],
         PC2=pcoafam3$points[,2])

#Get eigenvalues
eig<-pcoafam3$eig

#Make sure not negative
eig<-eig[eig>0]

#Variation explained
var_explained<-eig/sum(eig)

#Into percent for axes
pc1_var<-round(100*var_explained[1],1)
pc2_var<-round(100*var_explained[2],1)

ggplot(pcoa_scoresfam3,aes(PC1,PC2,color=data))+geom_point()+
  theme_classic()+labs(title="All gall Communities- CMDS family Jaccard",
                       x = paste0("PC1 (", pc1_var, "%)"),
                       y = paste0("PC2 (", pc2_var, "%)"))





