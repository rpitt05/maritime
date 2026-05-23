#Connect sample's closest accession with unite blast
#Rachel Pitt
#4/23/26


#Packages
library(tidyverse)

#Data
tree<-read.csv("gall_master_datasheet_v23.csv")%>%
  mutate(isolation_id=paste0("t",isolation_id),
         fungal_id=paste0("t",fungal_id),
         gall_id=paste0("t",gall_id))%>%
  mutate(fungal_id=sub("tNA",NA,fungal_id))


ant_isolation<-read.csv("ant_nest_isolations_v7.csv")
ant_cultures<-read.csv("ant_nest_cultures_v7.csv")
ground<-left_join(ant_isolation, ant_cultures, by="isolation_id")%>%
  mutate(isolation_id=paste0("g",isolation_id),
         fungal_id=paste0("g",fungal_id),
         fungal_id=sub("gNA",NA,fungal_id),
         gall_id=paste0("g",gall_id))%>%
  mutate(
    substrate = case_when(
      str_detect(isolation_id, "B$") ~ "gall_body",
      str_detect(isolation_id, "K$") ~ "kapello",
      str_detect(isolation_id, "A$") ~ "adult",
      str_detect(isolation_id, "L$") ~ "larvae",
      str_detect(isolation_id,"S$") ~ "seed",
      TRUE ~ NA_character_
    )
  )
  

#Join together and make sure they have the same order of columns
t<-tree%>%
  mutate(log=NA)%>%
  mutate(location=rep("tree"))%>%
  mutate(status=rep("live"))%>%
  mutate(age=rep(0))%>%
  mutate(data="tree")%>%
  select(substrate, gall_id,isolation_id, fungal_id, sequence_id,group,tree_inat,lat,lon,log,location,status,age,data,species_unite,unite_accession)

g<-ground%>%
  mutate(group=NA)%>%
  mutate(data="ground",
         sequence_id=extraction_id)%>%
  select(substrate,gall_id,isolation_id, fungal_id, sequence_id,group,tree_inat,lat,lon,log,location,status,age,data,species_unite,unite_accession)

#Join
both<-rbind(t,g)


#Unite blast
first<-read.csv("first_local_blast_results.csv")
second<-read.csv("second_local_blast_results.csv")
third<-read.csv("third_local_blast_results.csv")
unite1<-rbind(first,second)
unite<-rbind(unite1,third)
unique(unite$Column1)

#Clean ids
unite$Column1<-sub("_Assembly_consensus_sequence","",unite$Column1)
for(i in 1:length(unite$Column1)){
  if (startsWith(unite$Column1[i],"D")){
    unite$Column1[i]<-sub("-NL4.*","-NL4",unite$Column1[i])
    unite$Column1[i]<-sub("-ITS.*","-ITS",unite$Column1[i])
    unite$Column1[i]<-sub("_NL4.*","-NL4",unite$Column1[i])
    unite$Column1[i]<-sub("_ITS.*","-ITS",unite$Column1[i])
    unite$Column1[i]<-sub("D-","",unite$Column1[i])
    unite$Column1[i]<-sub("-","_",unite$Column1[i])
  }
  if(startsWith(unite$Column1[i],"G")){
    unite$Column1[i]<-sub("_RP.*","",unite$Column1[i])
    unite$Column1[i]<-sub("__R.*","-NL4",unite$Column1[i])
    unite$Column1[i]<-sub("__F.*","-ITS",unite$Column1[i])
  }
  unite$Column1[i]<-sub("-NL4.*","-NL4",unite$Column1[i])
  unite$Column1[i]<-sub("-ITS.*","-ITS",unite$Column1[i])
  unite$Column1[i]<-sub("_NL4.*","-NL4",unite$Column1[i])
  unite$Column1[i]<-sub("_ITS.*","-ITS",unite$Column1[i])
  unite$Column1[i]<-sub("_Assembly","",unite$Column1[i])
  if(startsWith(unite$Column1[i],"G")){
    unite$Column1[i]<-sub("_Consensus","",unite$Column1[i])
    unite$Column1[i]<-sub("_ForwardONLY","-ITS",unite$Column1[i])
    unite$Column1[i]<-sub("_ReverseONLY","-NL4",unite$Column1[i])
    unite$Column1[i]<-sub("_ReverseOnly","-NL4",unite$Column1[i])
    unite$Column1[i]<-sub("_ForwardOnly","-ITS",unite$Column1[i])
  }
}





#Record whether it was a single read or consensus
unite$read_type <- "consensus"
unite$read_type[grepl("-NL4$", unite$Column1)] <- "single read NL4"
unite$read_type[grepl("-ITS$", unite$Column1)] <- "single read ITS"
unite$Column1<-sub("-ITS","",unite$Column1)
unite$Column1<-sub("-NL4","",unite$Column1)


#Change the G 2024 ids to regular notation
is_G <- grepl("^G", unite$Column1)


unite$Column1[is_G] <- paste0(
  "0_",
  sprintf("%02d", as.numeric(sub("^G", "", unite$Column1[is_G])))
)




unique(unite$Column1)

#Now separate species and accession
unite_sep <- unite %>%
  separate(Column2, into = c("species_unite", "unite_accession", "other_unite_id", "status", "taxonomy"), sep = "\\|") %>%
  separate(taxonomy, into = c("kingdom", "phylum", "class", "order", "family", "genus", "species"), sep = ";") %>%
  mutate(
    across(kingdom:species, ~ sub("^[a-z]__", "", .))
  )%>%
  dplyr::rename(
    sequence_id=Column1
  )%>%
  select(-status)%>%
  dplyr::rename(perc_id=Column3,
         align_length=Column4,
         bitscore=Column12)%>%
  select(-X,-Column5,-Column6,-Column7,-Column8,-Column9,-Column10,-Column11)




#Join together the blasts and metadata
joined<-left_join(both,unite_sep,by=c("sequence_id","species_unite","unite_accession"))%>%
  mutate(tree = as.numeric(fct_inorder(as.character(tree_inat))))%>%
  group_by(sequence_id) %>%
  slice_max(order_by = perc_id, n = 1, with_ties = FALSE) %>%
  ungroup()


view(joined)


write.csv(joined,"ultimate_datasheet_v11.csv")




