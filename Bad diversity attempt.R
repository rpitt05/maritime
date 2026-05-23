#Diversity analysis
#Rachel Pitt
#11/4/25


library(tidyverse)



#Get mushroom data
mushroom_data_clean


#Get counts of unique species per plot in wide format
mushroom<-mushroom_data_clean %>%
  select(-site,-plot,-plot_number)%>%
  mutate(presence= 1) %>%
  distinct(survey, species, .keep_all=TRUE)%>%
  pivot_wider(
    names_from=species,
    values_from=presence,
    values_fill=0
  )

heatmap_data <- mushroom %>%
  pivot_longer(
    names_to = "species",
    values_to = "presence"
  )

ggplot(heatmap_data, aes(x = species, y = survey, fill = factor(presence))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "white", "1" = "darkgreen")) +
  theme_minimal() +
  labs(
    x = "Species",
    y = "Survey Day",
    fill = "Presence",
    title = "Species Presence by Survey Day"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )
ggplot(mushroom, aes())


