#NMDS attempt 1 via chatgpt
#11/25/25
#Rachel Pitt



library(tidyverse)
library(vegan)



wide <- mushroom_data_clean %>%
  dplyr::select(-family)%>%
  mutate(pres = 1) %>%
  distinct() %>%
  pivot_wider(names_from = species, values_from = pres, values_fill = 0)

species_mat <- wide %>% 
  dplyr::select(-site, -plot_number, -plot, -survey)

nmds1<-metaMDS(species_mat, k = 2, distance = "bray", trymax = 200)

scores <- as.data.frame(scores(nmds1))
plot_data <- bind_cols(wide %>% select(site, plot_number,survey), scores)





d <- vegdist(species_mat, method = "bray")

# PCoA (classical multidimensional scaling)
pco <- cmdscale(d, k = 2, eig = TRUE)
pco_scores <- as.data.frame(pco$points)
colnames(pco_scores) <- c("PCoA1", "PCoA2")
pco_scores$sample <- rownames(pco_scores)

# Combine with metadata (replace metadata name)
plotdata <- bind_cols(wide[rownames(pco_scores), ], pco_scores)

ggplot(plotdata, aes(PCoA1, PCoA2, color = site, shape = as.factor(survey))) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(color = "Site", shape = "Survey Day")




hell <- decostand(species_mat, method = "hellinger")
nmds2 <- metaMDS(hell, k = 2, trymax = 200)

scores_nmds <- as.data.frame(scores(nmds2))
scores_nmds$sample <- rownames(scores_nmds)

plotdata <- bind_cols(metadata[rownames(scores_nmds), ], scores_nmds)

ggplot(plotdata, aes(NMDS1, NMDS2, color = site, shape = as.factor(survey))) +
  geom_point(size = 3) +
  theme_minimal()
