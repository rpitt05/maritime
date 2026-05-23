#Biostats project structural equation modeling
#Rachel Pitt
#12/2/25


#Library
library(tidyverse)
library(lavaan)
library(semPlot)

#Data taken from biostats project variable and biostats project ordination
#reduced_all


#Rename columns for easier read
SEM_variables<-reduced_all%>%
  rename(
    TD=tree_density,
    CC=canopy_coverage,
    TSR=tree_species_richness,
    VO=overall_species_richness,
    VG=vegetation_cover,
    LN=needle_coverage,
    PO=percent_organic_matter_10cm,
    SS=soil_salinity_ppt_10cm,
    PW=soil_percent_water_10cm,
    MSR=plot_mushroom_richness,
    JD=julian_date
  )

#Write out the model based on the SEM diagram
mod1<-'
CC~TD
TSR~TD+CC
VO~CC
VG~VO
LN~CC
PO~VG+LN
PW~PO+JD
SS~PW+JD
MSR~JD+LN+SS+PW+TSR+PO
'

fit1<-sem(mod1, data=SEM_variables)

summary(fit1, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)

#Graph of mod1 SEM
semPaths(
  fit1,
  what = "std",
  layout = "spring",
  style = "lisrel",
  edge.label.cex = 0.9,
  edge.label.position = 0.65,
  residuals = FALSE,
  intercepts = FALSE,
  curvePivot = TRUE,
  fade = FALSE,
  sizeMan = 7,
  mar = c(6,6,6,6),
  nCharNodes = 999
)

#Reduce variables based on significance and create mod2
mod2<-'
LN~CC
PO~LN
MSR~PO+TSR+JD'

fit2<-sem(mod2, data=SEM_variables)
summary(fit2, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)

#Graph mod2 SEM
semPaths(
  fit2,
  what = "std",
  layout = "spring",
  style = "lisrel",
  edge.label.cex = 0.9,
  edge.label.position = 0.65,
  residuals = FALSE,
  intercepts = FALSE,
  curvePivot = TRUE,
  fade = FALSE,
  sizeMan = 7,
  mar = c(6,6,6,6),
  nCharNodes = 999
)


#Reduce some variables based on significance but retain biologically informed relationships
mod3<-'
CC~TD
TSR~TD+CC
LN~CC
PO~LN
MSR~TSR+PO+JD
'
fit3<-sem(mod3, data=SEM_variables)
summary(fit3, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)

#Graph mod3 SEM
semPaths(
  fit3,
  what = "std",
  layout = "spring",
  style = "lisrel",
  edge.label.cex = 0.9,
  edge.label.position = 0.65,
  residuals = FALSE,
  intercepts = FALSE,
  curvePivot = TRUE,
  fade = FALSE,
  sizeMan = 7,
  mar = c(6,6,6,6),
  nCharNodes = 999
)
