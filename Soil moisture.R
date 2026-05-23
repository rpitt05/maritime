#Soil by Rachel Pitt
#10/8/25

#This is my second attempt at analyzing the soil data. The first time I did a bunch of 
#Calculations in a googlesheet but realized I would much rather do them in R.
#I also used a package to pull from a googlesheet, but I think I would rather same them
#as csvs and update the version whenever I get new data.


#Packages
library(tidyverse)
library(ggplot2)


#Download data
soil<-read.csv("new_soil_v1.csv")


#Calculate soil moisture
soil_moisture<-soil %>%
  mutate(wet_soil_weight_g=(combined_wet_weight_g-beaker_weight_g))%>%
  mutate(dry_soil_weight_g=(combined_dry_weight_g-beaker_weight_g))%>%
  mutate(water_volume_mL=(wet_soil_weight_g-dry_soil_weight_g))%>%
  mutate(percent_water_content=(water_volume_mL/wet_soil_weight_g)*100)


#Lets look at histograms of the average soil percent water content of each plot at 10cm
#Although I need to keep in mind it is not completely relevant to average together soil moisture of different days
#Because that can really vary depending on rainfall. I think averaging them together is relevant is some 
#scenarios if I'm comparing moisture of a site to grain size. But also keeping in mind rainfall for those
#comparisons I think is important. Or maybe I can do relative moisture somehow?

#How can I calculate a relative soil moisture for each of the sample days? This might make it easier to compare somehow?

#Site 1 plot 1 (plot number 1) percent water content
ggplot(filter(soil_moisture, plot_number=="1"), aes(percent_water_content))+geom_histogram(fill="red")+labs(title="Site 1 plot 1 (plot number 1) percent water content")

#Site 1 plot 2 (plot number 2) percent water content
ggplot(filter(soil_moisture, plot_number=="2"), aes(percent_water_content))+geom_histogram(fill="red")+labs(title="Site 1 plot 2 (plot number 2) percent water content")

#Site 1 plot 3 (plot number 3) percent water content
ggplot(filter(soil_moisture, plot_number=="3"), aes(percent_water_content))+geom_histogram(fill="red")+labs(title="Site 1 plot 3 (plot number 3) percent water content")

#Site 2 plot 1 (plot number 4) percent water content
ggplot(filter(soil_moisture, plot_number=="4"), aes(percent_water_content))+geom_histogram(fill="orange")+labs(title="Site 2 plot 1 (plot number 4) percent water content")

#Site 2 plot 2 (plot number 5) percent water content
ggplot(filter(soil_moisture, plot_number=="5"), aes(percent_water_content))+geom_histogram(fill="orange")+labs(title="Site 2 plot 2 (plot number 5) percent water content")

#Site 2 plot 3 (plot number 6) percent water content
ggplot(filter(soil_moisture, plot_number=="6"), aes(percent_water_content))+geom_histogram(fill="orange")+labs(title="Site 2 plot 3 (plot number 6) percent water content")

#Site 3 plot 1 (plot number 7) percent water content
ggplot(filter(soil_moisture, plot_number=="7"), aes(percent_water_content))+geom_histogram(fill="yellow")+labs(title="Site 3 plot 1 (plot number 7) percent water content")

#Site 3 plot 2 (plot number 8) percent water content
ggplot(filter(soil_moisture, plot_number=="8"), aes(percent_water_content))+geom_histogram(fill="yellow")+labs(title="Site 3 plot 2 (plot number 8) percent water content")

#Site 3 plot 3 (plot number 9) percent water content
ggplot(filter(soil_moisture, plot_number=="9"), aes(percent_water_content))+geom_histogram(fill="yellow")+labs(title="Site 3 plot 3 (plot number 9) percent water content")

#Site 4 plot 1 (plot number 10) percent water content
ggplot(filter(soil_moisture, plot_number=="10"), aes(percent_water_content))+geom_histogram(fill="green")+labs(title="Site 4 plot 1 (plot number 10) percent water content")

#Site 4 plot 2 (plot number 11) percent water content
ggplot(filter(soil_moisture, plot_number=="11"), aes(percent_water_content))+geom_histogram(fill="green")+labs(title="Site 4 plot 2 (plot number 11) percent water content")

#Site 4 plot 3 (plot number 12) percent water content
ggplot(filter(soil_moisture, plot_number=="12"), aes(percent_water_content))+geom_histogram(fill="green")+labs(title="Site 4 plot 3 (plot number 12) percent water content")

#Site 5 plot 1 (plot number 13) percent water content
ggplot(filter(soil_moisture, plot_number=="13"), aes(percent_water_content))+geom_histogram(fill="blue")+labs(title="Site 5 plot 1 (plot number 13) percent water content")

#Site 5 plot 2 (plot number 14) percent water content
ggplot(filter(soil_moisture, plot_number=="14"), aes(percent_water_content))+geom_histogram(fill="blue")+labs(title="Site 5 plot 2 (plot number 14) percent water content")

#Site 5 plot 3 (plot number 15) percent water content
ggplot(filter(soil_moisture, plot_number=="15"), aes(percent_water_content))+geom_histogram(fill="blue")+labs(title="Site 5 plot 3 (plot number 15) percent water content")

#Site 6 plot 1 (plot number 16) percent water content
ggplot(filter(soil_moisture, plot_number=="16"), aes(percent_water_content))+geom_histogram(fill="purple")+labs(title="Site 6 plot 1 (plot number 16) percent water content")

#Site 6 plot 2 (plot number 17) percent water content
ggplot(filter(soil_moisture, plot_number=="17"), aes(percent_water_content))+geom_histogram(fill="purple")+labs(title="Site 6 plot 2 (plot number 17) percent water content")

#Site 6 plot 3 (plot number 18) percent water content
ggplot(filter(soil_moisture, plot_number=="18"), aes(percent_water_content))+geom_histogram(fill="purple")+labs(title="Site 6 plot 3 (plot number 18) percent water content")


#Histograms of soil moisture per site

#Site 1 percent water content
ggplot(filter(soil_moisture, site=="1"), aes(percent_water_content))+geom_histogram(fill="red")+labs(title="Site 1 percent water content")

#Site 2 percent water content
ggplot(filter(soil_moisture, site=="2"), aes(percent_water_content))+geom_histogram(fill="orange")+labs(title="Site 2 percent water content")

#Site 3 percent water content
ggplot(filter(soil_moisture, site=="3"), aes(percent_water_content))+geom_histogram(fill="yellow")+labs(title="Site 3 percent water content")

#Site 4 percent water content
ggplot(filter(soil_moisture, site=="4"), aes(percent_water_content))+geom_histogram(fill="green")+labs(title="Site 4 percent water content")

#Site 5 percent water content
ggplot(filter(soil_moisture, site=="5"), aes(percent_water_content))+geom_histogram(fill="blue")+labs(title="Site 5 percent water content")

#Site 6 percent water content
ggplot(filter(soil_moisture, site=="6"), aes(percent_water_content))+geom_histogram(fill="purple")+labs(title="Site 6 percent water content")


#Soil moisture of each plot on each sampling day

#Site 1 plot 1 (plot number 1) percent water content per survey day
ggplot(filter(soil_moisture, plot_number=="1"), aes(survey, percent_water_content))+geom_bar(stat="identity", fill="red")+labs(title="Site 1 plot 1 (plot number 1) percent water content per survey day")

#Site 1 plot 2 (plot number 2) percent water content per survey day
ggplot(filter(soil_moisture, plot_number=="2"), aes(survey, percent_water_content))+geom_bar(stat="identity", fill="red")+labs(title="Site 1 plot 2 (plot number 2) percent water content per survey day")

#Site 1 plot 3 (plot number 3) percent water content per survey day
ggplot(filter(soil_moisture, plot_number=="3"), aes(survey, percent_water_content))+geom_bar(stat="identity", fill="red")+labs(title="Site 1 plot 3 (plot number 3) percent water content per survey day")

#Site 2 plot 1 (plot number 4) percent water content per survey day
ggplot(filter(soil_moisture, plot_number=="4"), aes(survey, percent_water_content))+geom_bar(stat="identity", fill="orange")+labs(title="Site 2 plot 1 (plot number 4) percent water content per survey day")

#Site 2 plot 2 (plot number 5) percent water content per survey day
ggplot(filter(soil_moisture, plot_number=="5"), aes(survey, percent_water_content))+geom_bar(stat="identity", fill="orange")+labs(title="Site 2 plot 2 (plot number 5) percent water content per survey day")

#Site 2 plot 3 (plot number 6) percent water content per survey day
ggplot(filter(soil_moisture, plot_number=="6"), aes(survey, percent_water_content))+geom_bar(stat="identity", fill="orange")+labs(title="Site 2 plot 3 (plot number 6) percent water content per survey day")

#Site 3 plot 1 (plot number 7) percent water content per survey day
ggplot(filter(soil_moisture, plot_number=="7"), aes(survey, percent_water_content))+geom_bar(stat="identity", fill="yellow")+labs(title="Site 3 plot 1 (plot number 7) percent water content per survey day")

#Site 3 plot 2 (plot number 8) percent water content per survey day
ggplot(filter(soil_moisture, plot_number=="8"), aes(survey, percent_water_content))+geom_bar(stat="identity", fill="yellow")+labs(title="Site 3 plot 2 (plot number 8) percent water content per survey day")

#Site 3 plot 3 (plot number 9) percent water content per survey day
ggplot(filter(soil_moisture, plot_number=="9"), aes(survey, percent_water_content))+geom_bar(stat="identity", fill="yellow")+labs(title="Site 3 plot 3 (plot number 9) percent water content per survey day")

#Site 4 plot 1 (plot number 10) percent water content per survey day
ggplot(filter(soil_moisture, plot_number=="10"), aes(survey, percent_water_content))+geom_bar(stat="identity", fill="green")+labs(title="Site 4 plot 1 (plot number 10) percent water content per survey day")

#Site 4 plot 2 (plot number 11) percent water content per survey day
ggplot(filter(soil_moisture, plot_number=="11"), aes(survey, percent_water_content))+geom_bar(stat="identity", fill="green")+labs(title="Site 4 plot 2 (plot number 11) percent water content per survey day")

#Site 4 plot 3 (plot number 12) percent water content per survey day
ggplot(filter(soil_moisture, plot_number=="12"), aes(survey, percent_water_content))+geom_bar(stat="identity", fill="green")+labs(title="Site 4 plot 3 (plot number 12) percent water content per survey day")

#Site 5 plot 1 (plot number 13) percent water content per survey day
ggplot(filter(soil_moisture, plot_number=="13"), aes(survey, percent_water_content))+geom_bar(stat="identity", fill="blue")+labs(title="Site 5 plot 1 (plot number 13) percent water content per survey day")

#Site 5 plot 2 (plot number 14) percent water content per survey day
ggplot(filter(soil_moisture, plot_number=="14"), aes(survey, percent_water_content))+geom_bar(stat="identity", fill="blue")+labs(title="Site 5 plot 2 (plot number 14) percent water content per survey day")

#Site 5 plot 3 (plot number 15) percent water content per survey day
ggplot(filter(soil_moisture, plot_number=="15"), aes(survey, percent_water_content))+geom_bar(stat="identity", fill="blue")+labs(title="Site 5 plot 3 (plot number 15) percent water content per survey day")

#Site 6 plot 1 (plot number 16) percent water content per survey day
ggplot(filter(soil_moisture, plot_number=="16"), aes(survey, percent_water_content))+geom_bar(stat="identity", fill="purple")+labs(title="Site 6 plot 1 (plot number 16) percent water content per survey day")

#Site 6 plot 2 (plot number 17) percent water content per survey day
ggplot(filter(soil_moisture, plot_number=="17"), aes(survey, percent_water_content))+geom_bar(stat="identity", fill="purple")+labs(title="Site 6 plot 2 (plot number 17) percent water content per survey day")

#Site 6 plot 3 (plot number 18) percent water content per survey day
ggplot(filter(soil_moisture, plot_number=="18"), aes(survey, percent_water_content))+geom_bar(stat="identity", fill="purple")+labs(title="Site 6 plot 3 (plot number 18) percent water content per survey day")


#Percent water content sites on each day

#Site 1 percent water content per survey day
ggplot(filter(soil_moisture, site=="1"), aes(survey, percent_water_content))+geom_bar(stat="identity", fill="red")+labs(title="Site 1 percent water content per survey day")

#Site 2 percent water content per survey day
ggplot(filter(soil_moisture, site=="2"), aes(survey, percent_water_content))+geom_bar(stat="identity", fill="orange")+labs(title="Site 2 percent water content per survey day")

#Site 3 percent water content per survey day
ggplot(filter(soil_moisture, site=="3"), aes(survey, percent_water_content))+geom_bar(stat="identity", fill="yellow")+labs(title="Site 3 percent water content per survey day")

#Site 4 percent water content per survey day
ggplot(filter(soil_moisture, site=="4"), aes(survey, percent_water_content))+geom_bar(stat="identity", fill="green")+labs(title="Site 4 percent water content per survey day")

#Site 5 percent water content per survey day
ggplot(filter(soil_moisture, site=="5"), aes(survey, percent_water_content))+geom_bar(stat="identity", fill="blue")+labs(title="Site 5 percent water content per survey day")

#Site 6 percent water content per survey day
ggplot(filter(soil_moisture, site=="6"), aes(survey, percent_water_content))+geom_bar(stat="identity", fill="purple")+labs(title="Site 6 percent water content per survey day")



#On each day, what is the percent water content of each plot?

#Survey 1 percent water content at each plot
ggplot(filter(soil_moisture, survey=="1"), aes(plot_number, percent_water_content, fill=plot_number))+geom_bar(stat="identity")+labs(title="Percent water content of each plot on survey 1")

#Survey 2 percent water content at each plot
ggplot(filter(soil_moisture, survey=="2"), aes(plot_number, percent_water_content, fill=plot_number))+geom_bar(stat="identity")+labs(title="Percent water content of each plot on survey 2")

#Survey 3 percent water content at each plot
ggplot(filter(soil_moisture, survey=="3"), aes(plot_number, percent_water_content, fill=plot_number))+geom_bar(stat="identity")+labs(title="Percent water content of each plot on survey 3")

#Survey 4 percent water content at each plot
ggplot(filter(soil_moisture, survey=="4"), aes(plot_number, percent_water_content, fill=plot_number))+geom_bar(stat="identity")+labs(title="Percent water content of each plot on survey 4")

#Survey 5 percent water content at each plot
ggplot(filter(soil_moisture, survey=="5"), aes(plot_number, percent_water_content, fill=plot_number))+geom_bar(stat="identity")+labs(title="Percent water content of each plot on survey 5")

#Survey 6 percent water content at each plot
ggplot(filter(soil_moisture, survey=="6"), aes(plot_number, percent_water_content, fill=plot_number))+geom_bar(stat="identity")+labs(title="Percent water content of each plot on survey 6")

#Survey 7 percent water content at each plot
ggplot(filter(soil_moisture, survey=="7"), aes(plot_number, percent_water_content, fill=plot_number))+geom_bar(stat="identity")+labs(title="Percent water content of each plot on survey 7")




