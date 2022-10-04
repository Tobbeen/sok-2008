#Utfordring 3.1.2

library(OECD)   #The OECD package
library(ggplot2)     # the ggplot package
library(tidyverse)  # the tidyverse package
library(dplyr)  # The DPLYR package
library(ggrepel) # The ggrepel package

#We want to create a graph that shows the correlation between minimum wages and unemployment. We need to search the OECD data frame for data on these topics.
#Search data set for minimum wages and unemployment statistics
dsets <- get_datasets()
search_dataset("wage",dsets)
search_dataset("unemployment",dsets)

#Data on minimum wages is available in "MIN2AVE"
#Data on unemployment is available in "MIG_NUP_RATES_GENDER"

#MinWage
minwage <- get_dataset("MIN2AVE",
                       filter = "USA+CAN+FRA+GBR+DEU+NZL", 
                       pre_formatted = TRUE)
#Selecting years and the min wage as a share of median wage
minwage2019 <- subset(minwage, Time < 2019 & Time >2007 & SERIES=="MEDIAN")
minwage2007_2019 <- subset(minwage2019, Time>2007)

#UnEmpl
unempl <- get_dataset("MIG_NUP_RATES_GENDER",
                      filter = "USA+CAN+FRA+GBR+DEU+NZL", 
                      pre_formatted = TRUE)

#Selecting years, the unemployment rate of people born in the country, and both sexes
unempl2019 <- subset(unempl, Time<2019 & RATE=="U_RATE" & BIRTH=="NB" & GENDER=="TOT")
unempl2007_2019 <- subset(unempl2019, Time>2007)

#Combining datasets - we need to merge by both country and year to get the right number in the right place
minwage_unempl <-left_join(minwage2007_2019, unempl2007_2019, by=c("COUNTRY","Time"))

#removing countries with missing data
complete_minwage_unempl <- na.omit(minwage_unempl)

#transforming the minimum wage and uneployment rate to numeric variables
complete_minwage_unempl$MinWage_0 <-as.numeric(complete_minwage_unempl$ObsValue.x) #MinWage is between 0 and 1, I want to transform it to between 0 and 100 later, so I call it MinWage_0 here
complete_minwage_unempl$UnEmpl <-as.numeric(complete_minwage_unempl$ObsValue.y)

#Transforming Minimum wage to percent
complete_minwage_unempl$MinWage <- complete_minwage_unempl$MinWage_0 * 100


#Code for the graph (you need to insert data and variable names)
minwage_plot <- ggplot(data=complete_minwage_unempl,aes(x=UnEmpl,y=MinWage, group=COUNTRY, color=COUNTRY)) + # Put unemployment in percent on the x-axis and min wage as percent of median wage on y-axis
  geom_line(aes(group=COUNTRY), size=1) +
  geom_point(size=2.5)+
  labs(x = "Arbeidsledigheten i prosent" , y ="Minstelønn vist som prosent av medianlønn")  + #Insert names for x and y-axis.
  theme(legend.position="none")+
  geom_label_repel(
    data=complete_minwage_unempl %>% group_by(COUNTRY) %>% #Insert name of data
      filter(UnEmpl==min(UnEmpl)), # Insert the name of the x-variable. This will put the country name at the start of each country line.
    aes(UnEmpl, MinWage, fill = factor(COUNTRY), label = sprintf('%s', COUNTRY)), #Insert name for x and y variable
    color = "black", # the color of the line around the country tag
    fill = "white") #The color of the fill of the country tag
minwage_plot 

