#Load packages
library(dplyr)
library(tidyverse)
library(ggpubr)

#Load data, areas 11 & 13
DC_catch.11.13 <- read.csv("Data/2007.2017_DC_catch.csv", header = TRUE)

#Load data, area 10
DC_catch.10 <- read.csv("Data/2007.2017_DC_catch.10.csv", header = TRUE)

#Stitch tables
DC_catch <- inner_join(DC_catch.11.13, DC_catch.10, by = "year")
head(DC_catch)

#Rename columns
DC_catch_renamed <- DC_catch %>% rename("10" = lbs_dungeness.10, 
                                     "11" = lbs_dungeness.11, "13" = lbs_dungeness.13)
head(DC_catch_renamed)

#Gather data
DC_catch_tidy <- DC_catch_renamed %>% 
  gather(`10`, `11`, `13`, key = "area", value = "lbs")
head(DC_catch_tidy)

#Main data plot (Areas 11 and 13)
ggplot(DC_catch_tidy) +
  geom_point(mapping = aes(x = year, y = lbs, col = area)) +
  geom_smooth(mapping = aes(x = year, y = lbs, col = area)) +
  labs(title = "Pounds of Dungeness Caught per Year", x = "Year", 
       y = "Pounds of Dungeness", col = "Area")

#Data subset for linear model
#Area 10
lin_data.10 <- subset(DC_catch, year %in% c("2013", "2014", "2015", "2016", "2017")
#Area 11
lin_data.11 <- subset(DC_catch, year %in% c("2014", "2015", "2016", "2017"))
#Area 13
lin_data.13 <- subset(DC_catch, year %in% c("2011", "2012", "2013", "2014", 
                                            "2015", "2016", "2017"))

#Linear model using subset
#Area 10
lm_fit.10 <- lm(year ~ lbs_dungeness.10, data=lin_data.10)
summary(lm_fit.10)
#Area 11
lm_fit.11 <- lm(year ~ lbs_dungeness.11, data=lin_data.11)
summary(lm_fit.11)
#Area 13
lm_fit.13 <- lm(year ~ lbs_dungeness.13, data=lin_data.13)
summary(lm_fit.13)

#Plotting data subsets with corresponding regression line from linear model
#Area 11
#Regression line dataframe, area 11
predicted.11 <- data.frame(catch_pred = predict(lm_fit.11, lin_data.11), 
                           lbs_dungeness.11=lin_data.11$lbs_dungeness.11)
#Plot, area 11
ggplot(lin_data.11) +
  geom_point(mapping = aes(x = year, y = lbs_dungeness.11), 
             col = "blue", shape = "triangle", size = 3) +
  geom_line(data = predicted.11, aes(x=catch_pred, y=lbs_dungeness.11)) +
  labs(title = "Pounds of Dungeness Caught per Year", x = "Year", 
       y = "Pounds of Dungeness") +
  stat_regline_equation(data = predicted.11, mapping = aes(x = catch_pred, 
                                                           y = lbs_dungeness.11), 
                        mapping = label = paste(0.8744)), label.x.npc = "center", label.y.npc = "top", 
                        output.type = "expression", geom = "text",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE)
#Area 13
#Regression line dataframe, area 13
predicted.13 <- data.frame(catch_pred = predict(lm_fit.13, lin_data.13), 
                           lbs_dungeness.13=lin_data.13$lbs_dungeness.13)
#Plot, area 13
ggplot(lin_data.13) +
  geom_point(mapping = aes( x = year, y = lbs_dungeness.13), 
             col = "red", size = 3) +
  geom_line(data = predicted.13, aes(x=catch_pred, y=lbs_dungeness.13)) +
  labs(title = "Pounds of Dungeness Caught per Year", x = "Year", 
       y = "Pounds of Dungeness")
