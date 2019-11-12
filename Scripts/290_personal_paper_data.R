DC_catch <- read.csv("data/2007.2017_DC_catch.csv", header = TRUE)
library(tidyverse)

ggplot(DC_catch) +
  geom_point(mapping = aes(x = year, y = lbs_dungeness.11), col = "blue", 
             shape  = "triangle", legend.title = "Marine Area 11") +
  geom_smooth(mapping = aes(x = year, y = lbs_dungeness.11), col = "blue") +
  geom_point(mapping = aes(x = year, y = lbs_dungeness.13, col = "red", 
                           legend.title = "Marine Area 11")) +
  geom_smooth(mapping = aes(x = year, y = lbs_dungeness.13), col = "red") +
  ylim(0,125000) +
  labs(title = "Pounds of Dungeness Caught per Year", x = "Year", 
       y = "Pounds of Dungeness") +
  theme(legend.title = "Marine Area")

lin_data.11 <- subset(DC_catch, year %in% c("2014", "2015", "2016", "2017"))
lin_data.13 <- subset(DC_catch, year %in% c("2011", "2012", "2013", "2014", 
                                            "2015", "2016", "2017"))

lm_fit.11 <- lm(year ~ lbs_dungeness.11, data=lin_data.11)
summary(lm_fit.11)

lm_fit.13 <- lm(year ~ lbs_dungeness.13, data=lin_data.13)
summary(lm_fit.13)

predicted.11 <- data.frame(catch_pred = predict(lm_fit.11, lin_data.11), 
                           lbs_dungeness.11=lin_data.11$lbs_dungeness.11)

ggplot(lin_data.11) +
  geom_point(mapping = aes( x = year, y = lbs_dungeness.11), 
             col = "blue", shape = "triangle") +
  geom_line(data = predicted.11, aes(x=catch_pred, y=lbs_dungeness.11))

predicted.13 <- data.frame(catch_pred = predict(lm_fit.13, lin_data.13), 
                           lbs_dungeness.13=lin_data.13$lbs_dungeness.13)

ggplot(lin_data.13) +
  geom_point(mapping = aes( x = year, y = lbs_dungeness.13), 
             col = "red") +
  geom_line(data = predicted.13, aes(x=catch_pred, y=lbs_dungeness.13))
