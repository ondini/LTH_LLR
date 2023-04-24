library(ggplot2)

# Part 3 of project 1 ####
# 3.A ####

## Load the data ####
weather <- read.csv("project1/data/weather.csv")
summary(weather)

## Fit the cbrt model with interaction ####
(speedimod<- lm(I(rain^(1/3)) ~ pressure + location * speed, data = weather))
(speedimod.sum <- summary(speedimod))

confint(speedimod)
