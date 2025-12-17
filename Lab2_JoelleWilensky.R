####### Data Analytics Fall 2025 Lab 2 ######

library(ggplot2)

### set working directory
setwd("~/0Classes/4.0_Senior_Fall/Data_Analytics/Lab 2")

### read in data
nyhouse.data <- read.csv("NY-House-Dataset.csv", header=TRUE)

View(nyhouse.data)
str(nyhouse.data)
summary(nyhouse.data)

# Remove NA
nyhouse.data <- na.omit(nyhouse.data)


