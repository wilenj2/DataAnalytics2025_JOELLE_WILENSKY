library(readr)
library(EnvStats)

# set working directory (relative path)
setwd("~/0Classes/4.0_Senior_Fall/Data_Analytics/Lab 1")

# read data
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

# view dataframe
View(epi.data)

# print summary of variables
summary(epi.data$H2O.new)
summary(epi.data$LED.new)

# boxplot of variable(s)
boxplot(H2O.new, LED.new, names = c("H2O", "LED"))

# histograms
hist(H2O.new, prob=TRUE, main="Histogram of H2O")
hist(LED.new, prob=TRUE, main="Histogram of LED")

# ecdf plots
plot(ecdf(H2O.new), main = "ECDF of H2O")
plot(ecdf(LED.new), main = "ECDF of LED")

# QQ against normal distribution
qqnorm(H2O.new, main = "H2O QQ Norm Plot"); qqline(H2O.new)
qqnorm(LED.new, main = "LED QQ Norm Plot"); qqline(LED.new)

# QQ against each other
qqplot(H2O.new, LED.new, main = "H2O vs LED QQ Plot", xlab = "H2O", ylab = "LED") 

# Normality statistical test
shapiro.test(H2O.new)
shapiro.test(LED.new)

# Statistial test for having identical distributions
ks.test(H2O.new,LED.new)
