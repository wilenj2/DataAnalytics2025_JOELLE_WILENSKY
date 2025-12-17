####### Data Analytics Fall 2025 Assignment 2 ######

# Libraries 

library(ggplot2)

library(tidyverse)
library(class)
library(caret)
library(GGally)

### set working directory
setwd("~/0Classes/4.0_Senior_Fall/Data_Analytics/Assignment 2")

### read in data
epi.data <- read.csv("epi_results_2024_pop_gdp_v2.csv", header=TRUE)

View(epi.data)
str(epi.data)
summary(epi.data$ECO.new)
unique(epi.data$region)

# Remove NA
epi.data <- na.omit(epi.data)


# 1. VARIABLE DISTRIBUTIONS

# Create subsets for two regions
region1 <- "Southern Asia"
region2 <- "Greater Middle East"

epi.data_r1 <- epi.data %>% filter(region == region1)
epi.data_r1 <- subset(epi.data, region %in% c(region1, region2))
epi.data_r2 <- epi.data %>% filter(region == region2)
epi.data_r2 <- subset(epi.data, region %in% c(region1, region2))

# Boxplots for ECO.new 
ggplot(epi.data_r1, aes(x = region, y = ECO.new, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("orange", "skyblue")) +
  labs(title = "Boxplots of ECO.new by Region",
       x = "Region", y = "ECO.new") +
  theme_minimal() +
  coord_flip() +
  facet_wrap(~region, scales = "free_x")

epi.data_r1 <- subset(epi.data, region %in% c(region1, region2))

ggplot(epi.data_r1, aes(x = region, y = ECO.new, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("orange", "skyblue")) +
  labs(title = "Boxplots of ECO.new for Two Regions",
       x = "Region", y = "ECO.new") +
  theme_minimal()

ggplot(epi.data_r2, aes(x = region, y = ECO.new, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("orange", "skyblue")) +
  labs(title = "Boxplots of ECO.new for Two Regions",
       x = "Region", y = "ECO.new") +
  theme_minimal()



epi.data_regions <- subset(epi.data, region %in% c(region1, region2))
table(epi.data_regions)

ggplot(epi.data_regions, aes(x = region, y = ECO.new, fill = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("orange", "skyblue")) +
  labs(title = "Boxplots of ECO.new for Two Regions",
       x = "Region", y = "ECO.new") +
  theme_minimal()


# Histograms with density lines
ggplot(epi.data_r1, aes(x = ECO.new)) +
  geom_histogram(aes(y = ..density..), fill = "orange", bins = 20, alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  labs(title = paste("Histogram of ECO.new -", region1)) +
  theme_minimal()

ggplot(epi.data_r2, aes(x = ECO.new)) +
  geom_histogram(aes(y = ..density..), fill = "skyblue", bins = 20, alpha = 0.6) +
  geom_density(color = "blue", size = 1) +
  labs(title = paste("Histogram of ECO.new -", region2)) +
  theme_minimal()

# QQ Plot between regions 
qqplot(epi.data_r1$ECO.new, epi.data_r2$ECO.new,
       main = paste("QQ Plot: ECO.new between", region1, "and", region2),
       xlab = region1, ylab = region2)
abline(0, 1, col = "red", lwd = 2)



# Linear models

# Model 1: ECO.new ~ GDP
model1 <- lm(ECO.new ~ gdp, data = epi.data)
summary(model1)

# Model 2: ECO.new ~ log(GDP) 
model2 <- lm(ECO.new ~ log(gdp), data = epi.data)
summary(model2)

# Plot significant predictor vs response
ggplot(epi.data, aes(x = gdp, y = ECO.new)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "ECO.new vs GDP", x = "GDP", y = "ECO.new") +
  theme_minimal()

# Residual plots
par(mfrow = c(1,2))
plot(model1$residuals, main = "Residuals: ECO.new ~ GDP", col = "darkgreen")
plot(model2$residuals, main = "Residuals: ECO.new ~ log(GDP)", col = "red")
par(mfrow = c(1,1))

# Repeat for one region (Southern Asia) 
model_r1 <- lm(ECO.new ~ log(gdp), data = epi.data_r1)
summary(model_r1)



# 3. CLASSIFICATION (kNN)

# Remove rows with missing values in relevant columns
epi.data_knn <- epi.data %>%
  select(region, EPI.new, ECO.new, LUF.new, GTI.new, GTP.new, GHN.new, CBP.new) %>%
  drop_na()

# Encode region as factor
epi.data_knn$region <- as.factor(epi.data_knn$region)

# Normalize numeric columns
epi.data_norm <- as.data.frame(scale(epi.data_knn[ , -1]))
epi.data_norm$region <- epi.data_knn$region

# Split data
set.seed(123)
train_index <- createDataPartition(epi.data_norm$region, p = 0.7, list = FALSE)
train <- epi.data_norm[train_index, ]
test <- epi.data_norm[-train_index, ]

# Model 1: kNN with 3 predictors
k_value <- 5
predictors1 <- c("EPI.new", "ECO.new", "LUF.new")
knn_pred1 <- knn(train[predictors1], test[predictors1],
                 train$region, k = k_value)

conf1 <- confusionMatrix(knn_pred1, test$region)
conf1
accuracy1 <- conf1$overall["Accuracy"]

# Model 2: kNN with 3 other predictors 
predictors2 <- c("GTI.new", "GTP.new", "GHN.new")
knn_pred2 <- knn(train[predictors2], test[predictors2],
                 train$region, k = k_value)

conf2 <- confusionMatrix(knn_pred2, test$region)
conf2
accuracy2 <- conf2$overall["Accuracy"]

