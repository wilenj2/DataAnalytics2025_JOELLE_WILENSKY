####### Data Analytics Fall 2025 Assignment 2 ######

### Libraries
library(tidyverse)
# library(skimr)
# library(janitor)
library(caret)
library(pROC)
library(randomForest)
library(e1071)
library(GGally)

### set working directory
setwd("~/0Classes/4.0_Senior_Fall/Data_Analytics/Assignment 6")

### read in data
df <- read.csv("sobar-72.csv", header=TRUE)

View(df)


## Part 1

set.seed(4600)

# Confirm target exists
stopifnot("ca_cervix" %in% names(df))

df <- df %>%
  mutate(ca_cervix = factor(ca_cervix, levels = c(0,1), labels = c("No","Yes")))

glimpse(df)

colSums(is.na(df))
df %>% count(ca_cervix) %>% mutate(prop = n/sum(n))

# Basic distribution plots for predictors (numeric)
num_cols <- df %>% select(-ca_cervix) %>% select(where(is.numeric)) %>% names()

ggplot(df_long, aes(x = value)) +
  geom_histogram(bins = 10) +
  facet_wrap(~feature, scales = "free") +
  theme_minimal() +
  labs(title = "Distributions")

# Outlier check via boxplots
ggplot(df_long, aes(x = feature, y = value)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Boxplots")



## Part 2

# Model 1: Regression
m_glm <- train(
  empowerment_knowledge ~ .,
  data = train_pp,
  method = "glm",
  family = binomial(),
  metric = "ROC",
  trControl = ctrl
)

res_glm <- eval_on_test(m_glm, test_pp)
res_glm$confusion
res_glm$auc
plot(res_glm$roc, main = "ROC - Logistic Regression")

# Model 2: Random Forest
m_rf <- train(
  empowerment_knowledge ~ .,
  data = train_pp,
  method = "rf",
  metric = "ROC",
  trControl = ctrl,
  tuneLength = 10
)

res_rf <- eval_on_test(m_rf, test_pp)
res_rf$confusion
res_rf$auc
plot(res_rf$roc, main = "ROC - Random Forest")

# Variable importance (nice for your write-up)
rf_imp <- varImp(m_rf)
plot(rf_imp, top = 15, main = "RF Variable Importance")

# Model 3: SVM (Radial)
m_svm <- train(
  empowerment_knowledge ~ .,
  data = train_pp,
  method = "svmRadial",
  metric = "ROC",
  trControl = ctrl,
  tuneLength = 10
)

res_svm <- eval_on_test(m_svm, test_pp)
res_svm$confusion
res_svm$auc
plot(res_svm$roc, main = "ROC - SVM (Radial)")


# Potential dimension reduction experiment with PCA

pca_pp <- preProcess(train %>% select(-ca_cervix),
                     method = c("center","scale","pca"),
                     thresh = 0.95)

train_pca <- predict(pca_pp, train %>% select(-ca_cervix)) %>%
  bind_cols(ca_cervix = train$ca_cervix)
test_pca  <- predict(pca_pp, test %>% select(-ca_cervix)) %>%
  bind_cols(ca_cervix = test$ca_cervix)

m_glm_pca <- train(
  ca_cervix ~ .,
  data = train_pca,
  method = "glm",
  family = binomial(),
  metric = "ROC",
  trControl = ctrl
)

res_glm_pca <- eval_on_test(m_glm_pca, test_pca)
res_glm_pca$auc
plot(res_glm_pca$roc, main = "ROC - Logistic Regression (PCA features)")
