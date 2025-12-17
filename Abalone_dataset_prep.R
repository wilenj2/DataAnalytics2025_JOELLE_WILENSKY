####################################
##### Abalone Data Preparation #####
####################################

# read dataset
abalone.data <- read.csv("~/0Classes/4.0_Senior_Fall/Data_Analytics/Lab 3/abalone_dataset.csv")

dataset <- abalone.data


## add new abalone.data## add new column age.group with 3 values based on the number of rings 
abalone.data$age.group <- cut(abalone.data$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

## alternative way of setting age.group
abalone.data$age.group[abalone.data$rings<=8] <- "young"
abalone.data$age.group[abalone.data$rings>8 & abalone.data$rings<=11] <- "adult"
abalone.data$age.group[abalone.data$rings>11 & abalone.data$rings<=35] <- "old"


