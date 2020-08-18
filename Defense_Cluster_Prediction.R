
# Name - Tony Ramsett
# Project Name - NBA Player Clustering
# Date Started - 07/05/2020
# Class - MSDS 696 - Data Practicum 2

library("class") # for KNN()
library("gmodels") # Used for CrossTable() function
library("caret") # dummyVars and confusionMatrix()
library("e1071") # confusionMatrix()

# Import data sets
defense_cluster_pred <- read.csv("Defense_Stats_With_Clusters.csv", header = T)

# Review data set
summary(defense_cluster_pred)
str(defense_cluster_pred)
head(defense_cluster_pred, 5)

# Necessary columns only for data to be used then view to ensure accuracy
defense_cluster_pred <- defense_cluster_pred[c(3:16)]
str(defense_cluster_pred) # Ensure desired columns remain

# Normalize data set
normalize <- function(x) {return((x-min(x))/(max(x)-min(x)))} # normalize function

# Apply normalize function to data set
defense_cluster_pred_2 <- as.data.frame(lapply(defense_cluster_pred[1:13], normalize))
str(defense_cluster_pred_2)

# Add cluster back into data (Cluster variable does not need to be normalized)
defense_cluster_pred_2$Cluster <- defense_cluster_pred$Cluster_Number
str(defense_cluster_pred_2)

# Create training and test data sets, 70/30 split
set.seed(135)
defense_cluster_pred_3 <- createDataPartition(defense_cluster_pred_2$Cluster, p = 0.7, list = FALSE) #70/30 split
defense_cluster_pred_train <- defense_cluster_pred_2[defense_cluster_pred_3, ]
defense_cluster_pred_test <- defense_cluster_pred_2[-defense_cluster_pred_3, ]

# Ensure train and test data sets are set up properly
dim(defense_cluster_pred_train); dim(defense_cluster_pred_test)
table(defense_cluster_pred_train$Cluster); table(defense_cluster_pred_test$Cluster)

# 1 - K-Nearest Neighbor Model

set.seed(135) # Set seed for reproduceable results

# Hypothesis
# H0: There is no significant difference between clusters
# H1: There is a significant difference between clusters

# Create model
knn_pred_defense <- knn(train = defense_cluster_pred_train, test = defense_cluster_pred_test, 
                cl = defense_cluster_pred_train$Cluster, k = 10)

# Summary of model
summary(knn_pred_defense)
plot(knn_pred_defense)

# Review model results
CrossTable(x = defense_cluster_pred_test$Cluster, y = knn_pred_defense, dnn = c("Actual Cluster", "Predicted Cluster"), prop.chisq = F)
confusionMatrix(factor(defense_cluster_pred_test$Cluster), knn_pred_defense, dnn = c("Actual Cluster", "Predicted Cluster"))

# P-value = 1.175e-10, alpha = 0.05 - Reject the null hypothesis

# 2 - Linear Regression mode

set.seed(135) # Set seed for reproduceable results

# Hypothesis
# H0: There is no significant difference between clusters
# H1: There is a significant difference between clusters

# Create model
regression_nba_defense <- lm(Cluster ~ ., data = defense_cluster_pred_train)

# View model results
summary(regression_nba_defense)

# Adjusted R-squared = 0.7455
# P-value = 1.126e-14, alpha = 0.05 - Reject the null hypothesis