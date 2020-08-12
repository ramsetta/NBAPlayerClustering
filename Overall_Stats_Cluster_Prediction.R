
# Name - Tony Ramsett
# Project Name - NBA Player Clustering
# Date Started - 07/05/2020
# Class - MSDS 696 - Data Practicum 2

library("class") # for KNN()
library("gmodels") # Used for CrossTable() function
library("caret") # dummyVars and confusionMatrix()
library("e1071") # confusionMatrix()

# Import data sets
nba_cluster_pred <- read.csv("Overall_Stats_With_Clusters.csv", header = T)

# Review data set
summary(nba_cluster_pred)
str(nba_cluster_pred)
head(nba_cluster_pred, 5)

# Necessary columns only for data to be used then view to ensure accuracy
nba_cluster_pred <- nba_cluster_pred[c(3:36)]
str(nba_cluster_pred) # Ensure desired columns remain

# Normalize data set
normalize <- function(x) {return((x-min(x))/(max(x)-min(x)))} # normalize function

# Apply normalize function to data set
nba_cluster_pred_2 <- as.data.frame(lapply(nba_cluster_pred[1:33], normalize))
str(nba_cluster_pred_2)

# Add cluster back into data (Cluster variable does not need to be normalized)
nba_cluster_pred_2$Cluster <- nba_cluster_pred$Cluster_Number
str(nba_cluster_pred_2)

# Create training and test data sets, 70/30 split
set.seed(135)
nba_cluster_pred_3 <- createDataPartition(nba_cluster_pred_2$Cluster, p = 0.7, list = FALSE) #70/30 split #70/30 split
nba_cluster_pred_train <- nba_cluster_pred_2[nba_cluster_pred_3, ]
nba_cluster_pred_test <- nba_cluster_pred_2[-nba_cluster_pred_3, ]

# Ensure train and test data sets are set up properly
dim(nba_cluster_pred_train); dim(nba_cluster_pred_test)
table(nba_cluster_pred_train$Cluster); table(nba_cluster_pred_test$Cluster)

# 1 - K-Nearest Neighbor Model
# source - https://rstudio-pubs-static.s3.amazonaws.com/316172_a857ca788d1441f8be1bcd1e31f0e875.html
# source - https://www.edureka.co/blog/knn-algorithm-in-r/

set.seed(135) # Set seed for reproduceable results

# Hypothesis
# H0: There is no significant difference between clusters
# H1: There is a significant difference between clusters

# Create model
knn_pred <- knn(train = nba_cluster_pred_train, test = nba_cluster_pred_test, 
                cl = nba_cluster_pred_train$Cluster, k = 10)

# Summary of model
summary(knn_pred)
plot(knn_pred)

# Review model results
CrossTable(x = nba_cluster_pred_test$Cluster, y = knn_pred, dnn = c("Actual Cluster", "Predicted Cluster"), prop.chisq = F)
confusionMatrix(factor(nba_cluster_pred_test$Cluster), knn_pred, dnn = c("Actual Cluster", "Predicted Cluster"))

# P-value = 3.242e-10, alpha = 0.05 - Reject the null hypothesis

# 2 - Linear Regression mode
# source - http://r-statistics.co/Linear-Regression.html

set.seed(135) # Set seed for reproduceable results

# Hypothesis
# H0: There is no significant difference between clusters
# H1: There is a significant difference between clusters

# Create model
regression_nba <- lm(Cluster ~ ., data = nba_cluster_pred_train)

# View model results
summary(regression_nba)

# Adjusted R-squared = 0.7281
# P-value = 8.842e-08, alpha = 0.05 - Reject the null hypothesis

# Plots
plot(regression_nba)
