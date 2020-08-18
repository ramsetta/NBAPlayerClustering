
# Name - Tony Ramsett
# Project Name - NBA Player Clustering
# Date Started - 07/05/2020
# Class - MSDS 696 - Data Practicum 2

library("class") # for KNN()
library("gmodels") # Used for CrossTable() function
library("caret") # dummyVars and confusionMatrix()
library("e1071") # confusionMatrix()

# Import data sets
score_cluster_pred <- read.csv("Scoring_Stats_With_Clusters.csv", header = T)

# Review data set
summary(score_cluster_pred)
str(score_cluster_pred)
head(score_cluster_pred, 5)

# Necessary columns only for data to be used then view to ensure accuracy
score_cluster_pred <- score_cluster_pred[c(3:23)]
str(score_cluster_pred) # Ensure desired columns remain

# Normalize data set
normalize <- function(x) {return((x-min(x))/(max(x)-min(x)))} # normalize function

# Apply normalize function to data set
score_cluster_pred_2 <- as.data.frame(lapply(score_cluster_pred[1:20], normalize))
str(score_cluster_pred_2)

# Add cluster back into data (Cluster variable does not need to be normalized)
score_cluster_pred_2$Cluster <- score_cluster_pred$Cluster_Number
str(score_cluster_pred_2)

# Create training and test data sets, 70/30 split
set.seed(135)
score_cluster_pred_3 <- createDataPartition(score_cluster_pred_2$Cluster, p = 0.7, list = FALSE) #70/30 split
score_cluster_pred_train <- score_cluster_pred_2[score_cluster_pred_3, ]
score_cluster_pred_test <- score_cluster_pred_2[-score_cluster_pred_3, ]

# Ensure train and test data sets are set up properly
dim(score_cluster_pred_train); dim(score_cluster_pred_test)
table(score_cluster_pred_train$Cluster); table(score_cluster_pred_test$Cluster)

# 1 - K-Nearest Neighbor Model

set.seed(135) # Set seed for reproduceable results

# Hypothesis
# H0: There is no significant difference between clusters
# H1: There is a significant difference between clusters

# Create model
knn_pred_score <- knn(train = score_cluster_pred_train, test = score_cluster_pred_test, 
                        cl = score_cluster_pred_train$Cluster, k = 10)

# Summary of model
summary(knn_pred_score)
plot(knn_pred_score)

# Review model results
CrossTable(x = score_cluster_pred_test$Cluster, y = knn_pred_score, dnn = c("Actual Cluster", "Predicted Cluster"), prop.chisq = F)
confusionMatrix(factor(score_cluster_pred_test$Cluster), knn_pred_score, dnn = c("Actual Cluster", "Predicted Cluster"))

# P-value = 6.456e-09, alpha = 0.05 - Reject the null hypothesis

# 2 - Linear Regression mode

set.seed(135) # Set seed for reproduceable results

# Hypothesis
# H0: There is no significant difference between clusters
# H1: There is a significant difference between clusters

# Create model
regression_nba_score <- lm(Cluster ~ ., data = score_cluster_pred_train)

# View model results
summary(regression_nba_score)

# Adjusted R-squared = 0.644
# P-value = 4.738e-09, alpha = 0.05 - Reject the null hypothesis