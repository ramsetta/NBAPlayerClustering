
# Name - Tony Ramsett
# Project Name - NBA Player Clustering
# Date Started - 07/05/2020
# Class - MSDS 696 - Data Practicum 2

library("corrplot") # Used for correlation plots
library("factoextra") # Used for clustering (K-means)
library("NbClust") # Used for determining optimal number of clusters
library("FactoMineR") # Used for HCA and dendrograms
library("cluster") # Used for clustering and dendrograms

# Import data sets
nbaDefense <- read.csv("Defense stats_2019-2020.csv", header = T)

# Review data set --------------------------------------------------------------------

# Review defense stats data set
summary(nbaDefense)
str(nbaDefense)
head(nbaDefense, 5)

# Data Cleaning, Data Preparation, and Correlation plot ---------------------------
# Source - (Nibras, 2018) 

# Necessary columns only for data to be used then view to ensure accuracy
nbaDefense2 <- nbaDefense[c(2,8:21)]
str(nbaDefense2) # Ensure desired columns remain

# Convert NA's to zeroes - NA's occurred where player attempted 0 of 0 3 pointers for example
sum(is.na(nbaDefense)) # Total NA's in data set
# 0 NA's - No changes required

# Subset data to cluster significant players then remove Minutes per game column
nbaDefense3 <- subset(nbaDefense2, nbaDefense2$MIN >= 29.35)
str(nbaDefense3)

nbaDefense3$MIN <- NULL
dim(nbaDefense3)

# Duplicate data set pre-normalization
nbaDefense4 <- nbaDefense3

# Normalize data set
normalize <- function(x) {return((x-min(x))/(max(x)-min(x)))} # normalize function

nbaDefense3 <- as.data.frame(lapply(nbaDefense3[2:14], normalize))
str(nbaDefense3)

# Create correlation plot
nba_corr <- nbaDefense3[1:13]
nba_corrplot <- cor(nba_corr)
corrplot(nba_corrplot, method = "circle", type = "upper")

# K-Means Clustering -------------------------------------------------------------

# Elbow method
set.seed(135)
fviz_nbclust(nbaDefense3, kmeans, method = "wss", nstart = 25) +
  geom_vline(xintercept = 3, linetype = 2)

# Silhouette method
set.seed(135)
fviz_nbclust(nbaDefense3, kmeans, method = "silhouette", nstart = 25)

# Gap Statistic method
set.seed(135)
fviz_nbclust(nbaDefense3, kmeans, method = "gap_stat", nstart = 25, nboot = 50)

# Visualize K-means clusters
# Source - (STDHA, 2017)

# Separate date into 3 clusters and view results of cluster means per variable
set.seed(135)
km_res_defense <- kmeans(nbaDefense3, 3, nstart = 25)
print(km_res_defense)

# View row totals per cluster
km_res_defense$size

# Input player names as row names for cluster plot visualization
rownames(nbaDefense3) <- nbaDefense4$Player

# Display cluster plot output
set.seed(135)
fviz_cluster(km_res_defense, nbaDefense3,  ellipse.alpha = 0.2,
             palette = "Dark2", repel = TRUE, ggtheme = theme_minimal(),
             main = "Defensive Performance Cluster Plot")

# Export to Excel for stats/ cluster groupings - then used for stats per cluster

# View directory of where csv will save
directory <-getwd()
directory

# Create data with cluster number and write csv
nbaDefense5 <- cbind(nbaDefense4, Cluster_Number = km_res_defense$cluster)
write.csv(nbaDefense5, "Defense_Stats_With_Clusters.csv")

# Hierarchical Clustering Analysis (HCA) / Dendrograms ------------------------------
# Source - Techvidvan Team, 2020), (UC Business Analytics R Programming Guide, n.d.)

# Use euclidean distance for dissimiliarity values and Wards method for clustering
set.seed(135)
distance <- dist(nbaDefense3, method = "euclidean")
hca_clust <- hclust(distance, method = "ward.D2" )

# Cut tree/data into 3 groups
sub_grp <- cutree(hca_clust, k = 3)

# Number of members in each cluster
table(sub_grp)

# Display dendrogram output
set.seed(135)
fviz_dend(hca_clust, k = 3, cex = 0.7, horiz = TRUE, rect = TRUE, rect_fill = TRUE, 
          rect_border = "Dark2", palette = "Dark2", repel = TRUE,
          main = "Defensive Performance Dendrogram")

# Principal Component Analysis (PCA) ---------------------------------------------
# Source - (STDHA, 2017),  (STDHA, n.d.)

# Compute PCA
nba_pca_defense <- PCA(nbaDefense3, graph = F)

# Create screeplot to determine number of principal components
fviz_eig(nba_pca_defense, addlabels = T, ylim = c(0,50))

# Create factor map, coloring by cos2 values - quality of representation of variables
fviz_pca_var(nba_pca_defense, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE )

# Create bar plots for dimension 1 and 2 variables
fviz_contrib(nba_pca_defense, choice = "var", axes = 1, top = 10)
fviz_contrib(nba_pca_defense, choice = "var", axes = 2, top = 10)

# Create plot to group players by cluster number
str(nbaDefense5)
nbaDefense6 <- nbaDefense5[c(2:14)] # Remove player and cluster number variables
rownames(nbaDefense6) <- nbaDefense4$Player
str(nbaDefense6)

set.seed(135)
nba_pca_defense_2 <- PCA(nbaDefense6, graph = F) # Compute PCA

nbaDefense5$Cluster_Number <- as.factor(nbaDefense5$Cluster_Number) # Cluster number as factor

fviz_pca_ind(nba_pca_defense_2, repel = T, palette = "Dark2", addEllipses = TRUE,
             ellipse.alpha = 0.2, col.ind = nbaDefense5$Cluster_Number)

# Create biplot showing players and variables
set.seed(135)
fviz_pca_biplot(nba_pca_defense_2, col.ind = nbaDefense5$Cluster_Number, addEllipses = TRUE,
                col.var = "black", repel = T, legend.title = "Cluster", palette = "Dark2")
