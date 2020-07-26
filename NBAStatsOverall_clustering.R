
# Name - Tony Ramsett
# Project Name - NBA Player Clustering
# Date Started - 07/05/2020
# Class - MSDS 696 - Data Practicum 2
# Data Reference - Two data sets from BasketballReference.com (2019-2020 Per 36 minutes and 2019-2020 Advanced Stats)

library("corrplot")
library("factoextra") # Used for clustering (K-means)
library("NbClust") # Used for determining optimal number of clusters
library("FactoMineR") # Used for HCA and dendrogram
library("cluster") # Used for clustering and dendrograms

# Import data sets
nbaTradional <- read.csv("NBA Traditional Stats 2019-2020_2.csv", header = T)
nbaAdvanced <- read.csv("NBA Advanced Stats 2019-2020_2.csv", header = T)

# Review each data set and combine to one data set --------------------------------

# Review stats in data set
summary(nbaTradional)
str(nbaTradional)
head(nbaTradional, 5)

summary(nbaAdvanced)
str(nbaAdvanced)
head(nbaAdvanced, 5)

# Data Cleaning, Data Preparation, and Correlation plot ---------------------------
# References - https://towardsdatascience.com/which-nba-players-are-most-similar-machine-learning-provides-the-answers-r-project-b903f9b2fe1f

# Combine two data sets
nbabothStats <- cbind(nbaTradional, nbaAdvanced)
str(nbabothStats)

# Necessary columns only for data to be used then view to ensure accuracy
nbaStats <- nbabothStats[c(2,8:26,30,39:51,53)]
str(nbaStats) # Ensure desired columns remain

# Convert NA's to zeroes - NA's occurred where player attempted 0 of 0 3 pointers for example
sum(is.na(nbaStats)) # Total NA's in data set
# No NA's exist

# Subset data to cluster significant players then remove Minutes per game column
nbaStats2 <- subset(nbaStats, nbaStats$MIN >= 29.35)
str(nbaStats2)

nbaStats2$MIN <- NULL
dim(nbaStats2)

# Duplicate data set pre-normalization
nbaStats3 <- nbaStats2
nbaStats4 <- nbaStats3

# Normalize data set
normalize <- function(x) {return((x-min(x))/(max(x)-min(x)))} # normalize function

nbaStats3 <- as.data.frame(lapply(nbaStats2[2:34], normalize))
str(nbaStats3)

# Create correlation plot
nba_corr <- nbaStats3[1:33]
nba_corrplot <- cor(nba_corr)
corrplot(nba_corrplot, method = "circle", type = "upper")

# K-Means Clustering -------------------------------------------------------------

# Determine number of clusters for analysis
# source - Machine Learning clustering homework

# Elbow method
set.seed(135)
fviz_nbclust(nbaStats3, kmeans, method = "wss", nstart = 25)

# Silhouette method
set.seed(135)
fviz_nbclust(nbaStats3, kmeans, method = "silhouette", nstart = 25)

# Gap Statistic method
set.seed(135)
fviz_nbclust(nbaStats3, kmeans, method = "gap_stat", nstart = 25,  nboot = 50)

# Visualize Kmeans clusters
# source - http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/117-hcpc-hierarchical-clustering-on-principal-components-essentials/

# Separate date into 3 clusters and view results of cluster means per variable
set.seed(135)
km_stats_res <- kmeans(nbaStats3, 3, nstart = 25)
print(km_stats_res)

# View row totals per cluster
km_stats_res$size

# Input player names as row names for cluster plot visualization
rownames(nbaStats3) <- nbaStats4$PLAYER

# Display cluster plot output
set.seed(135)
fviz_cluster(km_stats_res, nbaStats3, ellipse.alpha = 0.2,
             palette = "jco",repel = TRUE, ggtheme = theme_minimal(),
             main = "Overall Stats Cluster Plot")

# Export to Excel for stats/ cluster groupings - then used for stats per cluster
# source - https://www.guru99.com/r-exporting-data.html

# View directory of where csv will save
directory <-getwd()
directory

# Create data with cluster number and write csv
nbaStats5 <- cbind(nbaStats4, Cluster_Number = km_stats_res$cluster)
write.csv(nbaStats5, "Overall_Stats_With_Clusters.csv")

# Hierarchical Clustering Analysis (HCA) / Dendrograms ------------------------------
# source - https://techvidvan.com/tutorials/cluster-analysis-in-r/
# source - https://uc-r.github.io/hc_clustering

# Use euclidean distance for dissimiliarity values and Wards method for clustering
set.seed(135)
distance <- dist(nbaStats3, method = "euclidean")
hca_clust <- hclust(distance, method = "ward.D2" )

# Cut tree/data into 3 groups
sub_grp <- cutree(hca_clust, k = 3)

# Number of members in each cluster
table(sub_grp)

# Display dendrogram output
set.seed(135)
fviz_dend(hca_clust, k = 3, cex = 0.7, horiz = TRUE, rect = TRUE, rect_fill = TRUE, 
          rect_border = "jco", palette = "jco", repel = TRUE,
          main = "Overall Stats Dendrogram")

# Principal Component Analysis (PCA) ---------------------------------------------
# source - http://www.sthda.com/english/wiki/factoextra-r-package-easy-multivariate-data-analyses-and-elegant-visualization
# source http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

# Compute PCA
nba_pca_overall <- PCA(nbaStats3, graph = F)

# Create screeplot to determine number of principal components
fviz_eig(nba_pca_overall, addlabels = T, ylim = c(0,50))

# Create factor map, coloring by cos2 values - quality of representation of variables
fviz_pca_var(nba_pca_overall, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# Create bar plots for dimension 1 and 2 variables
fviz_contrib(nba_pca_overall, choice = "var", axes = 1, top = 10)
fviz_contrib(nba_pca_overall, choice = "var", axes = 2, top = 10)

# Create plot to group players by cluster number
str(nbaStats5)
nbaStats6 <- nbaStats5[c(2:34)] # Remove player and cluster number variables
rownames(nbaStats6) <- nbaStats4$PLAYER
str(nbaStats5)

set.seed(135)
nba_pca_overall_2 <- PCA(nbaStats6, graph = F) # Compute PCA

nbaStats5$Cluster_Number <- as.factor(nbaStats5$Cluster_Number) # Cluster number as factor

fviz_pca_ind(nba_pca_overall_2, repel = T, palette = "jco", addEllipses = TRUE,
             ellipse.alpha = 0.2, col.ind = nbaStats5$Cluster_Number)

# Create biplot showing players and variables
set.seed(135)
fviz_pca_biplot(nba_pca_overall_2, col.ind = nbaStats5$Cluster_Number, addEllipses = TRUE,
                col.var = "black", repel = T, legend.title = "Cluster", palette = "jco")