
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
nbaShooting <- read.csv("Offense Stats_2019-2020.csv", header = T)

# Review data set --------------------------------------------------------------------

# Review shooting stats data set
summary(nbaShooting)
str(nbaShooting)
head(nbaShooting, 5)

# Data Cleaning, Data Preparation, and Correlation plot ---------------------------
# Source - (Nibras, 2018) 

# Necessary columns only for data to be used then view to ensure accuracy
nbaShooting2 <- nbaShooting[c(2,6:27)]
str(nbaShooting2) # Ensure desired columns remain

# Feature Engineering - Add in minutes per game
nbaShooting2$MPG <- nbaShooting2$MP/nbaShooting2$G
str(nbaShooting2)

# Convert NA's to zeroes - NA's occurred where player attempted 0 of 0 3 pointers for example
sum(is.na(nbaShooting2)) # Total NA's in data set
nbaShooting2[is.na(nbaShooting2)] <- 0 
summary(nbaShooting2)
sum(is.na(nbaShooting2)) # Ensure zero NA's remain

# Subset data to cluster significant players then remove games played, minutes player and minutes per game columns
nbaShooting3 <- subset(nbaShooting2, nbaShooting2$MPG >= 29.35)
str(nbaShooting3)

nbaShooting3$G <- NULL
nbaShooting3$MP <- NULL
nbaShooting3$MPG <- NULL
dim(nbaShooting3)

# Duplicate data set pre-normalization
nbaShooting4 <- nbaShooting3

# Normalize data set
normalize <- function(x) {return((x-min(x))/(max(x)-min(x)))} # normalize function

nbaShooting3 <- as.data.frame(lapply(nbaShooting3[2:21], normalize))
str(nbaShooting3)

# Create correlation plot
nba_corr <- nbaShooting3[1:20]
nba_corrplot <- cor(nba_corr)
corrplot(nba_corrplot, method = "circle", type = "upper")

# K-Means Clustering -------------------------------------------------------------

# Elbow method
set.seed(135)
fviz_nbclust(nbaShooting3, kmeans, method = "wss", nstart = 25) +
  geom_vline(xintercept = 3, linetype = 2)

# Silhouette method
set.seed(135)
fviz_nbclust(nbaShooting3, kmeans, method = "silhouette", nstart = 25)

# Gap Statistic method
set.seed(135)
fviz_nbclust(nbaShooting3, kmeans, method = "gap_stat", nstart = 25, nboot = 50)

# Visualize K-means clusters
# Source - (STDHA, 2017)

# Separate date into 3 clusters and view results of cluster means per variable
set.seed(135)
km_res_scoring <- kmeans(nbaShooting3, 3, nstart = 25)
print(km_res_scoring)

# View row totals per cluster
km_res_scoring$size

# Input player names as row names for cluster plot visualization
rownames(nbaShooting3) <- nbaShooting4$Player

# Display cluster plot output
set.seed(135)
fviz_cluster(km_res_scoring, nbaShooting3, ellipse = T, ellipse.alpha = 0.2,
             palette = "uchicago", repel = TRUE, ggtheme = theme_minimal(),
             main = "Offensive Scoring Cluster Plot")

# Export to Excel for stats/ cluster groupings - then used for stats per cluster

# View directory of where csv will save
directory <-getwd()
directory

# Create data with cluster number and write csv
nbaShooting5 <- cbind(nbaShooting4, Cluster_Number = km_res_scoring$cluster)
write.csv(nbaShooting5, "Offensive_Scoring_Stats_With_Clusters.csv")

# Hierarchical Clustering Analysis (HCA) / Dendrograms ------------------------------
# Source - Techvidvan Team, 2020), (UC Business Analytics R Programming Guide, n.d.)
        
# Use euclidean distance for dissimiliarity values and Wards method for clustering
set.seed(135)
distance <- dist(nbaShooting3, method = "euclidean")
hca_clust <- hclust(distance, method = "ward.D2" )

# Cut tree/data into 3 groups
sub_grp <- cutree(hca_clust, k = 3)

# Number of members in each cluster
table(sub_grp)

# Display dendrogram output
set.seed(135)
fviz_dend(hca_clust, k = 3, cex = 0.7, horiz = TRUE, rect = TRUE, rect_fill = TRUE, 
          rect_border = "uchicago", palette = "uchicago", repel = TRUE,
          main = "Offensive Scoring Dendrogram")

# Principal Component Analysis (PCA) ---------------------------------------------
# Source - (STDHA, 2017), (STDHA, n.d.)

# Compute PCA
nba_pca_scoring <- PCA(nbaShooting3, graph = F)

# Create screeplot to determine number of principal components
fviz_eig(nba_pca_scoring, addlabels = T, ylim = c(0,50))

# Create factor map, coloring by cos2 values - quality of representation of variables
fviz_pca_var(nba_pca_scoring, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE )

# Create bar plots for dimension 1 and 2 variables
fviz_contrib(nba_pca_scoring, choice = "var", axes = 1, top = 10)
fviz_contrib(nba_pca_scoring, choice = "var", axes = 2, top = 10)

# Create plot to group players by cluster number
str(nbaShooting5)
nbaShooting6 <- nbaShooting5[c(2:21)] # Remove player and cluster number variables
rownames(nbaShooting6) <- nbaShooting4$Player
str(nbaShooting6)

set.seed(135)
nba_pca_scoring_2 <- PCA(nbaShooting6, graph = F) # Compute PCA

nbaShooting5$Cluster_Number <- as.factor(nbaShooting5$Cluster_Number) # Cluster number as factor

fviz_pca_ind(nba_pca_scoring_2, repel = T, palette = "uchicago", addEllipses = TRUE,
             ellipse.alpha = 0.2, col.ind = nbaShooting5$Cluster_Number)

# Create biplot showing players and variables
set.seed(135)
fviz_pca_biplot(nba_pca_scoring_2, col.ind = nbaShooting5$Cluster_Number, addEllipses = TRUE,
                col.var = "black", repel = T, legend.title = "Cluster", palette = "uchicago")

# Variable contribution by dimension
# Source - (STDHA, 2017)

print(nba_pca_scoring)
var <- get_pca_var(nba_pca_scoring)
head(var$contrib)
head(var$coord, 80)

# Correlations for dimensions
res.desc <- dimdesc(nba_pca_scoring, axes = c(1,2), proba = 0.05)
res.desc$Dim.1
res.desc$Dim.2

