
# Load required packages
library(readr)

# Load the wine dataset
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
wine_data <- read_csv(url, col_names = FALSE)

# Assign column names
colnames(wine_data) <- c("Class", "Alcohol", "Malic_Acid", "Ash", "Alcalinity_of_Ash", "Magnesium", "Total_Phenols", "Flavanoids", "Nonflavanoid_Phenols", "Proanthocyanins", "Color_Intensity", "Hue", "OD280_OD315", "Proline")

# Perform PCA with scaling (scaling is important to give equal importance to each feature)
pca <- prcomp(wine_data[,-1], scale = TRUE)

# Create a biplot of the PCA results
biplot(pca)

# Identify the feature opposite to 'Hue'
# From the biplot, it appears that 'Malic_Acid' is pointed in the opposite direction of 'Hue'

# Calculate the correlation between 'Malic_Acid' and 'Hue'
correlation <- cor(wine_data$Malic_Acid, wine_data$Hue)
cat("Correlation between Malic_Acid and Hue:", correlation, "\n")

# Create a scree plot
plot(pca, type = "lines")

# Calculate the variance explained by PC1 and PC2
variance_explained <- pca$sdev^2 / sum(pca$sdev^2) * 100
cat("Percentage of total variance explained by PC1:", variance_explained[1], "%\n")
cat("Percentage of total variance explained by PC2:", variance_explained[2], "%\n")


# PROBLEM 2

# Load the USArrests dataset
data(USArrests)
# Convert the dataset to a dataframe with row.names as state names
df <- data.frame(USArrests, row.names = rownames(USArrests))

# Decide whether or not to center/scale the observations
# Since the variables in the dataset have different units, we should scale them to make them comparable
scaled_df <- scale(df)

# Perform k-means clustering with increasing values of k from 2 to 10
set.seed(1)
wss <- c()
for(k in 2:10){
  fit <- kmeans(scaled_df, k)
  wss[k] <- sum(fit$withinss)
}

# Plot the within-cluster sum of squares for each value of k
plot(2:10, wss[2:10], type = "b", xlab = "Number of Clusters (k)", ylab = "Within-cluster sum of squares")

# Determine the optimal number of clusters using the elbow method
# From the plot, it appears that the elbow occurs at k = 5
# Therefore, we will choose k = 5 for our final clustering
k <- 5
fit <- kmeans(scaled_df, k)

# Plot the optimal clustering using the fviz_cluster() function from factoextra

library(tidyverse)
library(factoextra)

fviz_cluster(fit, data = scaled_df, ellipse.type = "t", geom = "point", stand = FALSE)


#####################################
# PROBLEM 3

# Load required libraries
library(tidyverse)

# Read the wine quality dataset
wine_data <- read.csv("winequality-white.csv", header = TRUE, sep = ";", check.names = FALSE)


# Center and scale the data (excluding the quality target variable)
# Centering and scaling is necessary since the variables have different units and scales
wine_scaled <- scale(wine_data[, -which(names(wine_data) == "quality")])

# Perform hierarchical clustering using single and complete linkage
single_linkage <- hclust(dist(wine_scaled), method = "single")
complete_linkage <- hclust(dist(wine_scaled), method = "complete")

# Plot the dendrograms
plot(single_linkage, main = "Single Linkage", xlab = "Wine Samples", ylab = "Distance", sub = "")
plot(complete_linkage, main = "Complete Linkage", xlab = "Wine Samples", ylab = "Distance", sub = "")

# Determine the distance value where the two penultimate clusters are merged
single_penultimate <- single_linkage$height[length(single_linkage$height) - 1]
complete_penultimate <- complete_linkage$height[length(complete_linkage$height) - 1]

cat("Single linkage penultimate distance:", single_penultimate, "\n")
cat("Complete linkage penultimate distance:", complete_penultimate, "\n")

# Obtain the two clusters using the cutree method
single_clusters <- cutree(single_linkage, k = 2)
complete_clusters <- cutree(complete_linkage, k = 2)


# Calculate summary statistics for each cluster
wine_data_single <- wine_data %>%
  mutate(cluster = single_clusters) %>%
  group_by(cluster) %>%
  summarise_all(list(mean, sd))

wine_data_complete <- wine_data %>%
  mutate(cluster = complete_clusters) %>%
  group_by(cluster) %>%
  summarise_all(list(mean, sd))

cat("\nSummary statistics for single linkage clustering:\n")
print(wine_data_single)

cat("\nSummary statistics for complete linkage clustering:\n")
print(wine_data_complete)

# Determine the feature means with the largest differences
single_diff <- abs(wine_data_single[1, -ncol(wine_data_single)] - wine_data_single[2, -ncol(wine_data_single)])
complete_diff <- abs(wine_data_complete[1, -ncol(wine_data_complete)] - wine_data_complete[2, -ncol(wine_data_complete)])

cat("\nFeature means with the largest differences for single linkage:\n")
print(single_diff[which.max(single_diff)])

cat("\nFeature means with the largest differences for complete linkage:\n")
print(complete_diff[which.max(complete_diff)])

# Determine which linkage method produces a more balanced clustering
single_balance <- abs(sum(single_clusters == 1) - sum(single_clusters == 2))
complete_balance <- abs(sum(complete_clusters == 1) - sum(complete_clusters == 2))

cat("\nCluster size differences:")
cat("\nSingle linkage:", single_balance)
cat("\nComplete linkage:", complete_balance)

if (single_balance < complete_balance) {
  cat("\n\nSingle linkage produces a more balanced clustering.")
} else if (single_balance < complete_balance) {
  cat("\n\nComplete linkage produces a more balanced clustering.")
} else {
  cat("\n\nComplete linkage and single linkage produced as balanced clustering.")
}






