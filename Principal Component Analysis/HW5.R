# Load required packages
library(readr)

# Load the wine dataset
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
wine_data <- read_csv(url, col_names = FALSE)

# Assign column names
colnames(wine_data) <- c("Class", "Alcohol", "Malic_Acid", "Ash", "Alcalinity_of_Ash", "Magnesium", "Total_Phenols", "Flavanoids", "Nonflavanoid_Phenols", "Proanthocyanins", "Color_Intensity", "Hue", "OD280_OD315", "Proline")

# Perform PCA with scaling 
scaled_pca <- prcomp(wine_data[,-1], scale = TRUE)

biplot(scaled_pca)

# Identify the feature opposite to 'Hue'
# From the biplot, it appears that 'Malic_Acid' is pointed in the opposite direction of 'Hue'

# Calculate the correlation between 'Malic_Acid' and 'Hue'
macide <- wine_data$Malic_Acid
cor_hue <- cor(macide, wine_data$Hue)
cat("Relation b/w Hue and Malic acid", cor_hue, "\n")

# Create a scree plot
plot(scaled_pca, type = "lines")

# Calculate the variance explained by PC1 and PC2
summed <- sum(scaled_pca$sdev^2) * 100
two_variances <- scaled_pca$sdev^2 / summed
cat("%n Variance explained by PC1:", two_variances[1], "%\t","Variance explained by PC2:", two_variances[2], "%\n")



###Problem3
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
} else if (single_balance > complete_balance) {
  cat("\n\nComplete linkage produces a more balanced clustering.")
} else {
  cat("\n\nComplete linkage and single linkage produced equally balanced clusterings.")
}
