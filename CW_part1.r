#====1St sub task======

# Part a
# Extract the features (attributes) excluding the output variable
library(cluster)
library(cluster)
library(factoextra)
install.packages("fviz_nbclust")
install.packages("factoextra")
install.packages("cluster")
library(factoextra)

install.packages("NbClust")  # Install the NbClust package if not already installed
library(NbClust) 

library(readxl)
Whitewine_v6 <- read_excel("E:/sem4/ML/CW/Whitewine_v6.xlsx")
View(Whitewine_v6)
features <- as.matrix(Whitewine_v6[, 1:11])




# Identifying using IQR method
outliers <- c()
for (i in 1:11) {
  lower_quantile <- quantile(features[, i], probs = 0.25)
  upper_quantile <- quantile(features[, i], probs = 0.75)
  inter_quantile_range <- upper_quantile - lower_quantile
  lower_outliers <- which(features[, i] < lower_quantile - 1.5 * inter_quantile_range)
  upper_outliers <- which(features[, i] > upper_quantile + 1.5 * inter_quantile_range)
  outliers <- c(outliers, upper_outliers, lower_outliers)
}
outliers <- unique(outliers)
head(outliers)

# Remove outliers from the dataset
wine_cleaned_features <- features[-outliers, ]
View(wine_cleaned_features)

# Create box plots using the cleaned dataset
par(mfrow = c(3, 4))  # Adjust layout for 11 features
for (i in 1:11) {
  # Create box plot
  boxplot(wine_cleaned_features[, i], main = colnames(wine_cleaned_features)[i])
}
par(mfrow = c(1, 1))  # Reset layout

# Perform standardization
scaled_features <- scale(wine_cleaned_features)


# Check the scaled features
head(scaled_features)
View(scaled_features)

# part b

#====== NB clust

str(scaled_features)
set.seed(26)

nb <- NbClust(scaled_features, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
nb 
barplot (table(nb$Best.nc[1,]), xlab = "Number of Clusters", ylab = "Value", main = "Number of Clusters Based on NBclust")


#====== Elbow method done
elbow <- fviz_nbclust(scaled_features,kmeans,method = "wss")+labs(title = "Elbow Method")
elbow
#======  Gap statistics done

# plot using clusGap function
gap_stat <- clusGap(scaled_features,FUN = kmeans, K.max = 10)
print(gap_stat)
plot(gap_stat)


# Silhouette Method ->  this approach measures the quality of clustering and determines how well each object lies within its cluster
#                   ->  A high average silhouette width indicates a good clustering
#                   ->  The similarity/dissimilarity score is measured between the assigned cluster and the next best/nearest cluster for each data poin


silloute_k <- fviz_nbclust(scaled_features, kmeans, method = 'silhouette')
silloute_k

 # PART C
# Determine the most favored value of k using nb clust method

most_favored_k <- as.numeric(names(which.max(table(nb$Best.nc))))
most_favored_k
set.seed(26)



# Perform k-means clustering using the most favored k
kmeans_result <- kmeans(scaled_features, centers = most_favored_k)
kmeans_result

# visualizing clusters 
cluster_visualizing <- fviz_cluster(kmeans_result,data = scaled_features,stand = FALSE,geom="point",
                                    ellipse=TRUE,ellipse.type="norm",pointsize=2,alpha.point=0.8)+labs(title = "vishual representaion 0f kemans clustering")

print(cluster_visualizing)
# Get cluster centers
centers <- kmeans_result$centers
centers

# Get cluster assignments for each data point
cluster_assignments <- kmeans_result$cluster
cluster_assignments

# Print kmeans output
print(kmeans_result)

# Print information
cat("Cluster Centers:\n")
print(centers)

cat("\nCluster Assignments:\n")
print(cluster_assignments)

# Calculate BSS and WSS
BSS <- sum(kmeans_result$betweenss)  # Between-cluster sum of squares
BSS
WSS <- kmeans_result$tot.withinss     # Within-cluster sum of squares
WSS

# Calculate TSS
TSS <- BSS + WSS

# Calculate BSS/TSS ratio
BSS_TSS_ratio <- BSS / TSS

# Print BSS, WSS, and BSS/TSS ratio
cat("Between-cluster sum of squares (BSS):", BSS, "\n")
cat("Within-cluster sum of squares (WSS):", WSS, "\n")
cat("Ratio of BSS to TSS:", BSS_TSS_ratio, "\n")


# part d



# Generate silhouette plot for the k-means clustering results
sil_plot <- silhouette(kmeans_result$cluster, dist(scaled_features))

# Plot silhouette plot with colors based on cluster assignments
plot(sil_plot, col = rainbow(most_favored_k), border = NA, main = "Silhouette Plot")

# Compute average silhouette width score
avg_sil_width <- mean(sil_plot[, "sil_width"])

# Print average silhouette width score
cat("Average Silhouette Width Score:", avg_sil_width, "\n \n")

# Discussion:
# The silhouette plot displays how close each point in one cluster is to points in neighboring clusters.
# The average silhouette width score ranges from -1 to 1, where a higher value indicates better cluster separation.
# A score close to 1 suggests that the clusters are well-separated, while a score close to 0 indicates overlapping clusters.
# Negative scores imply that data points may have been assigned to the wrong clusters.
# Based on the silhouette plot and average silhouette width score, we can assess the quality of the obtained clusters.
# If the average silhouette width score is close to 1 and the silhouette plot shows distinct clusters, it indicates high-quality clusters.
  
  
#==========2 nd sub task==================

#==============part e

# Perform PCA analysis
#We standardize the data scale and implement PCA in R using ‘prcomp’ as code below. Then, we can see the results of PCA statistics from these data.
pca_result <- prcomp(scaled_features, scale. = TRUE)
pca_result
summary(pca_result)
#  points from -> summary(pca_result)
#  Standard Deviation is the eigenvalue of each principal component, which means that the eigenvalue 1 is 1.8637, eigenvalue 2 is 1.2497 and so on. 
#  proportion of varia -> . 
#                      -> . PC1 has a 0.3158 proportion of variance. That is, PC1 is able to explain ~32% 

#pca_result_transform = as.data.frame(-pca_result$x[,1:3])   
#pca_result_transform

#The center and scale components correspond to the means and standard deviations of the variables that were used for scaling prior to implementing PCA.
# means
pca_result$center

# standard deviations
pca_result$scale

# Extract eigenvalues and eigenvectors
eigenvalues <- pca_result$sdev^2
eigenvalues
eigenvectors <- pca_result$rotation
eigenvectors

# plot the first two principal components using biplot
#the default is choices = 1:2
windows(width = 10, height = 8)  
biplot(pca_result, scale = 0)


# Compute cumulative scores
#To compute the proportion of variance explained by each principal component, we simply divide the variance explained by each principal component by the total variance explained by all four principal components:

cumulative_scores <- (eigenvalues) / sum(eigenvalues)
cumulative_scores
round(cumulative_scores,2)

varPercent <- cumulative_scores*100
windows(width = 10, height = 8)
barplot(varPercent, xlab='PC', ylab='Percent Variance', names.arg=1:length(varPercent), las=1, ylim=c(0, max(varPercent)), col='gray')
abline(h=1/ncol(USArrests)*100, col='red')


# Find the number of principal components with cumulative score > 85%
pca_selected <- pca_result$x[, varPercent <= 85]
pca_selected

# Create a new transformed dataset with principal components as attributes
wine_transformed <- as.data.frame(pca_selected)
wine_transformed

# Choosing the number of principal components is crucial in PCA.
#We aim to retain a significant amount of variance from the original dataset while reducing dimensionality.
#In this case, selecting principal components with a cumulative score > 85% ensures that we capture a substantial portion of the variability in the data.
#This balance between dimensionality reduction and retaining information guides our choice, making the analysis more manageable without sacrificing too much information.





# Display eigenvalues, eigenvectors, cumulative scores, and selected principal components
print("Eigenvalues:")
print(eigenvalues)
print("Eigenvectors:")
print(eigenvectors)
print("Cumulative Scores:")
print(cumulative_scores)


# part f 

# Apply NbClust to PCA-based dataset

##########
set.seed(26)

nb_pca <- NbClust(wine_transformed, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
nb_pca
barplot(nb_pca$Best.nc[1,], xlab = "Number of Clusters", ylab = "Value", main = "Number of Clusters Based on NBclust after applying pca")

# Apply Elbow method to PCA-based dataset

############
elbow <- fviz_nbclust(wine_transformed,kmeans,method = "wss")+labs(title = "Elbow Method")
elbow

# Apply Gap statistics to PCA-based dataset
################
gap_stat_pca <- fviz_nbclust(wine_transformed, kmeans, method = 'gap_stat')
# Plot the results
plot(gap_stat_pca, main = "Gap Statistics afrer applying pca")

# Apply Silhouette method to PCA-based dataset
install.packages("factoextra")
library(factoextra)

silloute_k <- fviz_nbclust(wine_transformed, kmeans, method = 'silhouette')
silloute_k



#############




# part g

# Perform k-means clustering using the most favored k from automated methods
kmeans_result_pca <- kmeans(wine_transformed, centers = most_favored_k_pca)

# Print kmeans output
print(kmeans_result_pca)


most_favored_k_pca <- as.numeric(names(which.max(table(nb_pca$Best.nc))))
print(most_favored_k_pca)

# visualizing clusters after pca 
cluster_visualizing_pca <- fviz_cluster(kmeans_result_pca,data = wine_transformed,stand = FALSE,geom="point",
                                    ellipse=TRUE,ellipse.type="norm",pointsize=2,alpha.point=0.8)+labs(title = "vishual representaion 0f kemans clustering after pca")
print(cluster_visualizing_pca)


# Calculate BSS and WSS
BSS_pca <- sum(kmeans_result_pca$betweenss)  # Between-cluster sum of squares
WSS_pca <- kmeans_result_pca$tot.withinss     # Within-cluster sum of squares

# Calculate TSS
TSS_pca <- BSS_pca + WSS_pca

# Calculate BSS/TSS ratio
BSS_TSS_ratio_pca <- BSS_pca / TSS_pca

# Print BSS, WSS, and BSS/TSS ratio
cat("Between-cluster sum of squares (BSS):", BSS_pca, "\n")
cat("Within-cluster sum of squares (WSS):", WSS_pca, "\n")
cat("Ratio of BSS to TSS:", BSS_TSS_ratio_pca, "\n")

# part h

###################

# Generate silhouette plot for the k-means clustering results
sil_plot_pca <- silhouette(kmeans_result_pca$cluster, dist(wine_transformed))

# Plot silhouette plot with colors based on cluster assignments
plot(sil_plot_pca, col = rainbow(most_favored_k), border = NA, main = "Silhouette Plot for PCA-based K-means")

# Compute average silhouette width score
avg_sil_width <- mean(sil_plot_pca[, "sil_width"])
#############

# Generate silhouette plot for the new k-means attempt
install.packages("cluster")  
library(cluster)

sil_plot_pca <- silhouette(kmeans_result_pca$cluster, dist(wine_transformed))

# Plot silhouette plot
plot(sil_plot_pca, main = "Silhouette Plot for PCA-based K-means")

# Compute average silhouette width score
avg_sil_width_pca <- mean(sil_plot_pca[, "sil_width"])

# Print average silhouette width score
cat("Average Silhouette Width Score:", avg_sil_width_pca, "\n \n")

# Discussion:
# The silhouette plot for the PCA-based k-means clustering displays how close each point in one cluster is to points in neighboring clusters.
# The average silhouette width score is a measure of the overall quality of the clustering. 
# A higher score indicates better-defined clusters, with values close to 1 suggesting well-separated clusters.
# On the other hand, negative values indicate that some points may have been assigned to the wrong clusters, while values close to 0 suggest overlapping clusters.
# Therefore, by analyzing the silhouette plot and the average silhouette width score, we can assess the level of "quality" of the obtained clusters.

# part i
install.packages("fpc")
# Load the cluster package
library(cluster)

install.packages("clusterCrit") 
library(clusterCrit)

# Load the fpc package
library(clusterCrit)
cluster_stats <- cluster.stats(dist(wine_transformed), kmeans_result_pca$cluster)

# Extract the Calinski-Harabasz Index
calinski_harabasz_index <- cluster_stats$ch

# Print the Calinski-Harabasz Index
cat("Calinski-Harabasz Index:", calinski_harabasz_index, "\n")








