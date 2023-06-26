##### A ######
data = read.csv('Clustering.csv',header=TRUE) #Clustering.csve

# Step 1: Scale the dataset
scaled_data <- scale(data)

# Step 2: Calculate Euclidean distances
dist_matrix <- dist(scaled_data, method = "euclidean")

# Step 3: Create a distance matrix
dist_matrix <- as.matrix(dist_matrix)

# Step 4: Plot the heatmap
install.packages('reshape2')
library(reshape2)
library(ggplot2)

heatmap <- ggplot(data = melt(dist_matrix), aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue") +
  theme_minimal() +
  labs(x = "Data Points", y = "Data Points", title = "Euclidean Distance Heatmap")

print(heatmap)

##### B ######
# Method 1: Elbow method
wss <- c()
for (i in 1:10) {
  kmeans_result <- kmeans(scaled_data, centers = i, nstart = 10)
  wss[i] <- kmeans_result$tot.withinss
}

plot(1:10, wss, type = "b", xlab = "Number of Clusters (k)", ylab = "Within-cluster Sum of Squares")

# Method 2: Silhouette coefficient
install.packages('tidyclust')
library(tidyclust)
silhouette <- c()
for (i in 2:10) {
  kmeans_result <- kmeans(scaled_data, centers = i, nstart = 10)
  silhouette[i] <- silhouette_avg(kmeans_result$cluster, dist(scaled_data))
}

plot(2:10, silhouette[2:10], type = "b", xlab = "Number of Clusters (k)", ylab = "Average Silhouette Coefficient")

# Method 3: Gap statistic
library(cluster)

set.seed(123)
gap_stat <- clusGap(scaled_data, FUNcluster = kmeans, nstart = 10, K.max = 10, B = 50)

plot(gap_stat, main = "Gap Statistic", xlab = "Number of Clusters (k)")

##### C ######
# Perform k-means clustering
optimal_k = 2
kmeans_result <- kmeans(scaled_data, centers = optimal_k, nstart = 10)

# Plot the resulting cluster solution
library(ggplot2)

cluster_plot <- ggplot(data = data, aes(x = x, y = y, color = as.factor(kmeans_result$cluster))) +
  geom_point() +
  scale_color_discrete(name = "Cluster") +
  theme_minimal() +
  labs(x = "x", y = "y", title = "Cluster Analysis")

print(cluster_plot)

##### D ######
# Perform agglomerative clustering
agglomerative_result <- hclust(dist(scaled_data), method = "complete")

# Plot the dendrogram
plot(agglomerative_result, main = "Dendrogram", xlab = "Data Points")


