pcaData = read.csv("pca.csv")
attach(pcaData)

pca <- prcomp(pcaData, scale = TRUE)

# Summary of PCA results
summary(pca)

# To decide the number of components, we can consider:
# 1. Scree plot: Plot the standard deviations against the component number and look for an "elbow" point where the eigenvalues start to level off.
plot(pca, type = "l", main = "Scree Plot")  # Scree plot
# 2. Cumulative proportion of variance explained: Identify the number of components that explain a significant portion of the total variance.
cumsum(pca$sdev^2 / sum(pca$sdev^2))  # Cumulative proportion of variance explained


# Extract loadings for the first two components
loadings <- pca$rotation[, 1:7]
summary(loadings)
barplot(loadings[,], beside = TRUE, names.arg = colnames(loadings), xlab = "Principal Component", ylab = "Loading")


biplot(pca, scale = 0)
