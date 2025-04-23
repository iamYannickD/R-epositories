# HDBSCAN Cluster Analysis
# Version: 2.0
# Last updated: 2025-04-23

library(dbscan)
library(ggplot2)
library(cluster)

# Load and preprocess data
data <- read.csv('cluster_data.csv')
coordinates <- as.matrix(data[,1:2])

# Parameter tuning
min_pts <- sample(5:15, 1)

# Perform HDBSCAN clustering
hdbscan_result <- hdbscan(coordinates, minPts = min_pts)

# Cluster validation metrics
sil_score <- silhouette(hdbscan_result$cluster, dist(coordinates))

# Visualize results
plot <- ggplot(data, aes(x, y, color = factor(hdbscan_result$cluster))) +
  geom_point(alpha = 0.6, size = 1.5) +
  theme_minimal() +
  scale_color_viridis_d() +
  labs(title = 'HDBSCAN Clustering Results') +
  theme(legend.position = 'none')

# Save output
write.csv(data.frame(coordinates, cluster = hdbscan_result$cluster),
          'cluster_results.csv', row.names = FALSE)


