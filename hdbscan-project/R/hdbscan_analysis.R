library(dbscan)
library(ggplot2)

# Basic HDBSCAN analysis function
basic_hdbscan <- function(data, min_pts = 5) {
  clusters <- hdbscan(data, minPts = min_pts)
  return(clusters)
}
