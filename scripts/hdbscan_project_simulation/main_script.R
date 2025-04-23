# Simulates ML development with HDBSCAN

library(lubridate)
library(dplyr)
library(dbscan)
library(ggplot2)

# Configuration
start_date <- as.Date("2024-09-06")
end_date <- as.Date("2025-02-28")
repo_dir <- "../data/data-polio-statistics/hdbscan_project_simulation"
busy_start <- as.Date("2024-11-01")
busy_end <- as.Date("2024-12-15")

# Create repo structure
dir.create(repo_dir)
setwd(repo_dir)
system("git init")

# Generate synthetic dataset for clustering
set.seed(42)
generate_dataset <- function() {
  n_points <- 1000
  data <- data.frame(
    x = c(rnorm(n_points/2, mean = 0, sd = 0.5), 
          rnorm(n_points/2, mean = 3, sd = 0.8)),
    y = c(rnorm(n_points/2, mean = 0, sd = 0.3),
          rnorm(n_points/2, mean = 4, sd = 0.5)),
    group = rep(c("A", "B"), each = n_points/2)
  )
  write.csv(data, "cluster_data.csv", row.names = FALSE)
}

# Create evolving HDBSCAN script
update_script <- function(day) {
  script_content <- paste0(
    "# HDBSCAN Cluster Analysis\n",
    "# Last updated: ", Sys.Date(), "\n\n",
    "library(dbscan)\n",
    "library(ggplot2)\n\n",
    "data <- read.csv('cluster_data.csv')\n",
    "coordinates <- as.matrix(data[,1:2])\n\n",
    ifelse(day > 10, "# Added parameter tuning after day 10\nmin_pts <- sample(5:15, 1)\n", ""),
    ifelse(day > 20, "# Added visualization improvements after day 20\nplot_theme <- theme_minimal()\n", ""),
    ifelse(day > 30, "# Added cluster validation metrics after day 30\nlibrary(cluster)\n", ""),
    "\n# Perform HDBSCAN clustering\n",
    "hdbscan_result <- hdbscan(coordinates, minPts = ", 
    ifelse(day > 10, "min_pts", "10"), ")\n\n",
    ifelse(day > 30, 
           "# Calculate silhouette scores\n",
           ""),
    ifelse(day > 30,
           "sil_score <- silhouette(hdbscan_result$cluster, dist(coordinates))\n",
           ""),
    "# Visualize results\n",
    "ggplot(data, aes(x, y, color = factor(hdbscan_result$cluster))) +\n",
    "  geom_point(alpha = 0.6) +\n",
    ifelse(day > 20, "  plot_theme +\n", ""),
    "  labs(title = 'HDBSCAN Clustering Results')\n\n",
    "# Save output\n",
    "write.csv(data.frame(coordinates, cluster = hdbscan_result$cluster),\n",
    "          'cluster_results.csv', row.names = FALSE)"
  )
  
  writeLines(script_content, "hdbscan_analysis.R")
}

# Simulate git commits
current_date <- start_date
while (current_date <= end_date) {
  #setwd(repo_dir)
  
  # Skip weekends (optional)
  if (format(current_date, "%u") %in% c("6", "7")) {
    current_date <- current_date + days(1)
    next
  }
  
  # Determine commit range based on time period
  if (current_date >= busy_start && current_date <= busy_end) {
    # Busy period (Nov to mid-Dec) - more commits possible
    commits_today <- sample(0:15, 1, prob = c(rep(0.02, 6), rep(0.1, 5), rep(0.15, 5)))
    # Higher chance of having many commits during this period
  } else {
    # Normal period
    commits_today <- sample(0:10, 1, prob = c(0.3, rep(0.1, 10)))
  }
  
  if (commits_today > 0) {
    # Update files
    generate_dataset()
    update_script(as.numeric(current_date - start_date))
    
    for (i in 1:commits_today) {
      # Set Git date
      hour <- sample(9:23, 1)  # Extended hours during busy period
      minute <- sample(0:59, 1)
      timestamp <- paste0(
        format(current_date, "%Y-%m-%d"),
        " ", hour, ":", minute, ":00"
      )
      
      # Make commit
      system(paste0("git add ."))
      
      # More diverse commit messages during busy period
      if (current_date >= busy_start && current_date <= busy_end) {
        commit_message <- paste0(
          "Day ", as.numeric(current_date - start_date), 
          ": ",
          sample(c("Optimized HDBSCAN parameters",
                   "Fixed critical clustering bug",
                   "Implemented new validation metrics",
                   "Refactored core algorithm",
                   "Added parallel processing",
                   "Enhanced visualization",
                   "Updated documentation",
                   "Improved data preprocessing",
                   "Fixed edge cases",
                   "Added unit tests",
                   "Performance improvements",
                   "Integration with new dataset",
                   "Finalized cluster analysis",
                   "Pre-submission fixes",
                   "Meeting requested changes"), 1)
        )
      } else {
        commit_message <- paste0(
          "Day ", as.numeric(current_date - start_date), 
          ": ",
          sample(c("Improved clustering params", 
                   "Added visualization",
                   "Fixed data loading",
                   "Optimized HDBSCAN",
                   "Updated documentation",
                   "Refactored code",
                   "Added error handling",
                   "Enhanced preprocessing"), 1)
        )
      }
      
      system(paste0(
        'git commit -m "', commit_message, '" --date="', timestamp, '"'
      ))
    }
  }
  
  current_date <- current_date + days(1)
}

# Create README.md
writeLines(paste0(
  "# HDBSCAN Clustering Project\n\n",
  "This project demonstrates HDBSCAN clustering on synthetic data.\n\n",
  "## Project Timeline\n",
  "- Development period: ", format(start_date, "%B %d, %Y"), " to ", format(end_date, "%B %d, %Y"), "\n",
  "## Files\n",
  "- `hdbscan_analysis.R`: Main analysis script\n",
  "- `cluster_data.csv`: Synthetic dataset\n",
  "- `cluster_results.csv`: Output with cluster assignments"
), "README.md")

system("git add README.md")
system('git commit -m "Add project README" --date="2024-09-06 10:00:00"')

cat("Git history simulation complete!\n")
cat("Repo created at:", normalizePath(repo_dir), "\n")
