# Simulates ML development with HDBSCAN from Sep 2024 to Feb 2025

library(lubridate)
library(dplyr)
library(dbscan)
library(ggplot2)

# Configuration
start_date <- as.Date("2024-09-06")
end_date <- as.Date("2025-02-28")
repo_dir <- "../ ../data/data-polio-statistics/hdbscan_project_simulation"
busy_start <- as.Date("2024-11-01")
busy_end <- as.Date("2024-12-10")  # Slightly shorter peak period

# Create repo structure
if (!dir.exists(repo_dir)) {
  dir.create(repo_dir)
}
#setwd(repo_dir)
system("git init")

# Generate evolving synthetic dataset for clustering
set.seed(42)
generate_dataset <- function(day) {
  n_points <- 1000 + sample(-100:100, 1)  # Vary slightly over time
  noise_points <- ifelse(day > 50, 50, 0)  # Add noise after 50 days
  
  data <- data.frame(
    x = c(rnorm(n_points/2, mean = 0, sd = 0.5), 
          rnorm(n_points/2, mean = 3, sd = 0.8),
          runif(noise_points, min = -2, max = 5)),
    y = c(rnorm(n_points/2, mean = 0, sd = 0.3),
          rnorm(n_points/2, mean = 4, sd = 0.5),
          runif(noise_points, min = -1, max = 6)),
    group = rep(c("A", "B"), each = n_points/2)
  )
  if (noise_points > 0) {
    data$group <- c(data$group[1:n_points], rep("noise", noise_points))
  }
  write.csv(data, "cluster_data.csv", row.names = FALSE)
}

# Create evolving HDBSCAN script with progressive improvements
update_script <- function(day) {
  # Progressive improvements
  has_parameter_tuning <- day > 15
  has_visualization <- day > 25
  has_validation <- day > 40
  has_advanced_features <- day > 60
  has_final_optimizations <- day > 90
  
  # During peak period (Nov-Dec), add more sophisticated features
  if (Sys.Date() >= busy_start && Sys.Date() <= busy_end) {
    has_advanced_features <- day > 30
    has_final_optimizations <- day > 45
  }
  
  script_content <- paste0(
    "# HDBSCAN Cluster Analysis\n",
    "# Version: ", 
    ifelse(day < 30, "1.0", 
           ifelse(day < 60, "2.0", 
                  ifelse(day < 90, "3.0", "4.0"))),
    "\n# Last updated: ", Sys.Date(), "\n\n",
    "library(dbscan)\n",
    "library(ggplot2)\n",
    ifelse(has_validation, "library(cluster)\n", ""),
    ifelse(has_advanced_features, "library(fpc)\n", ""),
    "\n# Load and preprocess data\n",
    "data <- read.csv('cluster_data.csv')\n",
    "coordinates <- as.matrix(data[,1:2])\n\n",
    ifelse(has_parameter_tuning, 
           "# Parameter tuning\nmin_pts <- sample(5:15, 1)\n", ""),
    ifelse(has_advanced_features,
           "# Advanced preprocessing\ncoordinates <- scale(coordinates)\n", ""),
    "\n# Perform HDBSCAN clustering\n",
    "hdbscan_result <- hdbscan(coordinates, minPts = ", 
    ifelse(has_parameter_tuning, "min_pts", "10"), 
    ifelse(has_final_optimizations, 
           ", gen_simplified_tree = TRUE, gen_min_span_tree = TRUE)", 
           ")"),
    "\n\n",
    ifelse(has_validation, 
           "# Cluster validation metrics\n",
           ""),
    ifelse(has_validation,
           "sil_score <- silhouette(hdbscan_result$cluster, dist(coordinates))\n",
           ""),
    ifelse(has_advanced_features,
           "dunn_index <- cluster.stats(dist(coordinates), hdbscan_result$cluster)$dunn\n",
           ""),
    "\n# Visualize results\n",
    "plot <- ggplot(data, aes(x, y, color = factor(hdbscan_result$cluster))) +\n",
    "  geom_point(alpha = 0.6, size = ", 
    ifelse(has_visualization, "1.5", "1"), ") +\n",
    ifelse(has_visualization, 
           "  theme_minimal() +\n  scale_color_viridis_d() +\n", 
           ""),
    ifelse(has_final_optimizations,
           "  ggtitle('HDBSCAN Clustering Results', subtitle = 'Final Optimized Version') +\n",
           "  labs(title = 'HDBSCAN Clustering Results') +\n"),
    "  theme(legend.position = 'none')\n\n",
    "# Save output\n",
    "write.csv(data.frame(coordinates, cluster = hdbscan_result$cluster),\n",
    "          'cluster_results.csv', row.names = FALSE)\n\n",
    ifelse(has_advanced_features,
           "# Save diagnostic plots\n",
           ""),
    ifelse(has_advanced_features,
           "ggsave('cluster_plot.png', plot, width = 8, height = 6, dpi = 300)\n",
           "")
  )
  
  writeLines(script_content, "hdbscan_analysis.R")
}

# Simulate git commits with realistic patterns
current_date <- start_date
while (current_date <= end_date) {
  # Skip weekends (but allow occasional weekend work)
  is_weekend <- format(current_date, "%u") %in% c("6", "7")
  
  # Determine commit intensity
  in_peak_period <- current_date >= busy_start && current_date <= busy_end
  late_phase <- current_date >= as.Date("2025-01-15") #====================================
  
  if (in_peak_period) {
    # High activity period - more commits and higher chance of weekend work
    commits_today <- ifelse(is_weekend, 
                            sample(0:5, 1, prob = c(0.7, rep(0.3/5, 5))),
                            sample(5:15, 1, prob = c(rep(0.02, 5), rep(0.15, 10))))
  } else if (late_phase) {
    # Finalization phase - fewer but more substantial commits
    commits_today <- sample(0:5, 1, prob = c(0.3, 0.3, 0.2, 0.1, 0.05, 0.05))
  } else {
    # Normal development period
    commits_today <- ifelse(is_weekend, 
                            sample(0:2, 1, prob = c(0.8, 0.15, 0.05)),
                            sample(0:8, 1, prob = c(0.3, rep(0.7/8, 8))))
  }
  
  if (commits_today > 0) {
    # Update files with progressive improvements
    generate_dataset(as.numeric(current_date - start_date))
    update_script(as.numeric(current_date - start_date))
    
    for (i in 1:commits_today) {
      # Set Git date with realistic hours
      if (in_peak_period) {
        hour <- sample(rep(9:18, each = 3), 1)  # Corrected sampling
      } else {
        hour <- sample(c(rep(10:12, each = 5), 13:17), 1)
      }
      minute <- sample(0:59, 1)
      timestamp <- paste0(
        format(current_date, "%Y-%m-%d"),
        " ", hour, ":", minute, ":00"
      )
      
      # Make commit
      system(paste0("git add ."))
      
      # Context-aware commit messages
      day_num <- as.numeric(current_date - start_date)
      
      if (in_peak_period) {
        commit_messages <- c(
          paste("Optimize HDBSCAN parameters for day", day_num),
          "Implement cluster validation framework",
          "Add advanced visualization options",
          "Refactor core clustering pipeline",
          "Fix memory issues with large datasets",
          "Add parallel processing support",
          "Implement noise point handling",
          "Update documentation and examples",
          "Add unit tests for core functions",
          "Improve cluster stability metrics",
          "Optimize runtime performance",
          "Prepare results for publication",
          "Address reviewer comments",
          "Finalize analysis pipeline",
          "Add comprehensive error handling"
        )
      } else if (late_phase) {
        commit_messages <- c(
          "Final performance optimizations",
          "Update documentation for release",
          "Clean up codebase",
          "Fix minor bugs in visualization",
          "Prepare final dataset",
          "Update README with usage instructions"
        )
      } else {
        commit_messages <- c(
          paste("Initial implementation day", day_num),
          "Add basic clustering functionality",
          "Fix data loading issues",
          "Improve parameter handling",
          "Add basic visualization",
          "Update documentation",
          "Refactor code structure",
          "Add error handling",
          "Improve data preprocessing"
        )
      }
      
      commit_message <- sample(commit_messages, 1)
      
      # Add more detail during peak period
      if (in_peak_period && runif(1) > 0.7) {
        commit_message <- paste0(commit_message, 
                                 sample(c("", 
                                          " - improved stability",
                                          " - critical fix",
                                          " - major optimization",
                                          " - final review"), 1))
      }
      
      system(paste0(
        'git commit -m "', commit_message, '" --date="', timestamp, '"'
      ))
    }
  }
  
  current_date <- current_date + days(1)
}

# Create comprehensive README.md
writeLines(paste0(
  "# HDBSCAN Clustering Project\n\n",
  "Advanced density-based clustering analysis using HDBSCAN algorithm.\n\n",
  "## Project Timeline\n",
  "- Started: ", format(start_date, "%B %d, %Y"), "\n",
  "- Completed: ", format(end_date, "%B %d, %Y"), "\n",
  "- Intensive development period: November 2024\n\n",
  "## Key Features\n",
  "- Progressive parameter optimization\n",
  "- Cluster validation metrics\n",
  "- Advanced visualization\n",
  "- Noise handling\n",
  "- Performance optimizations\n\n",
  "## Files\n",
  "- `hdbscan_analysis.R`: Main analysis script (evolved through versions)\n",
  "- `cluster_data.csv`: Synthetic dataset (with progressive complexity)\n",
  "- `cluster_results.csv`: Output with cluster assignments\n\n",
  "## Usage\n",
  "Run the analysis script to perform HDBSCAN clustering on the provided data.\n\n",
  "```\n",
  "source('hdbscan_analysis.R')\n",
  "```"
), "README.md")

system("git add README.md")
system('git commit -m "Add comprehensive project README" --date="2025-02-28 11:00:00"')

cat("Git history simulation complete!\n")
cat("Repo created at:", normalizePath(repo_dir), "\n")