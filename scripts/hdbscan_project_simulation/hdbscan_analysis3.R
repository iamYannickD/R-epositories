# Simulates GitHub activity for HDBSCAN project from Sep 6, 2024 to Feb 28, 2025
# With peak activity between November and beginning of December 2024

library(lubridate)
library(dplyr)

# Configuration
start_date <- as.Date("2024-09-06")
end_date <- as.Date("2025-02-28")
repo_dir <- "hdbscan_project"
busy_start <- as.Date("2024-11-01")
busy_end <- as.Date("2024-12-05")  # Peak period: Nov 1 - Dec 5

# Create repo
if (!dir.exists(repo_dir)) {
  dir.create(repo_dir)
}
setwd(repo_dir)
system("git init")

# Create initial files
writeLines("# HDBSCAN Project\n\nCluster analysis using HDBSCAN algorithm", "README.md")
dir.create("src")
writeLines("library(dbscan)\n\n# Initial script", "src/hdbscan_analysis.R")
system("git add .")
system('git commit -m "Initial commit" --date="2024-09-06 09:30:00"')

# Simulate development history
current_date <- start_date + 1  # Start from day after initial commit

while (current_date <= end_date) {
  # Skip weekends (65% chance)
  if (format(current_date, "%u") %in% c("6", "7") && runif(1) < 0.65) {
    current_date <- current_date + 1
    next
  }
  
  # Determine commit frequency
  in_peak_period <- current_date >= busy_start && current_date <= busy_end
  
  if (in_peak_period) {
    # Peak period: higher activity (3-10 commits/day)
    commits_today <- sample(3:10, 1, prob = c(0.1, 0.15, 0.2, 0.2, 0.15, 0.1, 0.05, 0.05))
  } else {
    # Normal period: 0-5 commits/day
    commits_today <- sample(0:5, 1, prob = c(0.3, 0.25, 0.2, 0.15, 0.07, 0.03))
  }
  
  if (commits_today > 0) {
    for (i in 1:commits_today) {
      # Random time during working hours
      hour <- if (in_peak_period) {
        sample(8:20, 1, prob = c(0.05, 0.1, 0.15, 0.2, 0.2, 0.15, 0.1, 0.05, rep(0.02, 5), 0.03))
      } else {
        sample(9:17, 1, prob = c(0.1, 0.15, 0.2, 0.2, 0.15, 0.1, 0.05, 0.03, 0.02))
      }
      minute <- sample(0:59, 1)
      timestamp <- paste0(format(current_date, "%Y-%m-%d"), " ", hour, ":", minute, ":00")
      
      # Modify files randomly
      if (runif(1) > 0.7) {
        writeLines(sample(c(
          "Fixed bug in cluster assignment",
          "Optimized parameter tuning",
          "Added new visualization",
          "Improved documentation",
          "Refactored core functions"
        ), 1), "temp.txt")
        system("git add temp.txt")
      } else {
        # Modify source code
        code_changes <- sample(c(
          "Added new distance metric option",
          "Improved noise handling",
          "Enhanced plotting functions",
          "Added validation metrics",
          "Fixed memory leak",
          "Optimized tree construction",
          "Added parallel processing option"
        ), 1)
        
        if (in_peak_period && runif(1) > 0.5) {
          code_changes <- paste(code_changes, "- performance improvement")
        }
        
        writeLines(paste("#", code_changes, "\n", readLines("src/hdbscan_analysis.R")), 
                   "src/hdbscan_analysis.R")
        system("git add src/hdbscan_analysis.R")
      }
      
      # Generate commit message
      if (in_peak_period) {
        commit_msg <- paste(sample(c(
          "Optimize", "Refactor", "Fix", "Implement", "Add", "Improve", "Enhance", 
          "Update", "Revise", "Finalize"
        ), 1), 
        sample(c(
          "cluster analysis", "parameter tuning", "visualization", "core algorithm",
          "documentation", "validation metrics", "noise handling", "performance",
          "memory usage", "parallel processing"
        ), 1))
        
        if (runif(1) > 0.7) {
          commit_msg <- paste(commit_msg, sample(c(
            "", "for large datasets", "in main pipeline", "with new metrics", 
            "across all functions", "significantly"
          ), 1))
        }
      } else {
        commit_msg <- paste(sample(c(
          "Update", "Modify", "Adjust", "Clean up", "Edit", "Revise"
        ), 1), 
        sample(c(
          "code", "parameters", "plots", "functions", "script", "documentation"
        ), 1))
      }
      
      system(paste0('git commit -m "', commit_msg, '" --date="', timestamp, '"'))
    }
  }
  
  current_date <- current_date + 1
}

# Finalize project
writeLines(c(readLines("README.md"), 
             "\n## Project Status\nCompleted on February 28, 2025"), 
           "README.md")
system("git add README.md")
system('git commit -m "Finalize project" --date="2025-02-28 16:30:00"')

cat("Git history simulation complete!\n")
cat("Repository created at:", normalizePath(repo_dir), "\n")

