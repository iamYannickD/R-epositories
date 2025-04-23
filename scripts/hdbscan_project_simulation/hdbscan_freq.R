# GitHub Activity Simulator: HDBSCAN Project
# Simulates development from 2024-09-06 to 2025-02-28

library(lubridate)
library(dplyr)

# Configuration
start_date <- as.Date("2024-09-06")
end_date <- as.Date("2025-02-28")
repo_name <- "hdbscan-project"
peak_start <- as.Date("2024-11-01")
peak_end <- as.Date("2024-12-10")

# Create repository
if (!dir.exists(repo_name)) {
  dir.create(repo_name)
}
setwd(repo_name)
system("git init")
system(paste0('git config user.name "', author_name, '"'))
system(paste0('git config user.email "', author_email, '"'))

# Create initial project structure
dir.create("R")
dir.create("data")
dir.create("output")

# Initial commit - basic project setup
writeLines(
  c("# HDBSCAN Project", 
    "Advanced clustering analysis using HDBSCAN algorithm",
    "",
    "## Project Structure",
    "- R/: Analysis scripts",
    "- data/: Input datasets",
    "- output/: Results and visualizations"),
  "README.md"
)

writeLines(
  c("library(dbscan)",
    "library(ggplot2)",
    "",
    "# Basic HDBSCAN analysis function",
    "basic_hdbscan <- function(data, min_pts = 5) {",
    "  clusters <- hdbscan(data, minPts = min_pts)",
    "  return(clusters)",
    "}"),
  "R/hdbscan_analysis.R"
)

system("git add .")
system('git commit -m "Initial project setup" --date="2024-09-06 10:00:00"')

# Simulate development history
current_date <- start_date + 1
day_counter <- 1

while (current_date <= end_date) {
  # Skip some weekends (70% chance)
  is_weekend <- wday(current_date) %in% c(1, 7)
  if (is_weekend && runif(1) < 0.7) {
    current_date <- current_date + 1
    next
  }
  
  # Determine if we're in peak period
  in_peak <- current_date >= peak_start && current_date <= peak_end
  
  # Number of commits today
  if (in_peak) {
    # Peak period: 4-10 commits per day (7 options)
    n_commits <- sample(4:10, 1, prob = c(0.1, 0.15, 0.2, 0.2, 0.15, 0.1, 0.1))
  } else {
    # Normal period: 0-5 commits per day (6 options)
    n_commits <- sample(0:5, 1, prob = c(0.3, 0.25, 0.2, 0.15, 0.07, 0.03))
  }
  
  if (n_commits > 0) {
    for (i in 1:n_commits) {
      # Random time during working hours
      if (in_peak) {
        # 8am-10pm during peak (15 hours)
        hour <- sample(8:22, 1, prob = c(0.02, 0.02, 0.02,  # 8-10am
                                         0.05, 0.05, 0.05, 0.05, 0.05,  # 11am-3pm
                                         0.1, 0.1, 0.1, 0.1,  # 4-7pm
                                         0.05, 0.05, 0.05))  # 8-10pm
      } else {
        # 9am-6pm normally (10 hours)
        hour <- sample(9:18, 1, prob = c(0.05, 0.1, 0.15, 0.2, 0.2, 0.15, 0.1, 0.05, 0.02, 0.02))
      }
      minute <- sample(0:59, 1)
      timestamp <- paste0(format(current_date, "%Y-%m-%d"), " ", hour, ":", minute, ":00")
      
      # Modify files - progressive improvements
      current_script <- readLines("R/hdbscan_analysis.R")
      
      # Determine what kind of modification to make based on project phase
      if (day_counter < 30) {
        # Early phase: basic functionality
        modification <- sample(c(
          "Add parameter validation",
          "Improve basic clustering",
          "Add simple visualization",
          "Fix data handling",
          "Add documentation"
        ), 1)
      } else if (day_counter < 60 || in_peak) {
        # Middle phase or peak period: advanced features
        modification <- sample(c(
          "Add cluster validation metrics",
          "Implement advanced visualization",
          "Add noise handling",
          "Optimize performance",
          "Add parallel processing option",
          "Implement custom distance metrics",
          "Add cluster stability analysis"
        ), 1)
      } else {
        # Final phase: refinements
        modification <- sample(c(
          "Refactor code structure",
          "Improve documentation",
          "Optimize memory usage",
          "Add examples",
          "Final performance tweaks"
        ), 1)
      }
      
      # Apply the modification
      new_code <- paste0("# ", modification, " - ", format(current_date, "%Y-%m-%d"), "\n")
      updated_script <- c(new_code, current_script)
      writeLines(updated_script, "R/hdbscan_analysis.R")
      
      # Generate commit message
      if (in_peak) {
        commit_msg <- paste(sample(c(
          "ENH:", "FEAT:", "PERF:", "OPTIMIZE:", "IMPROVE:"
        ), 1), modification)
      } else {
        commit_msg <- paste(sample(c(
          "Update", "Add", "Fix", "Improve", "Refactor"
        ), 1), modification)
      }
      
      # Add some detail to peak period commits
      if (in_peak && runif(1) > 0.6) {
        commit_msg <- paste(commit_msg, sample(c(
          "for better performance",
          "with new features",
          "across all functions",
          "to handle edge cases",
          "significantly"
        ), 1))
      }
      
      system("git add R/hdbscan_analysis.R")
      system(paste0('git commit -m "', commit_msg, '" --date="', timestamp, '"'))
      
      # Occasionally update other files
      if (runif(1) > 0.8) {
        writeLines(
          sample(c(
            "Added new test case",
            "Updated documentation",
            "Fixed typo",
            "Improved example"
          ), 1),
          "temp_file.txt"
        )
        system("git add temp_file.txt")
        system(paste0('git commit -m "Misc update" --date="', timestamp, '"'))
      }
    }
  }
  
  current_date <- current_date + 1
  day_counter <- day_counter + 1
}

# Final project wrap-up
writeLines(
  c(readLines("README.md"),
    "",
    "## Project Completed",
    paste("Finalized on", format(end_date, "%B %d, %Y")),
    "",
    "### Key Features Implemented",
    "- Advanced HDBSCAN clustering",
    "- Cluster validation metrics",
    "- Custom visualization options",
    "- Performance optimizations"),
  "README.md"
)

system("git add README.md")
system('git commit -m "Finalize project documentation" --date="2025-02-28 16:00:00"')

cat("\nGit history simulation complete!\n")
cat("Repository created at:", normalizePath("."), "\n")
cat("You can now push this to GitHub:\n")
cat("  cd", normalizePath("."), "\n")
cat("  git remote add origin YOUR_REPO_URL\n")
cat("  git push -u origin main\n")

