# ================================
# Append All Run Reports into One File (CSV only, robust to data type differences)
# ================================

# Install packages if needed (run once)
# install.packages(c("vroom", "dplyr", "purrr", "janitor", "openxlsx"))

library(vroom)      # Fast CSV reader, can force all columns to character
library(dplyr)      # Data manipulation
library(purrr)      # Functional programming (map)
library(janitor)    # Clean column names
library(openxlsx)   # Write Excel output

# ---- 1. Set Folder Path ----
# Change this to the folder containing all CSV run reports
input_folder <- "../data/data_ngs/drc_ddns/"   # <-- EDIT THIS if needed

# ---- 2. List all CSV files ----
files <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)

if (length(files) == 0) {
  stop("No CSV files found in the specified folder.")
}

cat("Found", length(files), "CSV files:\n")
print(basename(files))

# ---- 3. Read and Merge Files ----
# Function to read each CSV with all columns as character
read_run_report <- function(f) {
  tryCatch({
    cat("Reading file:", basename(f), "\n")
    df <- vroom::vroom(
      f,
      delim = ",",
      col_types = cols(.default = "c"),  # Force all columns to character
      trim_ws = TRUE                     # Trim white spaces
    ) %>%
      janitor::clean_names()             # Standardize column names
    df$source_file <- basename(f)        # Track source file
    return(df)
  }, error = function(e) {
    cat("Error reading", basename(f), ":", e$message, "\n")
    return(NULL)
  })
}

# Read all files into a list
all_data <- map(files, read_run_report)

# Remove NULL entries (failed reads)
all_data <- all_data[!sapply(all_data, is.null)]

if (length(all_data) == 0) {
  stop("No files could be read successfully.")
}

# Bind rows while keeping all columns
# Since all columns are character, no type conflicts will occur
cumulative <- bind_rows(all_data)

cat("Successfully merged", length(all_data), "files with",
    nrow(cumulative), "rows and", ncol(cumulative), "columns.\n")

# ---- 4. Save Output ----
today <- format(Sys.Date(), "%Y-%m-%d")
output_name <- paste0("Cumulative_Runs_as_of_", today, ".xlsx")

write.xlsx(cumulative, file = output_name)
cat("Cumulative run report saved as:", output_name, "\n")

# ---- 5. Optional Summary ----
cat("\nSummary of Final File:\n")
cat("- Total rows:", nrow(cumulative), "\n")
cat("- Total columns:", ncol(cumulative), "\n")
cat("- Column names:\n", paste(names(cumulative), collapse = ", "), "\n")
