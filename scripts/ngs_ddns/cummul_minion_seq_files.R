# ================================
# Append All Run Reports into One File (CSV only, robust to data type differences and encoding issues)
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
input_folder <- "../data/data_ngs/drc_minion/"

# ---- 2. List all CSV files ----
files <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)

if (length(files) == 0) {
  stop("No CSV files found in the specified folder.")
}

cat("Found", length(files), "CSV files:\n")
print(basename(files))

# ---- 3. Read and Merge Files ----
# Function to read each CSV with all columns as character and handle encoding
read_run_report <- function(f) {
  tryCatch({
    cat("Reading file:", basename(f), "\n")
    df <- vroom::vroom(
      f,
      delim = ",",
      col_types = cols(.default = "c"),  # Force all columns to character
      trim_ws = TRUE,                    # Trim white spaces
      locale = locale(encoding = "UTF-8") # Specify UTF-8 encoding
    ) %>%
      janitor::clean_names()             # Standardize column names
    
    df$source_file <- basename(f)        # Track source file
    
    # Clean any invalid UTF-8 characters
    df <- df %>%
      mutate(across(everything(), ~ iconv(.x, to = "UTF-8", sub = "byte")))
    
    return(df)
  }, error = function(e) {
    cat("Error reading", basename(f), ":", e$message, "\n")
    # Try reading with different encoding if UTF-8 fails
    tryCatch({
      cat("Trying with Latin-1 encoding for", basename(f), "\n")
      df <- vroom::vroom(
        f,
        delim = ",",
        col_types = cols(.default = "c"),
        trim_ws = TRUE,
        locale = locale(encoding = "Latin-1")
      ) %>%
        janitor::clean_names()
      
      df$source_file <- basename(f)
      
      # Convert to UTF-8 and clean
      df <- df %>%
        mutate(across(everything(), ~ iconv(.x, from = "Latin-1", to = "UTF-8", sub = "byte")))
      
      return(df)
    }, error = function(e2) {
      cat("Failed to read", basename(f), "with both UTF-8 and Latin-1 encodings\n")
      return(NULL)
    })
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

# Final cleaning of UTF-8 characters
cumulative <- cumulative %>%
  mutate(across(everything(), ~ stringi::stri_enc_toutf8(.x, validate = TRUE)))

cat("Successfully merged", length(all_data), "files with",
    nrow(cumulative), "rows and", ncol(cumulative), "columns.\n")

# ---- 4. Save Output ----
today <- format(Sys.Date(), "%Y-%m-%d")

# Try saving as Excel first
tryCatch({
  output_name_xlsx <- paste0("Cumulative_Runs_as_of_", today, ".xlsx")
  write.xlsx(cumulative, file = output_name_xlsx)
  cat("✅ Cumulative run report saved as:", output_name_xlsx, "\n")
}, error = function(e) {
  cat("⚠️  Excel export failed:", e$message, "\n")
  cat("Saving as CSV instead...\n")
  
  # Fallback: save as CSV
  output_name_csv <- paste0("Cumulative_Runs_as_of_", today, ".csv")
  vroom::vroom_write(cumulative, file = output_name_csv, delim = ",")
  cat("✅ Cumulative run report saved as:", output_name_csv, "\n")
})

# ---- 5. Optional Summary ----
cat("\nSummary of Final File:\n")
cat("- Total rows:", nrow(cumulative), "\n")
cat("- Total columns:", ncol(cumulative), "\n")
cat("- Column names:\n", paste(names(cumulative), collapse = ", "), "\n")

# Check for potential encoding issues
cat("\nChecking for potential encoding issues...\n")
problematic_cols <- c()
for (col in names(cumulative)) {
  if (any(grepl("[^\x01-\x7F]", cumulative[[col]], na.rm = TRUE))) {
    problematic_cols <- c(problematic_cols, col)
  }
}

if (length(problematic_cols) > 0) {
  cat("⚠️  Columns with non-ASCII characters:", paste(problematic_cols, collapse = ", "), "\n")
  cat("   These columns may contain special characters that could cause issues.\n")
} else {
  cat("✅ No obvious encoding issues detected.\n")
}