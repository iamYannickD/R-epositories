# Check if the package pacman is installed (pacman Install and load multiple desired packages at once)
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

p_load(tidyverse, readxl)

# Function to search for IDs in all sheets of an Excel file
search_ids_in_excel <- 
  function(ids, excel_file) {
    # Get the names of all sheets in the Excel file
    sheet_names <- excel_sheets(excel_file)
    
    # Create an empty dataframe to store the results
    results <- data.frame(ID = ids, Found = 0, Sheet = NA, stringsAsFactors = FALSE)
    # Loop over each sheet
    for (sheet in sheet_names) {
      # Read the current sheet
      sheet_data <- read_excel(excel_file, sheet = sheet)
      
      # Loop over each ID
      for (i in seq_along(ids)) {
        # Check if the ID is in the current sheet
        if (any(ids[i] %in% unlist(sheet_data))) {
          results$Found[i] <- 1
          results$Sheet[i] <- sheet
        }
      }
    }
    return(results)
  }

# Example usage
# Define your list of IDs
ids <- c("a", "b", "...", "z")

# Specify the path to your Excel file
excel_file <- "data/data_matching/Resolved_01.xlsx"

excel_file2 <- "data/data_matching/AFP_Contact_02.xlsx"

# Call the function and get the results
results1 <- search_ids_in_excel(ids, excel_file) |> as.tibble() 

results2 <- search_ids_in_excel(ids, excel_file2) |> as.tibble() 

results
