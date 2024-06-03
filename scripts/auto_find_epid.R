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