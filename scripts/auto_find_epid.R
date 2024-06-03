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