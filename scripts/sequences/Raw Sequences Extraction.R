# Load required libraries
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)

# Install and load required libraries
p_load(pdftools, openxlsx, tidyverse)

# Function to extract sequencing results from a PDF
extract_sequences <- function(pdf_path) {
  # Read the PDF text
  pdf_text_data <- pdf_text(pdf_path)
  
  # Convert PDF text into a vector of lines
  lines <- unlist(strsplit(pdf_text_data, "\n"))
  
  # Initialize empty data frames for structured and unstructured data
  structured_data <- tibble(
    Source_PDF = character(),
    Sequence_Name = character(), 
    Epid_Number = character(), 
    Location = character(), 
    Collection_Date = character(), 
    Nt_Changes = character(), 
    Emergence_Group = character()
  )
  
  unclassified_data <- tibble(
    Source_PDF = character(),
    Full_Line = character()
  )
  
  # Extract the file name (without the path)
  pdf_name <- basename(pdf_path)
  
  # Process each line
  for (line in lines) {
    fields <- unlist(strsplit(line, "\\s+"))  # Split by spaces
    
    if (length(fields) == 6) { 
      # Append to structured_data
      structured_data <- bind_rows(structured_data, tibble(
        Source_PDF = pdf_name,
        Sequence_Name = fields[1],
        Epid_Number = fields[2],
        Location = fields[3],
        Collection_Date = fields[4],
        Nt_Changes = fields[5],
        Emergence_Group = fields[6]
      ))
      
    } else if (length(fields) > 6) {
      # Append to unclassified_data
      unclassified_data <- bind_rows(unclassified_data, tibble(
        Source_PDF = pdf_name,
        Full_Line = paste(fields, collapse = " ")  # Store full line as a single string
      ))
    }
  }
  
  # Return extracted data
  return(list(
    structured = if (nrow(structured_data) > 0) structured_data else NULL, 
    unclassified = if (nrow(unclassified_data) > 0) unclassified_data else NULL
  ))
}

# Define the folder where PDFs are located
pdf_folder <- "../data/data_sequences/emergences/"  # Update with the correct path
pdf_files <- list.files(path = pdf_folder, pattern = "*.pdf", full.names = TRUE)

# Check if PDFs were found
if (length(pdf_files) == 0) {
  stop("No PDF files found in the specified folder!")
}

# Create an Excel file
excel_filename <- "../data/data_sequences/emergences/Extracted_Sequences.xlsx"
wb <- createWorkbook()

all_structured_data <- tibble(
  Source_PDF = character(),
  Sequence_Name = character(), 
  Epid_Number = character(), 
  Location = character(), 
  Collection_Date = character(), 
  Nt_Changes = character(), 
  Emergence_Group = character()
)

all_unclassified_data <- tibble(
  Source_PDF = character(),
  Full_Line = character()
)

# Process each PDF file
for (pdf in pdf_files) {
  # Extract data from the current PDF
  extracted_data <- extract_sequences(pdf)
  
  # Store structured data
  if (!is.null(extracted_data$structured)) {
    all_structured_data <- bind_rows(all_structured_data, extracted_data$structured)
  }
  
  # Store unclassified data
  if (!is.null(extracted_data$unclassified)) {
    all_unclassified_data <- bind_rows(all_unclassified_data, extracted_data$unclassified)
  }
}

# Add structured data to Excel
if (nrow(all_structured_data) > 0) {
  addWorksheet(wb, "Structured_Sequences")
  writeData(wb, "Structured_Sequences", all_structured_data)
}

# Add unclassified data to Excel
if (nrow(all_unclassified_data) > 0) {
  addWorksheet(wb, "Unclassified")
  writeData(wb, "Unclassified", all_unclassified_data)
}

# Save the Excel file
saveWorkbook(wb, excel_filename, overwrite = TRUE)

print(paste("Excel file", excel_filename, "has been created successfully!"))
