# Set the folder containing the txt files
folder_path <- "../data/data_seq/Raw Sequences/PV1/"

# Output file
output_file <- "../data/data_seq/Outputs/all_PV1_sequences.fasta"

# List all .fas files
files <- list.files(path = "../data/data_seq/Raw Sequences/PV1/" , pattern = "\\.fas$", full.names = TRUE)

# Initialize a character vector to store sequences
all_sequences <- character()

for (f in files) {
  # Read the file
  lines <- readLines(f)
  
  # Remove empty lines
  lines <- lines[nchar(lines) > 0]
  
  # Identify headers (lines starting with ">")
  header_idx <- grep("^>", lines)
  
  # Loop over each sequence in the file (in case multiple sequences per file)
  for (i in seq_along(header_idx)) {
    header <- lines[header_idx[i]]
    
    # Determine sequence lines for this header
    if (i < length(header_idx)) {
      seq_lines <- lines[(header_idx[i] + 1):(header_idx[i+1] - 1)]
    } else {
      seq_lines <- lines[(header_idx[i] + 1):length(lines)]
    }
    
    # Collapse sequence lines into a single string
    sequence <- paste(seq_lines, collapse = "")
    
    # Append header and sequence to all_sequences
    all_sequences <- c(all_sequences, as.character(header), as.character(sequence))
  }
}

# Write the combined FASTA file
writeLines(all_sequences, output_file)

cat("FASTA file created:", output_file, "\n")
