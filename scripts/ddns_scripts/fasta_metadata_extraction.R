# =====================================================
# FASTA Metadata Extraction and Merge Script
# =====================================================

# Check if the package pacman is installed (pacman Install and load multiple desired packages at once)
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# libraries
p_load(tidyverse, openxlsx)

# INPUT:
input_folders <- c("../data/data_sequences/vp1-sequences/minion/", "PATH/TO/FOLDER2")

# Locate all FASTA files
fasta_files <- unlist(
  lapply(input_folders, function(dir) {
    list.files(dir, pattern = "\\.(fasta|fa)$", recursive = TRUE, full.names = TRUE) })
)

if (length(fasta_files) == 0) {
  stop("No FASTA files found in the provided folders.")
}

cat("Found", length(fasta_files), "FASTA files\n")

# ---- 3. FUNCTIONS ----

# Parse a single FASTA file
parse_fasta <- function(fasta_file) {
  lines <- readLines(fasta_file)
  
  header_idx <- grep("^>", lines)
  
  results_metadata <- list()
  results_fasta <- character()
  
  for (i in seq_along(header_idx)) {
    start <- header_idx[i]
    end <- if (i < length(header_idx)) header_idx[i + 1] - 1 else length(lines)
    
    header <- lines[start]
    sequence <- paste(lines[(start + 1):end], collapse = "")
    
    # Remove ">" and split metadata
    metadata_raw <- sub("^>", "", header)
    metadata_parts <- str_split(metadata_raw, "\\|")[[1]]
    
    # Sequence name = first element before first "|"
    sequence_name <- metadata_parts[1]
    
    # Metadata dataframe (variable length handled later)
    metadata_df <- as.data.frame(t(metadata_parts), stringsAsFactors = FALSE)
    metadata_df$source_fasta <- basename(fasta_file)
    metadata_df$sequence_name <- sequence_name
    
    results_metadata[[length(results_metadata) + 1]] <- metadata_df
    
    # Build merged FASTA entry
    results_fasta <- c(
      results_fasta,
      paste0(">", sequence_name),
      sequence
    )
  }
  
  list(
    metadata = results_metadata,
    fasta = results_fasta
  )
}

# 4. PROCESS ALL FASTA FILES
parsed <- map(fasta_files, parse_fasta)

# 5. METADATA TABLE
metadata_list <- purrr::map(parsed, "metadata") |> purrr::flatten() ##

metadata_df <- bind_rows(metadata_list)

#
# Identify metadata columns automatically
meta_cols <- setdiff(colnames(metadata_df), c("sequence_name", "source_fasta"))
#

# Rename metadata columns
num_meta_cols <- setdiff(colnames(metadata_df), c("sequence_name", "source_fasta")) #ncol(metadata_df) - 2

colnames(metadata_df)[colnames(metadata_df) %in% meta_cols] <- paste0("metadata_field_", seq_along(meta_cols))
#colnames(metadata_df)[1:num_meta_cols] <- paste0("metadata_field_", seq_len(num_meta_cols))

# Reorder columns
metadata_df <- metadata_df |> relocate(sequence_name, source_fasta)
  #select(sequence_name, source_fasta, everything())

# 6. MERGED FASTA
merged_fasta <- unlist(map(parsed, "fasta"))

# 7. OUTPUT FILES
today <- format(Sys.Date(), "%Y-%m-%d")

metadata_output <- paste0("../data/data_sequences/vp1-sequences/output_minion/FASTA_Metadata_", today, ".xlsx")
fasta_output <- paste0("../data/data_sequences/vp1-sequences/output_minion/Merged_Sequences_", today, ".fasta")

write.xlsx(metadata_df, metadata_output)
writeLines(merged_fasta, fasta_output)

cat(" Outputs generated successfully:\n")
cat(" - Excel metadata:", metadata_output, "\n")
cat(" - Merged FASTA :", fasta_output, "\n")

