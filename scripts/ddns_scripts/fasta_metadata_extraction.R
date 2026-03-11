# =====================================================
# FASTA Metadata Extraction and Quality Analysis
# =====================================================

if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, openxlsx, Biostrings)  # Biostrings helps with sequence analysis

# INPUT
input_folders <- c("../data/data_sequences/vp1-sequences/ddns/", "PATH/TO/FOLDER2")

# Locate all FASTA files
fasta_files <- unlist(
  lapply(input_folders, function(dir) {
    list.files(dir, pattern = "\\.(fasta|fa)$", recursive = TRUE, full.names = TRUE)
  })
)

if(length(fasta_files) == 0) stop("No FASTA files found in the provided folders.")

cat("Found", length(fasta_files), "FASTA files\n")

# FUNCTIONS

# Parse a single FASTA file
parse_fasta <- function(fasta_file) {
  lines <- readLines(fasta_file)
  header_idx <- grep("^>", lines)
  
  results_metadata <- list()
  results_fasta <- character()
  
  for(i in seq_along(header_idx)) {
    start <- header_idx[i]
    end <- if(i < length(header_idx)) header_idx[i+1] - 1 else length(lines)
    
    header <- lines[start]
    sequence <- paste(lines[(start+1):end], collapse = "")
    
    metadata_raw <- sub("^>", "", header)
    metadata_parts <- str_split(metadata_raw, "\\|")[[1]]
    
    sequence_name <- metadata_parts[1]
    
    metadata_df <- as.data.frame(t(metadata_parts), stringsAsFactors = FALSE)
    metadata_df$source_fasta <- basename(fasta_file)
    metadata_df$sequence_name <- sequence_name
    
    results_metadata[[length(results_metadata) + 1]] <- metadata_df
    results_fasta <- c(results_fasta, paste0(">", sequence_name), sequence)
  }
  
  list(
    metadata = results_metadata,
    fasta = results_fasta
  )
}

# PROCESS ALL FASTA FILES

parsed <- map(fasta_files, parse_fasta)
metadata_list <- map(parsed, "metadata") |> flatten()
metadata_df <- bind_rows(metadata_list)

# Rename metadata columns dynamically
meta_cols <- setdiff(colnames(metadata_df), c("sequence_name", "source_fasta"))
colnames(metadata_df)[colnames(metadata_df) %in% meta_cols] <- paste0("metadata_field_", seq_along(meta_cols))
metadata_df <- metadata_df |> relocate(sequence_name, source_fasta)

# Merge all sequences
merged_fasta <- unlist(map(parsed, "fasta"))

# QUALITY ANALYSIS

# 1. Duplicate sequence names
dup_sequence_names <- metadata_df |>
  count(sequence_name, name = "n") |>
  filter(n > 1)

# 2. Extract sequences into dataframe
sequence_df <- tibble(
  sequence_name = character(),
  sequence = character(),
  source_fasta = character()
)

for(p in parsed) {
  fasta_entries <- p$fasta
  headers <- fasta_entries[seq(1, length(fasta_entries), by = 2)]
  seqs <- fasta_entries[seq(2, length(fasta_entries), by = 2)]
  
  # Add source file
  src <- rep(p$metadata[[1]]$source_fasta, length(headers))
  all_seq_annotations <- rep(p$metadata[[1]]$V6, length(headers))  #extract all metadata
  
  sequence_df <- bind_rows(sequence_df,
                           tibble(
                             sequence_name = str_remove(headers, "^>"),
                             sequence = seqs,
                             source_fasta = src,
                             seq_meta = all_seq_annotations
                           ))
}

# 3. Identical sequences (group sequences, list all sources)
identical_sequences <- sequence_df |>
  group_by(sequence) |>
  filter(n() > 1) |>
  summarise(
    sequence_names = paste(sequence_name, collapse = "; "),
    source_fasta = paste(unique(source_fasta), collapse = "; "),
    sequence = first(sequence),
    .groups = "drop"
  ) |>
  arrange(sequence)

# 4. Metadata quality checks
metadata_quality <- tibble(
  Total_Records = nrow(metadata_df),
  Missing_Metadata_Fields = sum(!complete.cases(metadata_df)),
  Unique_Sequence_Names = n_distinct(metadata_df$sequence_name),
  Duplicated_Sequence_Names = nrow(dup_sequence_names)
)

# 5. Sequence statistics (length, GC content, N content)
sequence_stats <- sequence_df |>
  mutate(
    seq_length = nchar(sequence),
    #GC_content = round((str_count(sequence, "[GCgc]") / seq_length) * 100, 2),
    N_count = str_count(sequence, "[Nn]"),
    N_pct = round((N_count / seq_length) * 100, 2)
  )

# OUTPUT

today <- format(Sys.Date(), "%Y%m%d")
output_excel <- paste0("../data/data_sequences/vp1-sequences/output_minion/FASTA_QC_Summary_", today, ".xlsx")
merged_fasta_file <- paste0("../data/data_sequences/vp1-sequences/output_minion/Merged_Sequences_", today, ".fasta")

# Write Excel with multiple tabs
write.xlsx(
  list(
    Metadata = metadata_df,
    Duplicate_Sequence_Names = dup_sequence_names,
    Identical_Sequences = identical_sequences,
    Metadata_Quality_Summary = metadata_quality,
    Sequence_Statistics = sequence_stats
  ),
  output_excel
)

# Write merged fasta
writeLines(merged_fasta, merged_fasta_file)

cat("Outputs generated successfully:\n")
cat(" - Excel metadata + QC summary:", output_excel, "\n")
cat(" - Merged FASTA:", merged_fasta_file, "\n")
