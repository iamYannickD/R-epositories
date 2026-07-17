# =====================================================
# FASTA Metadata Extraction and Quality Analysis
# =====================================================

if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, openxlsx, Biostrings)

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

# FUNCTION: Parse FASTA (CORE FIX)

parse_fasta <- function(fasta_file) {
  
  lines <- readLines(fasta_file)
  header_idx <- grep("^>", lines)
  
  all_sequences <- list()
  
  for(i in seq_along(header_idx)) {
    
    start <- header_idx[i]
    end <- if(i < length(header_idx)) header_idx[i+1] - 1 else length(lines)
    
    header <- lines[start]
    sequence <- paste(lines[(start+1):end], collapse = "")
    
    # Clean header
    header_clean <- sub("^>", "", header)
    
    # Split metadata (assuming | separator)
    metadata_parts <- str_split(header_clean, "\\|")[[1]]
    
    # Create base dataframe (1 row = 1 sequence)
    df <- tibble(
      sequence_name = metadata_parts[1],
      sequence = sequence,
      source_fasta = basename(fasta_file)
    )
    
    # Dynamically store ALL metadata fields
    for(j in seq_along(metadata_parts)) {
      df[[paste0("meta_", j)]] <- metadata_parts[j]
    }
    
    all_sequences[[i]] <- df
  }
  
  bind_rows(all_sequences)
}

# PROCESS ALL FILES (SIMPLIFIED & FIXED)

# Directly create clean dataset
sequence_df <- map_dfr(fasta_files, parse_fasta)

cat("Total sequences parsed:", nrow(sequence_df), "\n")

# QUALITY ANALYSIS

# Duplicate sequence names
dup_sequence_names <- sequence_df |>
  count(sequence_name, name = "n") |>
  filter(n > 1)

# Identical sequences (same nucleotide string)
identical_sequences <- sequence_df |>
  group_by(sequence) |>
  filter(n() > 1) |>
  summarise(
    sequence_names = paste(sequence_name, collapse = "; "),
    source_fasta = paste(unique(source_fasta), collapse = "; "),
    count = n(),
    .groups = "drop"
  ) |>
  arrange(desc(count))

# Metadata quality summary
metadata_quality <- tibble(
  Total_Records = nrow(sequence_df),
  Missing_Metadata = sum(!complete.cases(sequence_df)),
  Unique_Sequences = n_distinct(sequence_df$sequence_name),
  Duplicates = nrow(dup_sequence_names)
)

# Sequence statistics
sequence_stats <- sequence_df |>
  mutate(
    seq_length = nchar(sequence),
    N_count = str_count(sequence, "[Nn]"),
    N_pct = round((N_count / seq_length) * 100, 2)
  )

# MERGED FASTA (CLEAN REBUILD)

# rebuild FASTA cleanly from parsed data
merged_fasta <- sequence_df |>
  mutate(fasta_format = paste0(">", sequence_name, "\n", sequence)) |>
  pull(fasta_format)

# OUTPUT

today <- format(Sys.Date(), "%Y%m%d")

output_excel <- paste0("../data/data_sequences/vp1-sequences/output_ddns/FASTA_QC_Summary_", today, ".xlsx")
merged_fasta_file <- paste0("../data/data_sequences/vp1-sequences/output_ddns/Merged_Sequences_", today, ".fasta")

# Write Excel with multiple sheets
write.xlsx(
  list(
    All_Sequences = sequence_df,
    Duplicate_Sequence_Names = dup_sequence_names,
    Identical_Sequences = identical_sequences,
    Metadata_Quality = metadata_quality,
    Sequence_Statistics = sequence_stats
  ),
  output_excel
)

# Write merged FASTA
writeLines(merged_fasta, merged_fasta_file)

cat("Outputs generated successfully:\n")
cat(" - Excel file:", output_excel, "\n")
cat(" - Merged FASTA:", merged_fasta_file, "\n")

# OPTIONAL: QUICK CHECKS (VERY USEFUL)

# Distribution of sequence lengths
print(summary(sequence_stats$seq_length))

# Check high N content sequences (>5%)
high_N_sequences <- sequence_stats |> filter(N_pct > 5)

cat("Sequences with >5% Ns:", nrow(high_N_sequences), "\n")