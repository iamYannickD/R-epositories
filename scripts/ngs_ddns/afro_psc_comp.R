#############################################################
# LINELIST RECONCILIATION – AFRO vs PSC
#############################################################

if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(tidyverse, readr, writexl, janitor, stringr)
datestamp <- format(Sys.Date(), "%Y%m%d")


# 1. IMPORT CSV FILES
LabName <- "DRC"   # <-- To change per piloting lab
afro <- read_csv(
  "../data/data_sequences/ddns/DRC - Cummulative DDNS Results as of January 31.csv",
  show_col_types = FALSE, na = c("", "NA", "NULL") ) #|> clean_names()

psc <- read_csv(
  "../data/data_sequences/ddns/DRC_imported_tables.csv",
  show_col_types = FALSE, na = c("", "NA", "NULL") ) #|> clean_names()


# 2. STANDARDIZATION
standardize_df <- function(df){
  
  required_cols <- c("RunNumber", "sample", "barcode")
  
  if(!all(required_cols %in% colnames(df))){
    stop("Missing required columns: RunNumber, sample, barcode")
  }
  
  df |>
    mutate(
      RunNumber = str_trim(as.character(RunNumber)),
      sample    = str_trim(as.character(sample)),
      barcode   = str_trim(as.character(barcode)),
      generic_key = paste(RunNumber, sample, barcode, sep = "_")
    )
}

afro <- standardize_df(afro)
psc  <- standardize_df(psc)

# LAYER 1 — DATA QUALITY CHECKS

# 1A. Duplicates in AFRO (RunQC == pass & sample repeated)

dup_afro_pass <-
  if("RunQC" %in% colnames(afro)){
    afro |>
      mutate(RunQC = str_to_lower(RunQC)) |>
      filter(RunQC == "pass") |>
      count(sample, name = "n") |>
      filter(n > 1)
  } else { tibble() }

# 1B. Barcode reuse within run (AFRO)

barcode_reuse_afro <-
  afro |>
  count(RunNumber, barcode, name = "n") |>
  filter(n > 1)

# LAYER 2 — RECORD MATCHING

# Matched records
matched <-
  inner_join(afro, psc, by = "generic_key", suffix = c("_afro", "_psc")  )

# PSC samples NOT reported to AFRO (CRITICAL GAP)
psc_not_in_afro <-
  anti_join(psc, afro, by = "generic_key")

# AFRO samples not in PSC
afro_not_in_psc <-
  anti_join(afro, psc, by = "generic_key")

# Quick Reporting Statistics

reporting_summary <-
  tibble(
    Metric = c("Total_AFRO", "Total_PSC", "Matched", "PSC_Not_Reported_to_AFRO", "AFRO_Not_in_PSC" ),
    Value = c(
      nrow(afro), nrow(psc), nrow(matched), nrow(psc_not_in_afro), nrow(afro_not_in_psc) )
  )

# Run-level Reporting (PSC → AFRO completeness)

run_level_summary <-
  psc |>
  count(RunNumber, name = "psc_total") |>
  left_join(
    matched |>
      mutate(RunNumber = RunNumber_psc) |>
      count(RunNumber, name = "reported_to_afro"),
    by = "RunNumber"
  ) |>
  replace_na(list(reported_to_afro = 0)) |>
  mutate(
    reporting_rate_pct =
      round(reported_to_afro / psc_total * 100, 2)
  )

# LAYER 3 — QC CONCORDANCE ANALYSIS
qc_cols <- c("RunQC", "SampleQC", "DDNSclassification")

qc_available <- qc_cols[
  qc_cols %in% colnames(afro) & qc_cols %in% colnames(psc)
]

if(length(qc_available) > 0){
  
  qc_compare <-
    matched |>
    select(
      generic_key,
      ends_with("_afro"),
      ends_with("_psc")
    )
  
  # Compare each QC variable
  for(col in qc_available){
    
    qc_compare[[paste0(col, "_match")]] <-
      str_to_lower(qc_compare[[paste0(col,"_afro")]]) ==
      str_to_lower(qc_compare[[paste0(col,"_psc")]])
  }
  
  # Agreement statistics
  agreement_rates <-
    qc_compare |>
    summarise(
      across(
        ends_with("_match"),
        ~ mean(.x, na.rm = TRUE)
      )
    ) |>
    pivot_longer(
      everything(),
      names_to = "QC_Metric",
      values_to = "AgreementRate"
    ) |>
    mutate(
      AgreementRate = round(AgreementRate, 4),
      DiscordanceRate = round(1 - AgreementRate, 4)
    )
  
  # Discordant DDNS (highest epidemiological importance)
  ddns_discordant <-
    qc_compare |>
    filter(DDNSclassification_match == FALSE)
  

  # Run-level QC Concordance
  run_qc_summary <-
    qc_compare |>
    mutate(RunNumber = matched$RunNumber_afro) |>
    group_by(RunNumber) |>
    summarise(
      across(
        ends_with("_match"),
        ~ round(mean(.x, na.rm = TRUE), 4)
      ),
      .groups = "drop"
    )
  
} else {
  
  qc_compare <- tibble()
  agreement_rates <- tibble()
  ddns_discordant <- tibble()
  run_qc_summary <- tibble()
}


# RUN-LEVEL SUMMARY FOR OUTPUT TAB
# run_level_detailed_summary <- 
#   tibble(RunNumber = unique(c(afro$RunNumber, psc$RunNumber))) |>
#   rowwise() |>
#   mutate(
#     Lab = LabName,
#     total_PSC = nrow(psc |> filter(RunNumber == cur_data()$RunNumber)),
#     total_AFRO = nrow(afro |> filter(RunNumber == cur_data()$RunNumber)),
#     RunQC_summary = ifelse(total_PSC == total_AFRO, "Pass", "Fail"),
#     SampleQC_summary = ifelse(
#       all(qc_compare |> filter(RunNumber == cur_data()$RunNumber) |> pull(SampleQC_match), na.rm = TRUE),
#       "Pass", "Fail"
#     ),
#     DDNSclassification_summary = ifelse(
#       all(qc_compare |> filter(RunNumber == cur_data()$RunNumber) |> pull(DDNSclassification_match), na.rm = TRUE),
#       "Pass", "Fail"
#     )
#   ) |>
#   ungroup() |>
#   select(Lab, RunNumber, total_PSC, total_AFRO, RunQC_summary, SampleQC_summary, DDNSclassification_summary)


# EXPORT
output_list <- list(
  Reporting_Summary = reporting_summary,
  Run_Level_Reporting = run_level_summary,
  #Run_Level_Detailed_Summary = run_level_detailed_summary,  # <-- New tab
  Duplicate_AFRO_RunQC_Pass = dup_afro_pass,
  Barcode_Reuse_AFRO = barcode_reuse_afro,
  Matched_Records = matched,
  PSC_Not_Reported_to_AFRO = psc_not_in_afro,
  AFRO_Not_in_PSC = afro_not_in_psc,
  QC_Comparison = qc_compare,
  QC_Agreement_Rates = agreement_rates,
  Run_Level_QC_Concordance = run_qc_summary,
  DDNS_Discordant = ddns_discordant
)

write_xlsx(
  output_list,
  paste0("../data/data_sequences/ddns/output/", LabName, "_summary_", datestamp, ".xlsx") )

# Check <-
#     qc_compare |> filter(DDNSclassification_match != T)

# write_csv(
#   Check,
#   paste0("../data/data_sequences/ddns/output/", LabName, "_summary1_", datestamp, ".csv") )
