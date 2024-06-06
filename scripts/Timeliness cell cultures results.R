# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages =====
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
p_load(tidyverse, RODBC,gt, gtExtras, webshot, openxlsx)

#Give the path to the AFP database
path_AFP <- "../data/dbs/afp_wk21.mdb" 

# Connect to the Microsoft Access database =====
AFPdb <- DBI::dbConnect(odbc::odbc(), 
                        .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                              DBQ=", path_AFP))
# load data in R =====
# Retrieve all data from the AFP database
AFPtables <- DBI::dbGetQuery(AFPdb, "SELECT * FROM POLIOLAB ORDER BY LabName, EpidNumber;", stringsAsFactors = FALSE) |>
  tibble() |>
  # select samples collected in 2024 only
  filter(substr(ICLabID, start = 5, stop = 6) == 24 ) 

Specify_the_period <- paste0("WEEK 1 - ", 
                             (epiweek(as.Date(ymd_hms(AFPtables$DateUpdated))) - 1) |> unique(), ", 2024")

# Analysis of databases =====
AFPkpis <- 
  AFPtables |>
  #filter( AFPtables$LabName != "CDC" & year(AFPtables$DateOfOnset) > 2023 ) |>
  filter( AFPtables$LabName != "CDC" & !is.na(FinalCellCultureResult)) |>
  #distinct(ICLabID, .keep_all = "TRUE") |>
  group_by(LabName) |>
  summarise(samples_with_results = n()) |>
  ungroup() |>
  left_join (
    AFPtables |>
      # starts with 1
      filter(str_detect(FinalCellCultureResult, "^1")) |>
      #distinct(ICLabID, .keep_all = "TRUE") |>
      group_by(LabName) |>
      summarise(pv_positive = n()), 
    by = "LabName") |>
  ungroup() |>
  mutate(prop_pv_positive = 100 * pv_positive /samples_with_results ) |>
  left_join (
    AFPtables |>
      filter(str_detect(FinalCellCultureResult, "^4")) |>
      #distinct(ICLabID, .keep_all = "TRUE") |>
      group_by(LabName) |>
      summarise(pv_positive_and_npent = n()), 
    by = "LabName") |>
  ungroup() |>
  mutate(prop_pv_positive_and_npent = 100 * pv_positive_and_npent /samples_with_results ) |>
  left_join (
    AFPtables |>
      filter(str_detect(FinalCellCultureResult, "^3")) |>
      #distinct(ICLabID, .keep_all = "TRUE") |>
      group_by(LabName) |>
      summarise(npent = n()), 
    by = "LabName") |>
  ungroup() |>
  mutate(prop_npent = 100 * npent /samples_with_results ) |>
  left_join (
    AFPtables |>
      filter(str_detect(FinalCellCultureResult, "^2")) |>
      #distinct(ICLabID, .keep_all = "TRUE") |>
      group_by(LabName) |>
      summarise(negative = n()), 
    by = "LabName") |>
  ungroup() |>
  mutate(prop_negative = 100 * negative /samples_with_results ) |>
  left_join(
    AFPtables |>
      filter((is.na(AFPtables$FinalCellCultureResult) | is.nan(AFPtables$FinalCellCultureResult) | is.null(AFPtables$FinalCellCultureResult)) 
             & (as.Date(ymd_hms(AFPtables$DateUpdated)) - ymd(AFPtables$DateStoolReceivedinLab)) >= 15) |>
      #distinct(ICLabID, .keep_all = "TRUE") |>
      group_by(LabName) |>
      summarise(pending_culture_results = n()), 
    by = "LabName" ) |>
  ungroup() |>
  left_join(
    AFPtables |>
      filter((is.na(AFPtables$FinalCellCultureResult) | is.nan(AFPtables$FinalCellCultureResult) | is.null(AFPtables$FinalCellCultureResult)) 
             & ( as.Date(ymd_hms(AFPtables$DateUpdated)) - ymd(AFPtables$DateStoolReceivedinLab)) >= 15 & (as.Date(ymd_hms(AFPtables$DateUpdated)) - ymd(AFPtables$DateStoolReceivedinLab)) <= 30) |>
      #distinct(ICLabID, .keep_all = "TRUE") |>
      group_by(LabName) |>
      summarise(pending_culture_15_30 = n()), 
    by = "LabName" ) |>
  ungroup() |>
  left_join(
    AFPtables |>
      filter((is.na(AFPtables$FinalCellCultureResult) | is.nan(AFPtables$FinalCellCultureResult) | is.null(AFPtables$FinalCellCultureResult)) 
             & (as.Date(ymd_hms(AFPtables$DateUpdated)) - ymd(AFPtables$DateStoolReceivedinLab)) > 30 & (as.Date(ymd_hms(AFPtables$DateUpdated)) - ymd(AFPtables$DateStoolReceivedinLab)) <= 60) |>
      #distinct(ICLabID, .keep_all = "TRUE") |>
      group_by(LabName) |>
      summarise(pending_culture_30_60 = n()), 
    by = "LabName" ) |>
  ungroup() |>
  left_join(
    AFPtables |>
      filter((is.na(AFPtables$FinalCellCultureResult) | is.nan(AFPtables$FinalCellCultureResult) | is.null(AFPtables$FinalCellCultureResult)) 
             & (as.Date(ymd_hms(AFPtables$DateUpdated)) - ymd(AFPtables$DateStoolReceivedinLab)) > 60 ) |>
      #distinct(ICLabID, .keep_all = "TRUE") |>
      group_by(LabName) |>
      summarise(pending_culture_more_60 = n()), 
    by = "LabName" ) |>
  ungroup() |>