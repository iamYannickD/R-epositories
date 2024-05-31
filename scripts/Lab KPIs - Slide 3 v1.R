#check directory =====
getwd()

# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages =====
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
p_load(tidyverse, RODBC,gt, gtExtras, webshot, officer)
#to convert the web image into an image
webshot::install_phantomjs()

#Give the path to the AFP database
path_AFP <- "data/afp_wk18.mdb" 

# Connect to the Microsoft Access database =====
AFPdb <- DBI::dbConnect(odbc::odbc(), 
                        .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                              DBQ=", path_AFP))
# load data in R =====
# Retrieve all data from the AFP database
AFPtables <- DBI::dbGetQuery(AFPdb, "SELECT * FROM POLIOLAB ORDER BY LabName, EpidNumber;", stringsAsFactors = FALSE) |>
  tibble() |>  mutate(proxy_date_infor_itd = if_else(is.na(DateIsolateinforITD), DateLarmIsolateRec, DateIsolateinforITD)
                ) |>
  # select samples collected in 2024 only
  filter(substr(ICLabID, start = 5, stop = 6) == 24 )

Specify_the_period <- paste0("WEEK 1 - ", 
                             (epiweek(as.Date(ymd_hms(AFPtables$DateUpdated))) - 5) |> unique(), ", 2024")

# Analysis of databases =====
AFPtables_gt <- 
  AFPtables |>
  #filter( AFPtables$LabName != "CDC" & year(AFPtables$DateOfOnset) > 2023 ) |>
  filter( AFPtables$LabName != "CDC") |>
  #distinct(ICLabID, .keep_all = "TRUE") |>
  group_by(LabName) |>
  summarise(workload_by_lab = n()) |>
  ungroup() |>
  # samples arrived in good conditions in the lab
  left_join (
      AFPtables |>
        #filter(AFPtables$StoolCondition %in% c("1-Good", "1-Adéquat", "2-Bad", "2-Inadéquat") | is.na(AFPtables$StoolCondition)) |>
        filter(AFPtables$StoolCondition %in% c("1-Good", "1-Adéquat")) |>
        #distinct(ICLabID, .keep_all = "TRUE") |>
        group_by(LabName) |>
        summarise(Sample_good_cond = n()), 
      by = "LabName") |>
      ungroup() |>
  mutate(Prop_sample_good_cond = round( Sample_good_cond / workload_by_lab * 100, 0) ) |>
  # total cell culture results =====
  left_join(
    AFPtables |>
      filter(!is.na(AFPtables$FinalCellCultureResult) & !is.nan(AFPtables$FinalCellCultureResult) & !is.null(AFPtables$FinalCellCultureResult)) |>
      #distinct(ICLabID, .keep_all = "TRUE") |>
      group_by(LabName) |>
      summarise(culture_results = n()), 
    by = "LabName" ) |>
  # Cell culture results in less than 14 days
  left_join(
    AFPtables |>
      filter(!is.na(AFPtables$FinalCellCultureResult) & !is.nan(AFPtables$FinalCellCultureResult) & !is.null(AFPtables$FinalCellCultureResult) &
               (AFPtables$DateFinalCellCultureResults - AFPtables$DateStoolReceivedinLab) < 15 & 
               (AFPtables$DateFinalCellCultureResults - AFPtables$DateStoolReceivedinLab) >= 0 ) |>
      #distinct(ICLabID, .keep_all = "TRUE") |>
      group_by(LabName) |>
      summarise(culture_results_14days = n()), 
    by = "LabName" ) |>
  mutate(Prop_culture_results_14days = round(culture_results_14days / culture_results * 100, 0) ) |>
  # all ITD results
  left_join(
    AFPtables |>
      filter((str_detect(AFPtables$FinalCellCultureResult, "^1") | str_detect(AFPtables$FinalCellCultureResult, "^4"))
             & !is.nan(AFPtables$FinalITDResult) & !is.null(AFPtables$FinalITDResult)) |>
      #distinct(ICLabID, .keep_all = "TRUE") |>
      group_by(LabName) |>
      summarise(ITD_results = n()), 
    by = "LabName" ) |>
  # ITD results in less than 7 days from reception in lab ====
  left_join(
    AFPtables |>
      filter(((str_detect(AFPtables$FinalCellCultureResult, "^1") | str_detect(AFPtables$FinalCellCultureResult, "^4"))
             & !is.nan(AFPtables$FinalITDResult) & !is.null(AFPtables$FinalITDResult)) 
            # & between((AFPtables$DateFinalrRTPCRResults - AFPtables$DateIsolateinforITD), ymd_hms(0), ymd_hms(700000)) ) |>
            & (AFPtables$DateFinalrRTPCRResults - AFPtables$DateIsolateinforITD) >= 0
            & (as.Date(AFPtables$DateFinalrRTPCRResults) - as.Date(AFPtables$proxy_date_infor_itd)) < 8) 









