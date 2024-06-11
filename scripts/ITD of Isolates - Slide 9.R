# Check if the package pacman is installed
# rm(list = ls())
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages =====
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
p_load(tidyverse, RODBC,gt, gtExtras, officer)

#Give the path to the AFP database
path_AFP <- "../data/dbs/afp_wk21.mdb" 

# Connect to the Microsoft Access database =====
AFPdb <- DBI::dbConnect(odbc::odbc(), 
                        .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                              DBQ=", path_AFP))
# load data in R =====
# Retrieve all data from the AFP database
AFPtables <- DBI::dbGetQuery(AFPdb, "SELECT * FROM POLIOLAB ORDER BY LabName, EpidNumber;", stringsAsFactors = FALSE) |>
  tibble() |>  mutate(proxy_date_infor_itd = coalesce(DateIsolateinforITD, DateLarmIsolateRec, 
                                                      DateRarmIsolateSentforITD, DateFinalCellCultureResults)
                      )   |>
  # select samples collected in 2024 only
  filter(substr(ICLabID, start = 5, stop = 6) == 24 )

Specify_the_period <- paste0("WEEK 1 - ", 
                             (epiweek(as.Date(ymd_hms(AFPtables$DateUpdated))) - 1) |> unique(), ", 2024")

# Analysis of databases =====
#AFPtables_gt <- 
#tempAFP2 <-
AFPtables |>
  filter(LabName != "CDC") |>
  select(LabName, DateStoolReceivedinLab, StoolCondition, FinalCellCultureResult, DateFinalCellCultureResults,
         proxy_date_infor_itd, FinalITDResult, DateFinalrRTPCRResults, DateUpdated) |>
  mutate( FinalCellCultureResult = str_replace_all(FinalCellCultureResult, "Supected", "Suspected") ) |>
  #distinct(ICLabID, .keep_all = "TRUE") |>
  group_by(LabName) |>
  mutate(is_itd = if_else( (FinalCellCultureResult == "1-Suspected Poliovirus" | FinalCellCultureResult == "4-Suspected Poliovirus + NPENT"), 1, 0),
         time_itd_results_7days = as.numeric(difftime(DateUpdated, proxy_date_infor_itd, units = "days")),
         ITD_results = sum(is_itd),
         is_itd_more_7days = if_else( 
           ( (FinalCellCultureResult == "1-Suspected Poliovirus" | FinalCellCultureResult == "4-Suspected Poliovirus + NPENT") &
               is.na(FinalITDResult) & time_itd_results_7days >= 8 ), 1, 0)
      ) |>
  mutate(virus_cat = 
          case_when(
            (is.na(FinalITDResult) | FinalITDResult == "9-Invalid") ~ NA,
            (FinalITDResult %in% c("16-nOPV2", "PV2+/nOPV2+") ) ~ "nOPV2",
            (FinalITDResult %in% c("15-PV2", "PV2+/nOPV2-", "12-PV2+/nOPV2-", "5-PV2 SL")) ~ "PV2",
            (FinalITDResult %in% c("4-PV1-SL", "4-PV1 SL")) ~ "Sabin Type 1",
            FinalITDResult == "6-PV3 SL" ~ "Sabin Type 3",
            FinalITDResult == "7-NPEV" ~ "NPEV",
            FinalITDResult == "8-NEV" ~ "NEV",
            FinalITDResult == "10-Mixture" ~ "Mixture",
            FinalITDResult == "12-PV1 SL Discordant" ~ "Mixture",
            FinalITDResult == "1-PV1 NSL" ~ "Type 1 Discordant",
            FinalITDResult == "3-PV3 NSL" ~ "Type 3 Discordant", 
            !is.na(FinalITDResult) ~ "check" # missed/unexpected results
        )) |>
  #filter(!is.na(virus_cat)) |>
  #count(virus_cat)
  summarize(
    ITD_results = sum(is_itd, na.rm = TRUE),
    ITD_pending_7days = sum(is_itd_more_7days, na.rm = TRUE),
    numb_viruses = n()
      ) #|>
  #dplyr::select(LabName, ITD_results, ITD_pending_7days)



