# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages =====
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
p_load(tidyverse, RODBC,gt, gtExtras, webshot, officer)

#Give the path to the AFP database
path_AFP <- "../data/dbs/afp_wk21.mdb" 

# Connect to the Microsoft Access database =====
AFPdb <- DBI::dbConnect(odbc::odbc(), 
                        .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                              DBQ=", path_AFP))
# load data in R =====
# Retrieve all data from the AFP database
AFPtables <- DBI::dbGetQuery(AFPdb, "SELECT * FROM POLIOLAB ORDER BY LabName, EpidNumber;", stringsAsFactors = FALSE) |>
  tibble() |>  mutate(proxy_date_infor_itd = if_else(is.na(DateIsolateinforITD),
                                                     if_else(is.na(DateLarmIsolateRec), DateRarmIsolateSentforITD, DateLarmIsolateRec),
                                                     DateIsolateinforITD
  )
  ) |>
  # select samples collected in 2024 only
  filter(substr(ICLabID, start = 5, stop = 6) == 24 )

Specify_the_period <- paste0("WEEK 1 - ", 
                             (epiweek(as.Date(ymd_hms(AFPtables$DateUpdated))) - 1) |> unique(), ", 2024")

# Analysis of databases =====
#AFPtables_gt <- 
AFPtables |>
  filter(LabName != "CDC") |>
  select(LabName, DateStoolReceivedinLab, StoolCondition, FinalCellCultureResult, DateFinalCellCultureResults,
         proxy_date_infor_itd, FinalITDResult, DateFinalrRTPCRResults) |>
  mutate( FinalCellCultureResult = str_replace_all(FinalCellCultureResult, "Supected", "Suspected") ) |>
  #distinct(ICLabID, .keep_all = "TRUE") |>
  mutate(StoolCondition = str_replace_all(StoolCondition, "1-Adéquat", "1-Good")) |>
  group_by(LabName) |>
  mutate(workload_by_lab = n(),
         time_itd_results_21days = as.numeric(difftime(DateFinalrRTPCRResults, DateStoolReceivedinLab, units = "days")),

         is_itd = if_else( (FinalCellCultureResult %in% c("1-Suspected Poliovirus", "4-Suspected Poliovirus + NPENT")), 1, 0),
         is_itd_positive = if_else( (FinalCellCultureResult %in% c("1-Suspected Poliovirus", "4-Suspected Poliovirus + NPENT") &
                                       (!FinalITDResult %in% c("7-NPEV", "8-NEV", "9-Invalid", "4-PV1-SL", "4-PV1 SL", "6-PV3 SL") )), 1, 0),
            
           
         is_itd_21days = if_else( (FinalCellCultureResult %in% c("1-Suspected Poliovirus", "4-Suspected Poliovirus + NPENT")) &
                                 (!is.na(FinalITDResult) & time_itd_results_21days < 22 & time_itd_results_21days >= 0), 1, 0),
         is_itd_21days_positive_sample = if_else( (FinalCellCultureResult %in% c("1-Suspected Poliovirus", "4-Suspected Poliovirus + NPENT")) &
                                                    # filter ITD results not in the list of results below
                                                     (!FinalITDResult %in% c("7-NPEV", "8-NEV", "9-Invalid", "4-PV1-SL", "4-PV1 SL", "6-PV3 SL") ) & 
                                                     (!is.na(FinalITDResult) & time_itd_results_21days < 22 & time_itd_results_21days >= 0), 1, 0),
         
         ITD_results = sum(is_itd, na.rm = TRUE),
         ITD_results_positive_sample = sum(is_itd_positive, na.rm = TRUE),
         ITD_results_21days = sum(is_itd_21days, na.rm = TRUE),
         ITD_results_21days_positive_sample = sum(is_itd_21days_positive_sample, na.rm = TRUE)
  ) |>
  summarize(
    Prop_ITD_21days = 100 * ITD_results_21days / ITD_results,
    Prop_ITD_21days_positive_sample = 100 * ITD_results_21days_positive_sample / ITD_results_positive_sample
       )

  #  #|>
  # dplyr::select(LabName, workload_by_lab, Prop_sample_good_cond, culture_results, culture_results_14days, 
  #               Prop_culture_results_14days, ITD_results, ITD_results_7days, Prop_ITD_7days, ITD_results_21days, Prop_ITD_21days) 
