# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages =====
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
p_load(tidyverse, RODBC,gt, gtExtras, webshot, officer)

#Give the path to the AFP database
path_AFP <- "../data/dbs/AFP_Week52_2024.mdb" 

# Connect to the Microsoft Access database =====
AFPdb <- DBI::dbConnect(odbc::odbc(), 
                        .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                              DBQ=", path_AFP))
# load data in R =====
# Retrieve all data from the AFP database
AFPtables <- DBI::dbGetQuery(AFPdb, "SELECT * FROM POLIOLAB ORDER BY LabName, EpidNumber;", stringsAsFactors = FALSE) |>
  tibble() |>  mutate(
    proxy_date_infor_itd = coalesce(DateIsolateinforITD, DateLarmIsolateRec, DateRarmIsolateSentforITD),
    proxy_date_itd_result = coalesce(DateFinalrRTPCRResults, DateFinalResultsSentReflabEPI)
    
  ) |>
  # select samples collected in 2025 only
  #filter(substr(ICLabID, start = 5, stop = 6) == 25 )

Specify_the_period <- paste0("WEEK 1 - ", 
                             (epiweek(as.Date(ymd(AFPtables$DateUpdated))) - 1) |> unique(), ", 2025")


# Analysis of databases =====
AFPCountries_35p <- 
 AFPtables |>
  filter(LabName != "CDC", !is.na(DateOfOnset)) |>
  filter(substr(EpidNumber, start = 1, stop = 3) %in% c("DJI", "SOM") == F ) |> #remove somalia and djibouti
  mutate(
    CountryCode = substr(EpidNumber, start = 1, stop = 3), .before = LabName,
    CountryCode = if_else(LabName == "MAD", "MAD", CountryCode )) |>
  mutate(IST = case_when(CountryCode %in% c("ALG", "BEN", "BFA", "CIV", "GAM", "GHA", "GUB", "GUI", "LIB", "MAI", "MAU",
                                            "NIE", "NIG", "SEN", "SIL",  "TOG" ) ~ "WEST",
                         CountryCode %in% c( "ANG", "CAE", "CAF", "CHA",  "EQG", "GAB", "CNG", "RDC") ~ "CENTRAL",
                         CountryCode %in% c( "BOT", "BUU", "COM", "ETH", "KEN", "LES", "MAD", "MAL", "MOZ", "NAM", "RSS", "RWA",
                                              "SOA", "SWZ", "TAN", "UGA", "ZAM", "ZIM") ~ "ESA"), .before = CountryCode) |>
  filter( !(LabName == "SOA" & substr(ICLabID, start = 1, stop = 3) %in% c("CIV", "MAD", "RDC", "UGA", "ZAM", "ZIM")) ) |> # remove sequencing data
  select(IST, CountryCode, LabName, DateOfOnset, DateStoolReceivedinLab, StoolCondition, FinalCellCultureResult, DateFinalCellCultureResults,
         proxy_date_infor_itd, FinalITDResult, DateFinalrRTPCRResults, proxy_date_itd_result) |>
  mutate(FinalCellCultureResult = str_replace_all(FinalCellCultureResult, "Supected", "Suspected") ) |>
  #distinct(ICLabID, .keep_all = "TRUE") |>
  group_by(CountryCode) |>
  mutate(workload_by_lab = n(),
         time_itd_results_35days = as.numeric(difftime(proxy_date_itd_result, DateOfOnset, units = "days")),
         
         is_itd = if_else( (FinalCellCultureResult %in% c("1-Suspected Poliovirus", "4-Suspected Poliovirus + NPENT")), 1, 0),
         
         is_itd_35days = if_else( (FinalCellCultureResult %in% c("1-Suspected Poliovirus", "4-Suspected Poliovirus + NPENT")) &
                                    (!is.na(FinalITDResult) & time_itd_results_35days < 36 & time_itd_results_35days >= 0), 1, 0)
  ) |>
  summarize(
    ITD_results = sum(is_itd, na.rm = TRUE),
    ITD_results_35days = sum(is_itd_35days, na.rm = TRUE),
    Prop_ITD_35days = 100 * ITD_results_35days / ITD_results,
  ) |>
  filter(!is.na(Prop_ITD_35days) & Prop_ITD_35days > 0) |>
  summarize(median_Prop_ITD_35days = median(Prop_ITD_35days, na.rm = TRUE)) #to know the proportion 35 days by IST
  
AFPCountries_35p
