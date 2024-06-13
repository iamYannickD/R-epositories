# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages =====
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
p_load(tidyverse, RODBC,gt, gtExtras)

#Give the path to the ES database
Specify_the_period <- "WEEK 1 - 21, 2024"
path_ES_2024 = "../data/dbs/es_2024.mdb"

# Connect to the Microsoft Access database ====
ESdb2024 <- DBI::dbConnect(odbc::odbc(), 
                           .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                              DBQ=", path_ES_2024))
# load data in R =====
# Retrieve all data from the AFP database
EStables2024 <- DBI::dbGetQuery(ESdb2024, "SELECT * FROM Environmental ORDER BY IDNumber;", stringsAsFactors = FALSE) |>
  as_tibble() |>
  mutate(Labname = str_replace_all(Labname, c("ENTEBBE" = "UGA", "GHANA" = "GHA", "INRB" = "RDC", "IPD SEN" = "SEN",
                                              "IPM, MAD" = "MAD", "IPM,MAD" = "MAD", "KEMRI" = "KEN", "IBD, Nigeria" = "IBD",
                                              "MDG, Nigeria" = "MDG", "ZAM UTH" = "ZAM", "ZAM-UTH" = "ZAM")),
         date_result_to_lab = if_else(is.na(Dateresultstolab), 
                                      Datefinalcultureresult, Dateresultstolab)
  )

#Specify_the_period <- paste0("WEEK 1 - ", (epiweek(as.Date(ymd_hms(AFPtables$DateUpdated))) - 1) |> unique(), ", 2024")

# Analysis of databases =====

EStables2024 |>
  group_by(Labname) |>
  mutate(Labname = str_replace_all(Labname, "ESWATINI", "SOA" )
  ) |>
  summarize(
    workload_by_lab = n(),
    
    ITD_results = sum(str_detect(Finalcellcultureresult, "^1") | str_detect(Finalcellcultureresult, "^4"), na.rm = TRUE),
    ITD_results_positive = sum(
            (str_detect(Finalcellcultureresult, "^1") | str_detect(Finalcellcultureresult, "^4")) &
              (str_detect(FinalcombinedrRTPCRresults, "Discordant") | str_detect(FinalcombinedrRTPCRresults, "PV2")), na.rm = TRUE),
    
    
    ITD_results_21days = sum( (str_detect(Finalcellcultureresult, "^1") | str_detect(Finalcellcultureresult, "^4")) & 
                                !is.na(FinalcombinedrRTPCRresults) & (DateFinalCombinedResult - Datesampleinlab) < 22 & 
                                (DateFinalCombinedResult - Datesampleinlab) >= 0, na.rm = TRUE),
    ITD_results_21days_positive = sum( (str_detect(Finalcellcultureresult, "^1") | str_detect(Finalcellcultureresult, "^4")) & 
                                   (str_detect(FinalcombinedrRTPCRresults, "Discordant") | str_detect(FinalcombinedrRTPCRresults, "PV2")) &
                                !is.na(FinalcombinedrRTPCRresults) & (DateFinalCombinedResult - Datesampleinlab) < 22 & 
                                (DateFinalCombinedResult - Datesampleinlab) >= 0, na.rm = TRUE),
    Prop_ITD_21days = round(ITD_results_21days / ITD_results * 100, 0),
    Prop_ITD_21days_positive = round(ITD_results_21days_positive / ITD_results_positive * 100, 0)
    ) |>
  select(Labname, ITD_results, ITD_results_21days, ITD_results_21days_positive, Prop_ITD_21days, Prop_ITD_21days_positive)
