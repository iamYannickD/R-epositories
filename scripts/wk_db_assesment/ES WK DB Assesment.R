# Check if the package pacman  is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages =====
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
p_load(tidyverse, RODBC)

#Give the path to the ES database
Specify_the_period <- "WEEK 1 - 8, 2025"
path_previous_ES_DB <- "../data/dbs/ES_WK07.mdb"
path_current_ES_DB <- "../data/dbs/ES_WK08.mdb" 
labname <- c("CAE", "CIV", "ETH") # Replace with the actual labname(s) you want to filter by

# Connect to the Microsoft Access database ====
Previous_ESdb <- DBI::dbConnect(odbc::odbc(), 
                                .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=", path_previous_ES_DB))

Current_ESdb <- DBI::dbConnect(odbc::odbc(), 
                               .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=", path_current_ES_DB))

# load data in R =====
# Retrieve all data from the ES database
Previous_weekEStables <- DBI::dbGetQuery(Previous_ESdb, "SELECT * FROM Environmental ORDER BY LabName, EpidNumber;", stringsAsFactors = FALSE) |>
  tibble()

Current_week_EStables <- DBI::dbGetQuery(Current_ESdb, "SELECT * FROM Environmental ORDER BY LabName, EpidNumber;", stringsAsFactors = FALSE) |>
  tibble()

# Identify new entries (present in Current but not in Previous)
new_entries <- anti_join(Current_week_EStables, Previous_weekEStables, by = "IDNumber")

# Identify deleted entries (present in Previous but not in Current)
deleted_entries <- anti_join(Previous_weekEStables, Current_week_EStables, by = "IDNumber")

# Identify updated entries (ICLabID exists in both but other values changed)
common_entries <- inner_join(Previous_weekEStables, Current_week_EStables, by = "IDNumber", suffix = c("_prev", "_curr"))

updated_entries <- common_entries |>
  filter(
    Dateofcollection_prev != Dateofcollection_curr |  Datesampleinlab_prev != Datesampleinlab_curr | Samplecondition_prev != Samplecondition_curr |
      Finalcellcultureresult_prev != Finalcellcultureresult_curr | Datefinalcultureresult_prev != Datefinalcultureresult_curr |
      FinalcombinedrRTPCRresults_prev != FinalcombinedrRTPCRresults_curr | DateFinalCombinedResult_prev != DateFinalCombinedResult_curr
  ) |>
  select(IDNumber, starts_with("Dateofcollection"), starts_with("Datesampleinlab"), starts_with("Samplecondition"), starts_with("Finalcellcultureresult"),
         starts_with("Datefinalcultureresult"), starts_with("FinalcombinedrRTPCRresults"), starts_with("DateFinalCombinedResult"))

# Combine results into a summary table
changes_summary <- list(
  New_Entries = new_entries,
  Deleted_Entries = deleted_entries,
  Updated_Entries = updated_entries
)

# Save results as an Excel file
openxlsx::write.xlsx(changes_summary, "../data/outputs_wk_checks/ES_Changes_Report.xlsx")

