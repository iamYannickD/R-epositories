# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages =====
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
p_load(tidyverse, RODBC)

#Give the path to the AFP database
path_previous_AFP_DB <- "../data/dbs/AFP_WK07.mdb" 
path_current_AFP_DB <- "../data/dbs/AFP_WK08.mdb" 
#labname <- c("CAE", "CIV", "ETH") # Replace with the actual labname(s) you want to filter by

# Connect to the Microsoft Access databases =====
Previous_AFPdb <- DBI::dbConnect(odbc::odbc(), 
                        .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=", path_previous_AFP_DB))

Current_AFPdb <- DBI::dbConnect(odbc::odbc(), 
                                 .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=", path_current_AFP_DB))

# load data in R =====
# Retrieve all data from the AFP database
Previous_week_AFPtables <- DBI::dbGetQuery(Previous_AFPdb, "SELECT * FROM POLIOLAB ORDER BY LabName, EpidNumber;", stringsAsFactors = FALSE) |>
  tibble()

Current_week_AFPtables <- DBI::dbGetQuery(Current_AFPdb, "SELECT * FROM POLIOLAB ORDER BY LabName, EpidNumber;", stringsAsFactors = FALSE) |>
  tibble()

Specify_the_period <- paste0("WEEK 1 - " , 
                             (epiweek(as.Date(ymd(Current_week_AFPtables$DateUpdated))) - 1) |> unique(), ", 2025")



# Identify new entries (present in Current but not in Previous)
new_entries <- anti_join(Current_week_AFPtables, Previous_week_AFPtables, by = "ICLabID")

# Identify deleted entries (present in Previous but not in Current)
deleted_entries <- anti_join(Previous_week_AFPtables, Current_week_AFPtables, by = "ICLabID")

# Identify updated entries (ICLabID exists in both but other values changed)
common_entries <- inner_join(Previous_week_AFPtables, Current_week_AFPtables, by = "ICLabID", suffix = c("_prev", "_curr"))

updated_entries <- common_entries |>
  filter(
    EpidNumber_prev != EpidNumber_curr |  DateOfOnset_prev != DateOfOnset_curr | DateStoolCollected_prev != DateStoolCollected_curr |
      FinalCellCultureResult_prev != FinalCellCultureResult_curr | DateFinalCellCultureResults_prev != DateFinalCellCultureResults_curr |
      FinalITDResult_prev != FinalITDResult_curr | ITDMixture_prev != ITDMixture_curr | DateFinalrRTPCRResults_prev != DateFinalrRTPCRResults_curr
  ) |>
  select(ICLabID, starts_with("EpidNumber"), starts_with("DateOfOnset"), starts_with("DateStoolCollected"), starts_with("FinalCellCultureResult"),
         starts_with("DateFinalCellCultureResults"), starts_with("FinalITDResult"), starts_with("ITDMixture"), starts_with("DateFinalrRTPCRResults"))

# Combine results into a summary table
changes_summary <- list(
  New_Entries = new_entries,
  Deleted_Entries = deleted_entries,
  Updated_Entries = updated_entries
)

# Save results as an Excel file
openxlsx::write.xlsx(changes_summary, "../data/outputs_wk_checks/AFP_Changes_Report.xlsx")
