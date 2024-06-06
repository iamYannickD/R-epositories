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
  gt() |>
  #edit some columns names
  cols_label(
    "samples_with_results" = "# of Specimens with Results",
    "pv_positive" = "Culture +ve for PV",
    "prop_pv_positive" = "Prop Culture +ve for PV",
    "pv_positive_and_npent" = "Culture +ve for PV & NPEV",
    "prop_pv_positive_and_npent" = "Prop Culture +ve for PV & NPEV",
    "npent" = "NPEV",
    "prop_npent" = "Prop NPEV",
    "negative" = "Negative",
    "prop_negative" = "Prop Negative",
    "pending_culture_results" = "Total Pending",
    "pending_culture_15_30" = "15 - 30 days",
    "pending_culture_30_60" = "31 - 60 days",
    "pending_culture_more_60" = "> 60 days"
  ) |>
  #center the values in the defined columns
  cols_align(
    align = "center",
    columns = c(2:14)
  ) |>
  tab_spanner(
    label = md('**Culture +ve for PV**'),
    columns = 3:4) |>
  tab_spanner(
    label = md('**Culture +ve for PV & NPEV**'),
    columns = 5:6) |>
  tab_spanner(
    label = md('**NPEV**'),
    columns = 7:8) |>
  tab_spanner(
    label = md('**Negative**'),
    columns = 9:10) |>
  #add the title that covers the columns in the 3th and 10th row
  tab_spanner(
    label = md('**PRIMARY VIRUS ISOLATION RESULTS**'),
    columns = 3:10) |>
  #add the title that covers the columns in the 7th and 8th row
  tab_spanner(
    label = md('**Pending Samples**'),
    columns = 11:14) |>
  #give a header to the table as well as a sub title
  tab_header(
    title = md(paste0("**AFP : Timeliness and Results of Primary Isolation (14 days)** ")),
    subtitle = md(paste0("**",Specify_the_period,"**")) ) |>
  sub_missing(
    columns = 2:14,
    rows = everything(),
    missing_text = 0
    #missing_text = "---"
  ) |>
  # add percentage in cells
  fmt_number(
    columns = c(1:14),
    decimals = 0,
    pattern = "{x}"
  ) |>
  fmt_number(
    columns = c(prop_pv_positive, prop_pv_positive_and_npent, prop_npent, prop_negative ),
    decimals = 1,
    pattern = "{x} %"
  ) |>
  #color the table based on the values in those cells
  # For sample conditions ====
tab_style(
  style = cell_fill(color = "pink"),
  locations = cells_body(
    columns = pending_culture_results,
    rows = pending_culture_results >= 15)
)  |>
  tab_style(
    style = cell_fill(color = "red2"),
    locations = cells_body(
      columns = pending_culture_results,
      rows = pending_culture_results >= 31)
  ) |> 
  tab_style(
    style = cell_fill(color = "red4"),
    locations = cells_body(
      columns = pending_culture_results,
      rows = pending_culture_results >= 61)
  ) |>
  data_color(
    #method = "numeric",
    columns = pending_culture_results,
    rows = pending_culture_results >= 15,
    palette = "ggsci::red_material"
  )  |>
  # Color in gray the table and beautify the formating
  opt_stylize(style = 6, color = 'gray') |>
  #call that theme
  #other themes gt_theme_excel()  |>gt_theme_pff() |>
  gt_theme_538() |>
  
  opt_align_table_header(align = "center") |>
  #reshape the table
  tab_options(
    data_row.padding = px(2),
    summary_row.padding = px(3), # A bit more padding for summaries
    row_group.padding = px(4)    # And even more for our groups
  ) |>
  tab_style(
    #style = cell_text(weight = "bold"),
    # Color each country in the group
    style = cell_fill(color = "lightgray"),
    locations = cells_row_groups(groups = everything()) 
  ) |>
  opt_css(
    css = "
    .cell-output-display {
      overflow-x: unset !important;
    }
    div#two {
      overflow-x: unset !important;
      overflow-y: unset !important;
    }
    #two .gt_col_heading {
      position: sticky !important;
      top: 0 !important;
    }
    "
  )

