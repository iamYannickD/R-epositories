#check directory =====
getwd()

# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages =====
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
p_load(tidyverse, RODBC,gt, gtExtras)

#Give the path to the ES database
Specify_the_period <- "WEEK 1 - 18, 2024"
path_ES_2024 = "data/es_2024_wk18.mdb"

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

# Analysis of databases =====
EStables2024 |>
  #distinct(IDNumber, .keep_all = "TRUE") |>
  group_by(Labname) |>
  summarise(workload_by_lab = n()) |>
  ungroup() |>
  # samples arrived in good conditions in the lab
  left_join (
    EStables2024 |>
      filter(EStables2024$Samplecondition == "1-Good") |>
      #distinct(ICLabID, .keep_all = "TRUE") |>
      group_by(Labname) |>
      summarise(Sample_good_cond = n()), 
    by = "Labname") |>
  ungroup() |>
  mutate(Prop_sample_good_cond = round( Sample_good_cond / workload_by_lab * 100, 0) ) |>
  # total cell culture results =====
left_join(
  EStables2024 |>
    filter(!is.na(Finalcellcultureresult) & !is.nan(Finalcellcultureresult) & !is.null(Finalcellcultureresult)) |>
    #distinct(ICLabID, .keep_all = "TRUE") |>
    group_by(Labname) |>
    summarise(culture_results = n()), 
  by = "Labname" ) |>
  # Cell culture results in less than 14 days
  left_join(
    EStables2024 |>
      filter(!is.na(Finalcellcultureresult) & !is.nan(Finalcellcultureresult) & !is.null(Finalcellcultureresult) &
               (Datefinalcultureresult - Datesampleinlab) < 15 & 
               (Datefinalcultureresult - Datesampleinlab) >= 0 ) |>
      #distinct(ICLabID, .keep_all = "TRUE") |>
      group_by(Labname) |>
      summarise(culture_results_14days = n()), 
    by = "Labname" ) |>
  mutate(Prop_culture_results_14days = round(culture_results_14days / culture_results * 100, 0) ) |>
  # all ITD results
  left_join(
    EStables2024 |>
      filter((str_detect(Finalcellcultureresult, "^1") | str_detect(Finalcellcultureresult, "^4"))) |>
      #  !is.na(FinalcombinedrRTPCRresults) & !is.nan(FinalcombinedrRTPCRresults) & !is.null(FinalcombinedrRTPCRresults)) |>
      #distinct(ICLabID, .keep_all = "TRUE") |>
      group_by(Labname) |>
      summarise(ITD_results = n()), 
    by = "Labname" ) |>
  # ITD results in less than 7 days from reception in lab ====
left_join(
  EStables2024 |>
    filter((str_detect(Finalcellcultureresult, "^1") | str_detect(Finalcellcultureresult, "^4")) &
             (as.Date(EStables2024$DateFinalCombinedResult) - as.Date(EStables2024$date_result_to_lab)) < 8 & 
             (as.Date(EStables2024$DateFinalCombinedResult) - as.Date(EStables2024$date_result_to_lab)) >= 0 ) |>
    #distinct(ICLabID, .keep_all = "TRUE") |>
    group_by(Labname) |>
    summarise(ITD_results_7days = n()), 
  by = "Labname" ) |>
  mutate(Prop_ITD_7days = round(ITD_results_7days / ITD_results * 100, 0)) |>
  # specimen with final lab results < 21 days =====
left_join(
  EStables2024 |>
    filter(!is.na(FinalcombinedrRTPCRresults) & !is.nan(FinalcombinedrRTPCRresults) & !is.null(FinalcombinedrRTPCRresults) &
             (DateFinalCombinedResult - Datesampleinlab) < 22 & 
             (DateFinalCombinedResult - Datesampleinlab) >= 0 ) |>
    #distinct(ICLabID, .keep_all = "TRUE") |>
    group_by(Labname) |>
    summarise(ITD_results_21days = n()), 
  by = "Labname" ) |>
  mutate(Prop_ITD_21days = round( ITD_results_21days / ITD_results * 100, 0) ) |> 
  dplyr::select(Labname, workload_by_lab, Prop_sample_good_cond, culture_results, culture_results_14days,
                Prop_culture_results_14days, ITD_results, ITD_results_7days, Prop_ITD_7days, ITD_results_21days, Prop_ITD_21days) |> #check values
  
  gt() |>
  #edit some columns names
  cols_label(
    "workload_by_lab" = "# of Stool specimens",
    "culture_results" = "# Culture Results",
    "culture_results_14days" = "# of Culture results in 14 days", 
    "ITD_results" = "# ITD Results",
    "ITD_results_7days" = "ITD Results in 7 days",
    "ITD_results_21days" = "ITD results in 21 days",
    "Prop_sample_good_cond" = "Samples in Good Condition",
    "Prop_culture_results_14days" = "PV Isolation Results on Time",
    "Prop_ITD_7days" = "ITD Results in 7 days of receipt of Isolate",
    "Prop_ITD_21days" = "Stool speciments with Final lab results availaible in 21 days of receipt"
  ) |>
  #center the values in the defined columns
  cols_align(
    align = "center",
    columns = c(1:11)
  ) |>
  #give a header to the table as well as a sub title
  tab_header(
    title = md(paste0("**ES : SUMMARY OF AFRO LABORATORY KEY PERFORMANCE INDICATORS (KPIs)** ")),
    subtitle = md(paste0("**",Specify_the_period,"**")) ) |>
  # add percentage in cells
  fmt_number(
    columns = c(`Prop_sample_good_cond`, `Prop_culture_results_14days`,
                `Prop_ITD_7days`, `Prop_ITD_21days`),
    decimals = 0,
    pattern = "{x} %"
  ) |>
  sub_missing(
    columns = 2:11,
    rows = everything(),
    missing_text = "-"
    #missing_text = "---"
  ) |>
  #color the table based on the values in those cells
  # For sample conditions ====
tab_style(
  style = cell_fill(color = "#00B050"),
  locations = cells_body(
    columns = Prop_sample_good_cond,
    rows = Prop_sample_good_cond >= 80)
)  |>
  tab_style(
    style = cell_fill(color = "yellow"),
    locations = cells_body(
      columns = Prop_sample_good_cond,
      rows = Prop_sample_good_cond < 80)
  ) |> 
  tab_style(
    style = cell_fill(color = "#00B050"),
    locations = cells_body(
      columns = Prop_sample_good_cond,
      rows = Prop_sample_good_cond == 100)
  ) |>
  # for pv isolation time 14 days
  tab_style(
    style = cell_fill(color = "#00B050"),
    locations = cells_body(
      columns = Prop_culture_results_14days,
      rows = Prop_culture_results_14days >= 80)
  )  |>
  tab_style(
    style = cell_fill(color = "yellow"),
    locations = cells_body(
      columns = Prop_culture_results_14days,
      rows = Prop_culture_results_14days < 80)
  ) |> 
  tab_style(
    style = cell_fill(color = "#00B050"),
    locations = cells_body(
      columns = Prop_culture_results_14days,
      rows = Prop_culture_results_14days == 100)
  ) |>
  # for ITD 7 days
  tab_style(
    style = cell_fill(color = "#00B050"),
    locations = cells_body(
      columns = Prop_ITD_7days,
      rows = Prop_ITD_7days >= 80)
  )  |>
  tab_style(
    style = cell_fill(color = "yellow"),
    locations = cells_body(
      columns = Prop_ITD_7days,
      rows = Prop_ITD_7days < 80)
  ) |>
  tab_style(
    style = cell_fill(color = "#00B050"),
    locations = cells_body(
      columns = Prop_culture_results_14days,
      rows = Prop_culture_results_14days == 100)
  ) |>
  # for ITD 21 days
  tab_style(
    style = cell_fill(color = "#00B050"),
    locations = cells_body(
      columns = Prop_ITD_21days,
      rows = Prop_ITD_21days >= 80)
  )  |>
  tab_style(
    style = cell_fill(color = "yellow"),
    locations = cells_body(
      columns = Prop_ITD_21days,
      rows = Prop_ITD_21days < 80)
  ) |>
  tab_style(
    style = cell_fill(color = "#00B050"),
    locations = cells_body(
      columns = Prop_ITD_21days,
      rows = Prop_ITD_21days == 100)
  ) |>
  # for no data
  tab_style(
    style = cell_fill(color = "gray"),
    locations = cells_body(
      columns = Prop_ITD_21days,
      rows = is.na(Prop_ITD_21days))
  ) |>
  tab_style(
    style = cell_fill(color = "gray"),
    locations = cells_body(
      columns = Prop_ITD_7days,
      rows = is.na(Prop_ITD_7days))
  ) |>
  
  # Color in gray the table and beautify the formating
  opt_stylize(style = 6, color = 'gray') |>
  #call that theme
  #other themes gt_theme_excel()  |>gt_theme_pff() |>
  gt_theme_excel() |>
  
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