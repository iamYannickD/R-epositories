# Check if the package pacman  is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages =====
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
p_load(tidyverse, RODBC,gt, gtExtras)

#Give the path to the ES database
Specify_the_period <- "WEEK 1 - 8, 2025"
path_ES_2024 = "../data/dbs/ES2025.mdb"
labname <- c("CAE", "CIV", "ETH") # Replace with the actual labname(s) you want to filter by

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
         date_result_to_lab = coalesce(Dateresultstolab, Datefinalcultureresult),
         date_itd_result = coalesce(DateFinalCombinedResult, DatefinalResultReported)
  )

#Specify_the_period <- paste0("WEEK 1 - ", (epiweek(as.Date(ymd_hms(AFPtables$DateUpdated))) - 1) |> unique(), ", 2024")

# Analysis of databases =====
EStables_gt <- 
  EStables2024 |>
  mutate(Labname = str_replace_all(Labname, "ESWATINI", "SOA" )) |>
  filter(Labname %in% labname) |>
  group_by(Labname, Countrycode) |>
  summarize(
    nb_workload_by_lab = n(),
    nb_Sample_good_cond = sum(Samplecondition == "1-Good", na.rm = TRUE),
    Prop_sample_good_cond = round(nb_Sample_good_cond / nb_workload_by_lab * 100, 0),
    
    nb_culture_results = sum(!is.na(Finalcellcultureresult) & !is.nan(Finalcellcultureresult) & !is.null(Finalcellcultureresult), na.rm = TRUE),
    nb_culture_results_14days = sum(!is.na(Finalcellcultureresult) & !is.nan(Finalcellcultureresult) & !is.null(Finalcellcultureresult) &
                                   (Datefinalcultureresult - Datesampleinlab) < 15 & 
                                   (Datefinalcultureresult - Datesampleinlab) >= 0, na.rm = TRUE),
    Prop_culture_results_14days = round(nb_culture_results_14days / nb_culture_results * 100, 0),
    
    nb_ITD_results = sum(str_detect(Finalcellcultureresult, "^1") | str_detect(Finalcellcultureresult, "^4"), na.rm = TRUE),
    nb_ITD_results_7days = sum((str_detect(Finalcellcultureresult, "^1") | str_detect(Finalcellcultureresult, "^4")) &
                              (as.Date(date_itd_result) - as.Date(date_result_to_lab)) < 8 & 
                              (as.Date(date_itd_result) - as.Date(date_result_to_lab)) >= 0, na.rm = TRUE),
    Prop_ITD_7days = round(nb_ITD_results_7days / nb_ITD_results * 100, 0),
    
    nb_ITD_results_21days = sum( (str_detect(Finalcellcultureresult, "^1") | str_detect(Finalcellcultureresult, "^4")) & 
                                !is.na(FinalcombinedrRTPCRresults) & (DateFinalCombinedResult - Datesampleinlab) < 22 & 
                                (DateFinalCombinedResult - Datesampleinlab) >= 0, na.rm = TRUE),
    Prop_ITD_21days = round(nb_ITD_results_21days / nb_ITD_results * 100, 0)
  ) |>
  select(Countrycode, nb_workload_by_lab, nb_Sample_good_cond, Prop_sample_good_cond, nb_culture_results, nb_culture_results_14days,
         Prop_culture_results_14days, nb_ITD_results, nb_ITD_results_7days, Prop_ITD_7days, nb_ITD_results_21days, Prop_ITD_21days) |>
  gt() |>
  #edit some columns names
  cols_label(
    "nb_workload_by_lab" = "# of Stool specimens",
    "nb_Sample_good_cond" = "# samples good conditions",
    "nb_culture_results" = "# Culture Results",
    "nb_culture_results_14days" = "# of Culture results in 14 days", 
    "nb_ITD_results" = "# ITD Results",
    "nb_ITD_results_7days" = "ITD Results in 7 days",
    "nb_ITD_results_21days" = "ITD results in 21 days",
    "Prop_sample_good_cond" = "Samples in Good Condition",
    "Prop_culture_results_14days" = "PV Isolation Results on Time",
    "Prop_ITD_7days" = "ITD Results in 7 days of receipt of Isolate",
    "Prop_ITD_21days" = "Stool speciments with Final lab results availaible in 21 days of receipt"
  ) |> 
  # insert a summary for proportions
  grand_summary_rows(
    columns = starts_with("Prop"),
    fns = list(
      "AVG" = ~ median(.x, na.rm = TRUE)
    ) )  |>
  grand_summary_rows(
    columns = starts_with("nb_"),
    fns = list(
      "TOTAL" = ~ sum(.x, na.rm = TRUE)
    ) ) |>
  #center the values in the defined columns
  cols_align(
    align = "center",
    columns = c(1:13)
  ) |>
  #give a header to the table as well as a sub title
  tab_header(
    title = md(paste0("**ES : SUMMARY OF AFRO LABORATORY KEY PERFORMANCE INDICATORS (KPIs)**")),
    subtitle = md(paste0("**",Specify_the_period,"**")) ) |>
  # add percentage in cells
  fmt_number(
    columns = c(`Prop_sample_good_cond`, `Prop_culture_results_14days`,
                `Prop_ITD_7days`, `Prop_ITD_21days`),
    decimals = 0,
    pattern = "{x} %"
  ) |>
  sub_missing(
    columns = 2:13,
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
  #other themes gt_theme_excel()  |>gt_theme_pff() |>  gt_theme_538()|>
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

EStables_gt

# export my table
gtsave(EStables_gt, "../data/outputs_db_ass/EStables.html")
