# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages =====
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
p_load(tidyverse, RODBC,gt, gtExtras)

#Give the path to the ES database
Specify_the_period <- "WEEK 1 - 28, 2024"
path_ES_2024 = "../data/dbs/ES_160724.mdb"
labname <- "CAE" # Replace with the actual labname you want to filter by
DateUpdated <- "2024-07-16"

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
                                              "MDG, Nigeria" = "MDG", "ZAM UTH" = "ZAM", "ZAM-UTH" = "ZAM"))
  )

# Analysis of databases =====

EStables2024 |>
  mutate(Labname = str_replace_all(Labname, "ESWATINI", "SOA" )) |>
  filter(Labname == labname) |>
  group_by(Countrycode) |>
  mutate(is_result = if_else(!is.na(Finalcellcultureresult), 1, 0),
         is_pv_positive = if_else((Finalcellcultureresult == "1-Suspected Poliovirus"), 1, 0),
         is_pv_positive_and_npent = if_else((Finalcellcultureresult == "4-Suspected Poliovirus + NPENT"), 1, 0),
         is_pv_negative = if_else((Finalcellcultureresult == "2-Negative"), 1, 0),
         is_npent = if_else((Finalcellcultureresult == "3-NPENT"), 1, 0),
         
         # cell culture < 14 days
         time_culture_results = as.numeric(difftime(Datefinalcultureresult, Datesampleinlab, units = "days")),
         is_culture_results_14days = if_else( (!is.na(Finalcellcultureresult) & time_culture_results < 15 & time_culture_results > 0), 1, 0),
         
         culture_results = sum(is_result),
         culture_results_14days = sum(is_culture_results_14days),
         
         Datesampleinlab = as.Date(ymd(Datesampleinlab)),

         days_to_results = as.numeric(difftime(DateUpdated, Datesampleinlab, units = "days")),
         is_pending_culture_results = if_else(is.na(Finalcellcultureresult) & days_to_results >= 15, 1, 0),
         is_pending_culture_15_30 = if_else(is.na(Finalcellcultureresult) & (days_to_results >= 15 & days_to_results < 30), 1, 0),
         is_pending_culture_30_60 = if_else(is.na(Finalcellcultureresult) & (days_to_results >= 30 & days_to_results < 60), 1, 0),
         is_pending_culture_more_60 = if_else(is.na(Finalcellcultureresult) & days_to_results >= 60, 1, 0)
  )  |>
  summarise(
    nb_samples_with_results = sum(is_result, na.rm = TRUE),
    
    nb_culture_results_14days = sum(is_culture_results_14days, na.rm = TRUE),
    Prop_culture_results_14days = 100 * nb_culture_results_14days / nb_samples_with_results,
    
    nb_pv_positive = sum(is_pv_positive, na.rm = TRUE),
    prop_pv_positive = 100 * nb_pv_positive / nb_samples_with_results,
    
    nb_pv_positive_and_npent = sum(is_pv_positive_and_npent, na.rm = TRUE),
    prop_pv_positive_and_npent = 100 * nb_pv_positive_and_npent / nb_samples_with_results,
    
    nb_npent = sum(is_npent, na.rm = TRUE), 
    prop_npent = 100 * nb_npent / nb_samples_with_results,
    
    nb_pv_negative = sum(is_pv_negative, na.rm = TRUE),
    prop_negative = 100 * nb_pv_negative / nb_samples_with_results,
    
    nb_pending_culture_results = sum(is_pending_culture_results),
    nb_pending_culture_15_30 = sum(is_pending_culture_15_30),
    nb_pending_culture_30_60 = sum(is_pending_culture_30_60),
    nb_pending_culture_more_60 = sum(is_pending_culture_more_60)
  ) |>
  ungroup() |>
  gt() |>
  #edit some columns names
  cols_label(
    "nb_samples_with_results" = "# of Specimens with Results",
    "nb_culture_results_14days" = "# of culture results in 14 days",
    "Prop_culture_results_14days" = "Prop cell cultures results 14 days",
    "nb_pv_positive" = "Culture +ve for PV",
    "prop_pv_positive" = "Prop Culture +ve for PV",
    "nb_pv_positive_and_npent" = "Culture +ve for PV & NPEV",
    "prop_pv_positive_and_npent" = "Prop Culture +ve for PV & NPEV",
    "nb_npent" = "NPEV",
    "prop_npent" = "Prop NPEV",
    "nb_pv_negative" = "Negative",
    "prop_negative" = "Prop Negative",
    "nb_pending_culture_results" = "Total Pending",
    "nb_pending_culture_15_30" = "15 - 30 days",
    "nb_pending_culture_30_60" = "31 - 60 days",
    "nb_pending_culture_more_60" = "> 60 days"
  ) |>
  # insert a summary for proportions
  grand_summary_rows(
    columns = starts_with("Prop"),
    #missing_text = "-",
    fns = list(
      "AVG" = ~ mean(.x, na.rm = TRUE)
    ) )  |>
  # fmt_percent(
  #   columns = starts_with("Prop"),
  #   rows = grand_summary_rows,
  #   decimals = 1 ) 
  grand_summary_rows(
    columns = starts_with("nb_"),
    #missing_text = "-",
    fns = list(
      "TOTAL" = ~ sum(.x, na.rm = TRUE)
    ) )  |>
  # customize the summary line
  tab_style(
    style = list(
      cell_text(weight = "bold", size = px(14))
    ),
    locations = cells_grand_summary()
  )  |>
  #center the values in the defined columns
  cols_align(
    align = "center",
    columns = c(2:16)
  ) |>
  tab_spanner(
    label = md('**Culture +ve for PV**'),
    columns = 5:6) |>
  tab_spanner(
    label = md('**Culture +ve for PV & NPEV**'),
    columns = 7:8) |>
  tab_spanner(
    label = md('**NPEV**'),
    columns = 9:10) |>
  tab_spanner(
    label = md('**Negative**'),
    columns = 11:12) |>
  #add the title that covers the columns in the 3th and 10th row
  tab_spanner(
    label = md('**PRIMARY VIRUS ISOLATION RESULTS**'),
    columns = 3:12) |>
  #add the title that covers the columns in the 7th and 8th row
  tab_spanner(
    label = md('**Pending Samples**'),
    columns = 13:16) |>
  #give a header to the table as well as a sub title
  tab_header(
    title = md(paste0("**ES : Timeliness and Results of Primary Isolation (14 days)** ")),
    subtitle = md(paste0("**",Specify_the_period,"**")) ) |>
  sub_missing(
    columns = 2:16,
    rows = everything(),
    missing_text = 0
    #missing_text = "---"
  ) |>
  # add percentage in cells
  fmt_number(
    columns = starts_with("nb_"),
    decimals = 0,
    pattern = "{x}"
  ) |>
  fmt_number(
    columns = starts_with("Prop"),
    decimals = 1,
    pattern = "{x} %"
  ) |>
  #color the table based on the values in those cells
  # For sample conditions ====
tab_style(
  style = cell_fill(color = "pink"),
  locations = cells_body(
    columns = nb_pending_culture_results,
    rows = nb_pending_culture_results >= 15)
)  |>
  tab_style(
    style = cell_fill(color = "red2"),
    locations = cells_body(
      columns = nb_pending_culture_results,
      rows = nb_pending_culture_results >= 31)
  ) |> 
  tab_style(
    style = cell_fill(color = "red4"),
    locations = cells_body(
      columns = nb_pending_culture_results,
      rows = nb_pending_culture_results >= 61)
  ) |>
  data_color(
    #method = "numeric",
    columns = nb_pending_culture_results,
    rows = nb_pending_culture_results >= 1,
    palette = "ggsci::red_material"
  )  |>
  # Color in gray the table and beautify the formating
  #opt_stylize(style = 6, color = 'gray') |> #remove style on lines
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
  
  
  
  
  
  
  
  
  