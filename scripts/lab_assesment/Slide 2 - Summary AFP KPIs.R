# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages =====
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
p_load(tidyverse, RODBC,gt, gtExtras, webshot, officer)

#Give the path to the AFP database
path_AFP <- "../data/dbs/AFP_160724.mdb" 
labname <- "CAE" # Replace with the actual labname you want to filter by

# Connect to the Microsoft Access database =====
AFPdb <- DBI::dbConnect(odbc::odbc(), 
                        .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                              DBQ=", path_AFP))
# load data in R =====
# Retrieve all data from the AFP database
AFPtables <- DBI::dbGetQuery(AFPdb, "SELECT * FROM POLIOLAB ORDER BY LabName, EpidNumber;", stringsAsFactors = FALSE) |>
  tibble() |>  mutate(proxy_date_infor_itd = coalesce(DateIsolateinforITD, DateLarmIsolateRec, DateRarmIsolateSentforITD)
  ) |>
  # select samples collected in 2024 only
  filter(substr(ICLabID, start = 5, stop = 6) == 24 )

Specify_the_period <- paste0("WEEK 1 - " , 
                             (epiweek(as.Date(ymd(AFPtables$DateUpdated))) - 1) |> unique(), ", 2024")

# Analysis of databases =====
#AFPtables_gt <- 
  AFPtables |>
  filter(LabName == labname) |>
  distinct(ICLabID, .keep_all = "TRUE") |>
  mutate( FinalCellCultureResult = str_replace_all(FinalCellCultureResult, "Supected", "Suspected"),
          CountryCode = substr(EpidNumber, start = 1, stop = 3), .after = LabName ) |>
  mutate(StoolCondition = str_replace_all(StoolCondition, "1-Adéquat", "1-Good")) |>
  select(LabName, CountryCode, DateStoolReceivedinLab, StoolCondition, FinalCellCultureResult, DateFinalCellCultureResults,
         proxy_date_infor_itd, FinalITDResult, DateFinalrRTPCRResults) |>
  group_by(CountryCode) |>
  mutate(workload_by_lab = n(),
         # sample conditions
         is_good = if_else( (StoolCondition == "1-Good" | StoolCondition == "1-Bonne"), 1, 0), 
         
         # cell culture < 14 days
         is_culture_result = if_else(!is.na(FinalCellCultureResult), 1, 0),
         time_culture_results = as.numeric(difftime(DateFinalCellCultureResults, DateStoolReceivedinLab, units = "days")),
         is_culture_results_14days = if_else( (!is.na(FinalCellCultureResult) & time_culture_results < 15 & time_culture_results > 0), 1, 0),
         
         is_itd = if_else( (FinalCellCultureResult == "1-Suspected Poliovirus" | FinalCellCultureResult == "4-Suspected Poliovirus + NPENT"), 1, 0),
         time_itd_results_7days = as.numeric(difftime(DateFinalrRTPCRResults, proxy_date_infor_itd, units = "days")),
         time_itd_results_21days = as.numeric(difftime(DateFinalrRTPCRResults, DateStoolReceivedinLab, units = "days")),
         
         culture_results = sum(is_culture_result),
         culture_results_14days = sum(is_culture_results_14days),
         
         ITD_results = sum(is_itd),
         is_itd_7days = if_else( 
           ( (FinalCellCultureResult == "1-Suspected Poliovirus" | FinalCellCultureResult == "4-Suspected Poliovirus + NPENT") &
               !is.na(FinalITDResult) & time_itd_results_7days < 8 & time_itd_results_7days >= 0), 1, 0),
         ITD_results_7days = sum(is_itd_7days),
         
         is_itd_21days = if_else( (FinalCellCultureResult == "1-Suspected Poliovirus" | FinalCellCultureResult == "4-Suspected Poliovirus + NPENT") &
                                    (!is.na(FinalITDResult) & time_itd_results_21days < 22 & time_itd_results_21days >= 0), 1, 0),
         ITD_results_21days = sum(is_itd_21days)
  ) |>
  summarize(
    nb_workload_by_lab = n(),
    nb_samples_good_cond = sum(is_good, na.rm = TRUE),
    Prop_sample_good_cond = 100 * sum(is_good, na.rm = TRUE) / nb_workload_by_lab,
    nb_culture_results = sum(is_culture_result, na.rm = TRUE),
    nb_culture_results_14days = sum(is_culture_results_14days, na.rm = TRUE),
    Prop_culture_results_14days = 100 * nb_culture_results_14days / nb_culture_results,
    nb_ITD_results = sum(is_itd, na.rm = TRUE),
    nb_ITD_results_7days = sum(is_itd_7days, na.rm = TRUE),
    Prop_ITD_7days = 100 * nb_ITD_results_7days / nb_ITD_results,
    nb_ITD_results_21days = sum(is_itd_21days, na.rm = TRUE),
    Prop_ITD_21days = 100 * nb_ITD_results_21days / nb_ITD_results
  ) |>
  dplyr::select(CountryCode, nb_workload_by_lab, nb_samples_good_cond, Prop_sample_good_cond, nb_culture_results, nb_culture_results_14days,
                Prop_culture_results_14days, nb_ITD_results, nb_ITD_results_7days, Prop_ITD_7days, nb_ITD_results_21days, Prop_ITD_21days) |> #check values
  
  gt() |>
  #edit some columns names
  cols_label(
    "nb_workload_by_lab" = "# of Stool specimens",
    "nb_samples_good_cond" = "# samples good conditions",
    "nb_culture_results" = "# culture Result",
    "nb_culture_results_14days" = "# of culture results in 14 days",
    "nb_ITD_results_7days" = "# of ITD Results in 7 days",
    "nb_ITD_results" = "# ITD results",
    "Prop_sample_good_cond" = "Samples in Good Condition",
    "Prop_culture_results_14days" = "PV Isolation Results on Time",
    "Prop_ITD_7days" = "ITD Results in 7 days of receipt of Isolate",
    "nb_ITD_results_21days" = "# of ITD Results in 21 days",
    "Prop_ITD_21days" = "Final lab results availaible in 21 days of receipt"
  ) |>
  # insert a summary for proportions
  grand_summary_rows(
    columns = starts_with("Prop"),
    fns = list(
      "AVG" = ~ mean(.x, na.rm = TRUE)
    ) )  |>
  grand_summary_rows(
    columns = starts_with("nb_"),
    fns = list(
      "TOTAL" = ~ sum(.x, na.rm = TRUE)
    ) )  |>
  # customize the summary line
  tab_style(
    style = list(
      cell_text(weight = "bold", size = px(14))  # Bold and increase font size
    ),
    locations = cells_grand_summary()
  ) |>
  #center the values in the defined columns
  cols_align(
    align = "center",
    columns = c(1:12)
  ) |>
  #give a header to the table as well as a sub title
  tab_header(
    title = md(paste0("**AFP : SUMMARY OF AFRO LABORATORY KEY PERFORMANCE INDICATORS (KPIs) in**", " ", labname)),
    subtitle = md(paste0("**",Specify_the_period,"**") ) ) |>
  # add percentage in cells
  fmt_number(
    columns = c(`Prop_sample_good_cond`, `Prop_culture_results_14days`,
                `Prop_ITD_7days`, `Prop_ITD_21days`),
    decimals = 0,
    pattern = "{x} %"
  ) |>
  sub_missing(
    columns = 2:12,
    rows = everything(),
    missing_text = "-"
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
  gt_theme_538()|>
  
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

AFPtables_gt

# export my table
gtsave(AFPtables_gt, "../data/outputs/AFPtables.html")
# Convert HTML to PNG
webshot::webshot("output/AFPtables.html", "output/AFPtables.png")
























