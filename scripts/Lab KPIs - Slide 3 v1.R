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
    workload_by_lab = n(),
    Prop_sample_good_cond = 100 * sum(is_good, na.rm = TRUE) / workload_by_lab,
    culture_results = sum(is_culture_result, na.rm = TRUE),
    culture_results_14days = sum(is_culture_results_14days, na.rm = TRUE),
    Prop_culture_results_14days = 100 * culture_results_14days / culture_results,
    ITD_results = sum(is_itd, na.rm = TRUE),
    ITD_results_7days = sum(is_itd_7days, na.rm = TRUE),
    Prop_ITD_7days = 100 * ITD_results_7days / ITD_results,
    ITD_results_21days = sum(is_itd_21days, na.rm = TRUE),
    Prop_ITD_21days = 100 * ITD_results_21days / ITD_results
  ) |>
  dplyr::select(LabName, workload_by_lab, Prop_sample_good_cond, culture_results, culture_results_14days, 
                Prop_culture_results_14days, ITD_results, ITD_results_7days, Prop_ITD_7days, ITD_results_21days, Prop_ITD_21days) |> #check values
  
  gt() |>
  #edit some columns names
  cols_label(
    "workload_by_lab" = "# of Stool specimens",
    "culture_results" = "# culture Result",
    "culture_results_14days" = "# of culture results in 14 days",
    "ITD_results_7days" = "# of ITD Results in 7 days",
    "ITD_results" = "# ITD results",
    "Prop_sample_good_cond" = "Samples in Good Condition",
    "Prop_culture_results_14days" = "PV Isolation Results on Time",
    "Prop_ITD_7days" = "ITD Results in 7 days of receipt of Isolate",
    "ITD_results_21days" = "# of ITD Results in 21 days",
    "Prop_ITD_21days" = "Final lab results availaible in 21 days of receipt"
  ) |>
  #center the values in the defined columns
  cols_align(
    align = "center",
    columns = c(1:11)
  ) |>
  #give a header to the table as well as a sub title
  tab_header(
    title = md(paste0("**AFP : SUMMARY OF AFRO LABORATORY KEY PERFORMANCE INDICATORS (KPIs)** ")),
    subtitle = md(paste0("**",Specify_the_period,"**") ) ) |>
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


# export my table
gtsave(AFPtables_gt, "output/AFPtables.html")
# Convert HTML to PNG
webshot::webshot("output/AFPtables.html", "output/AFPtables.png")


# open the table of our presentation
Pres_ppt <- read_pptx(path = "data/AFRO polio labs bulletin week 1-18_2024.pptx")

# Insert the image in a new slide
#Pres_ppt <- ph_with(on_slide(Pres_ppt, index = 3), external_img("output/AFPtables.png"), location = ph_location_fullsize())

# add the table in the 4th slide of the presentation
Pres_ppt <- ph_with(on_slide(Pres_ppt, index = 4), external_img("output/AFPtables.png"), 
                    location = ph_location(left = 1, top = 1, width = 12, height = 8))

# Save the updated presentation
print(Pres_ppt, target = paste0("data/AFRO polio labs bulletin week 1-", Specify_the_period, "_2024.pptx"))































