#check directory =====
getwd()

# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages =====
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
p_load(tidyverse, RODBC,gt, gtExtras, webshot, officer)
#to convert the web image into an image
webshot::install_phantomjs()

#Give the path to the AFP database
path_AFP <- "data/afp_wk18.mdb" 

# Connect to the Microsoft Access database =====
AFPdb <- DBI::dbConnect(odbc::odbc(), 
                        .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                              DBQ=", path_AFP))
# load data in R =====
# Retrieve all data from the AFP database
AFPtables <- DBI::dbGetQuery(AFPdb, "SELECT * FROM POLIOLAB ORDER BY LabName, EpidNumber;", stringsAsFactors = FALSE) |>
  tibble() |>  mutate(proxy_date_infor_itd = if_else(is.na(DateIsolateinforITD), DateLarmIsolateRec, DateIsolateinforITD)
                ) |>
  # select samples collected in 2024 only
  filter(substr(ICLabID, start = 5, stop = 6) == 24 )

Specify_the_period <- paste0("WEEK 1 - ", 
                             (epiweek(as.Date(ymd_hms(AFPtables$DateUpdated))) - 5) |> unique(), ", 2024")

# Analysis of databases =====
AFPtables_gt <- 
  AFPtables |>
  #filter( AFPtables$LabName != "CDC" & year(AFPtables$DateOfOnset) > 2023 ) |>
  filter( AFPtables$LabName != "CDC") |>
  #distinct(ICLabID, .keep_all = "TRUE") |>
  group_by(LabName) |>
  summarise(workload_by_lab = n()) |>
  ungroup() |>
  # samples arrived in good conditions in the lab
  left_join (
      AFPtables |>
        #filter(AFPtables$StoolCondition %in% c("1-Good", "1-Adéquat", "2-Bad", "2-Inadéquat") | is.na(AFPtables$StoolCondition)) |>
        filter(AFPtables$StoolCondition %in% c("1-Good", "1-Adéquat")) |>
        #distinct(ICLabID, .keep_all = "TRUE") |>
        group_by(LabName) |>
        summarise(Sample_good_cond = n()), 
      by = "LabName") |>
      ungroup() |>
  mutate(Prop_sample_good_cond = round( Sample_good_cond / workload_by_lab * 100, 0) ) |>
  # total cell culture results =====
  left_join(
    AFPtables |>
      filter(!is.na(AFPtables$FinalCellCultureResult) & !is.nan(AFPtables$FinalCellCultureResult) & !is.null(AFPtables$FinalCellCultureResult)) |>
      #distinct(ICLabID, .keep_all = "TRUE") |>
      group_by(LabName) |>
      summarise(culture_results = n()), 
    by = "LabName" ) |>
  # Cell culture results in less than 14 days
  left_join(
    AFPtables |>
      filter(!is.na(AFPtables$FinalCellCultureResult) & !is.nan(AFPtables$FinalCellCultureResult) & !is.null(AFPtables$FinalCellCultureResult) &
               (AFPtables$DateFinalCellCultureResults - AFPtables$DateStoolReceivedinLab) < 15 & 
               (AFPtables$DateFinalCellCultureResults - AFPtables$DateStoolReceivedinLab) >= 0 ) |>
      #distinct(ICLabID, .keep_all = "TRUE") |>
      group_by(LabName) |>
      summarise(culture_results_14days = n()), 
    by = "LabName" ) |>
  mutate(Prop_culture_results_14days = round(culture_results_14days / culture_results * 100, 0) ) |>
  # all ITD results
  left_join(
    AFPtables |>
      filter((str_detect(AFPtables$FinalCellCultureResult, "^1") | str_detect(AFPtables$FinalCellCultureResult, "^4"))
             & !is.nan(AFPtables$FinalITDResult) & !is.null(AFPtables$FinalITDResult)) |>
      #distinct(ICLabID, .keep_all = "TRUE") |>
      group_by(LabName) |>
      summarise(ITD_results = n()), 
    by = "LabName" ) |>
  # ITD results in less than 7 days from reception in lab ====
  left_join(
    AFPtables |>
      filter(((str_detect(AFPtables$FinalCellCultureResult, "^1") | str_detect(AFPtables$FinalCellCultureResult, "^4"))
             & !is.nan(AFPtables$FinalITDResult) & !is.null(AFPtables$FinalITDResult)) 
            # & between((AFPtables$DateFinalrRTPCRResults - AFPtables$DateIsolateinforITD), ymd_hms(0), ymd_hms(700000)) ) |>
            & (AFPtables$DateFinalrRTPCRResults - AFPtables$DateIsolateinforITD) >= 0
            & (as.Date(AFPtables$DateFinalrRTPCRResults) - as.Date(AFPtables$proxy_date_infor_itd)) < 8) |>
      #filter(LabName == "SOA") |>
      
      #select(ICLabID, LabName, proxy_date_infor_itd, DateFinalrRTPCRResults) |>
      #distinct(ICLabID, .keep_all = "TRUE") |>
      group_by(LabName) |>
      summarise(ITD_results_7days = n()), 
    by = "LabName" ) |>
  mutate(Prop_ITD_7days = round(ITD_results_7days / ITD_results * 100, 0)) |>
  # specimen with final lab results < 21 days =====
  left_join(
    AFPtables |>
      filter(!is.na(AFPtables$FinalITDResult) & !is.nan(AFPtables$FinalITDResult) & !is.null(AFPtables$FinalITDResult) &
               (AFPtables$DateFinalrRTPCRResults - AFPtables$DateStoolReceivedinLab) < 22 & 
               (AFPtables$DateFinalrRTPCRResults - AFPtables$DateStoolReceivedinLab) >= 0 ) |>
      #distinct(ICLabID, .keep_all = "TRUE") |>
      group_by(LabName) |>
      summarise(ITD_results_21days = n()), 
    by = "LabName" ) |>
  mutate(Prop_ITD_21days = round( ITD_results_21days / ITD_results * 100, 0) ) |> 
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
  