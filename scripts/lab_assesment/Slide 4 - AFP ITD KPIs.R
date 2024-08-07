# Check if the package pacman  is installed
# rm(list = ls())
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages =====
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
p_load(tidyverse, RODBC,gt, gtExtras, officer)

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
  tibble() |>  mutate(proxy_date_infor_itd = coalesce(DateIsolateinforITD, DateLarmIsolateRec, 
                                                      DateRarmIsolateSentforITD, DateFinalCellCultureResults)
  )   |>
  # select samples collected in 2024 only
  filter(substr(ICLabID, start = 5, stop = 6) == 24 )

Specify_the_period <- paste0("WEEK 1 - ", 
                             (epiweek(as.Date(ymd(AFPtables$DateUpdated))) - 1) |> unique(), ", 2024")

# Analysis of databases =====
AFPtables_gt <- 
  AFPtables |>
  filter(LabName == labname) |>
  #distinct(ICLabID, .keep_all = "TRUE") |>
  mutate( FinalCellCultureResult = str_replace_all(FinalCellCultureResult, "Supected", "Suspected"),
          CountryCode = substr(EpidNumber, start = 1, stop = 3), .after = LabName) |>
  select(CountryCode, DateStoolReceivedinLab, StoolCondition, FinalCellCultureResult, DateFinalCellCultureResults,
         proxy_date_infor_itd, FinalITDResult, DateFinalrRTPCRResults, DateUpdated) |>
  mutate(virus_cat = 
           case_when(
             (is.na(FinalITDResult) | FinalITDResult == "9-Invalid") ~ NA,
             (FinalITDResult %in% c("16-nOPV2", "PV2+/nOPV2+") ) ~ "nOPV2",
             (FinalITDResult %in% c("15-PV2", "PV2+/nOPV2-", "12-PV2+/nOPV2-", "5-PV2 SL")) ~ "PV2",
             (FinalITDResult %in% c("4-PV1-SL", "4-PV1 SL")) ~ "Sabin Type 1",
             FinalITDResult == "6-PV3 SL" ~ "Sabin Type 3",
             FinalITDResult == "7-NPEV" ~ "NPEV",
             FinalITDResult == "8-NEV" ~ "NEV",
             FinalITDResult == "10-Mixture" ~ "Mixture",
             FinalITDResult %in% c("12-PV1 SL Discordant", "1-PV1 NSL") ~ "Type 1 Discordant",
             #FinalITDResult == "3-PV3 NSL" ~ "Type 3 Discordant", 
             !is.na(FinalITDResult) ~ "check" # missed/unexpected results
           )) |>
  filter(!is.na(virus_cat)) |>
  group_by(CountryCode, virus_cat) |>
  summarise(n = n()) |>
  pivot_wider(names_from = virus_cat, values_from = n) |> 
  left_join(AFPtables |>
              filter(LabName == labname) |>
              #distinct(ICLabID, .keep_all = "TRUE") |>
              mutate(FinalCellCultureResult = str_replace_all(FinalCellCultureResult, "Supected", "Suspected"),
                     CountryCode = substr(EpidNumber, start = 1, stop = 3), .after = LabName) |>
              select(CountryCode, DateStoolReceivedinLab, StoolCondition, FinalCellCultureResult, DateFinalCellCultureResults,
                     proxy_date_infor_itd, FinalITDResult, DateFinalrRTPCRResults, DateUpdated) |>
              group_by(CountryCode) |>
              mutate(
                is_itd = if_else(FinalCellCultureResult %in% c("1-Suspected Poliovirus", "4-Suspected Poliovirus + NPENT"), 1, 0),
                time_itd_results_7days = as.numeric(difftime(DateUpdated, proxy_date_infor_itd, units = "days")),
                is_itd_more_7days = if_else(
                  FinalCellCultureResult %in% c("1-Suspected Poliovirus", "4-Suspected Poliovirus + NPENT") &
                    is.na(FinalITDResult) & time_itd_results_7days >= 8, 1, 0
                )
              ) |>
              summarize(
                ITD_results = sum(is_itd, na.rm = TRUE),
                ITD_pending_7days = sum(is_itd_more_7days, na.rm = TRUE)
              ), by = c("CountryCode" = "CountryCode")) |>
  rename(Numb_Isolates = ITD_results, Pending_Isolates = ITD_pending_7days) |> 
  ungroup() |>
  gt() |>
  tab_header(
    title = "AFP Table"
  ) |>
  #edit some columns names
  cols_label(
    Numb_Isolates = "Number of Isolates",
    PV2 = "nOPV2-",
    nOPV2 = "nOPV2+",
    Pending_Isolates = "Pending Isolates >7 days"
  ) |>
  grand_summary_rows(
    columns = !CountryCode,
    missing_text = "",
    fns = list(
      "TOTAL" = ~ sum(.x, na.rm = TRUE)
    ) )  |>
  # customize the summary line
  tab_style(
    style = list(
      cell_text(weight = "bold", size = px(14))  # Bold and increase font size
    ),
    locations = cells_grand_summary()
  )  |>
  #center the values in the defined columns
  cols_align(
    align = "center",
    columns = everything()
  ) |>
  sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text = 0
  )  |>
  # tab_spanner(
  #   label = md('**Sabin Like PV**'),
  #   columns = 3:4) |>
  # tab_spanner(
  #   label = md('**Discordant**'),
  #   columns = 5:6) |>
  # tab_spanner(
  #   label = md('**PV2**'),
  #   columns = 7:8) |>
  #give a header to the table as well as a sub title
  tab_header(
    title = md(paste0("**INTRATYPIC DIFFERENTIATION (ITD 7 DAYS) OF AFP ISOLATES** ")),
    subtitle = md(paste0("**",Specify_the_period,"**")) ) |>
  data_color(
    #method = "numeric",
    columns = Pending_Isolates,
    rows = Pending_Isolates >= 0,
    palette = "ggsci::red_material"
  ) |>
  # Color in gray the table and beautify the formating
  opt_stylize(style = 6, color = 'gray') |>
  #gt_theme_excel()  gt_theme_pff() 
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

AFPtables_gt

# export 
AFPtables_gt |> gtsave(filename = "../data/outputs_lab_ass/AFPtables_gt.html", inline_css = TRUE)
AFPtables_gt |> gtsave("../data/outputs_lab_ass/AFPtables_gt.png")




















