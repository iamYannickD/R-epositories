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
  ) |> mutate(proxy_date_infor_itd = coalesce(DatefinalResultReported, DateFinalCombinedResult) )

# Analysis of databases =====
EStables2024 |>
  mutate(Labname = str_replace_all(Labname, "ESWATINI", "SOA" )) |>
  filter(Labname == labname) |>
  mutate(virus_cat = 
           case_when(
             str_detect(FinalcombinedrRTPCRresults, "nOPV2") ~ "nOPV2",
             (str_detect(FinalcombinedrRTPCRresults, "PV2") & !str_detect(FinalcombinedrRTPCRresults, "nOPV2")) ~ "PV2",
             str_detect(FinalcombinedrRTPCRresults, "PV1Discordant") ~ "Type 1 Discordant",
             str_detect(FinalcombinedrRTPCRresults, "PV3Discordant") ~ "Type 3 Discordant",
             
             (str_detect(FinalcombinedrRTPCRresults, "PV1SL") & !str_detect(FinalcombinedrRTPCRresults, "PV1 SL")) ~ "Sabin Type 1",
             str_detect(FinalcombinedrRTPCRresults, "PV3SL") ~ "Sabin Type 3",
             
             str_detect(FinalcombinedrRTPCRresults, "NPEV") ~ "NPEV",
             str_detect(FinalcombinedrRTPCRresults, "NEV") ~ "NEV",
             (is.na(FinalcombinedrRTPCRresults) | str_detect(FinalcombinedrRTPCRresults, "Invalid")) ~ NA,
             
             !is.na(FinalcombinedrRTPCRresults) ~ "check" # missed/unexpected results
           )) |>
  filter(!is.na(virus_cat)) |>
  group_by(Countrycode, virus_cat) |>
  summarise(n = n()) |>
  pivot_wider(names_from = virus_cat, values_from = n) |>
  left_join(EStables2024 |>
              filter(Labname == labname) |>
              select(Countrycode, Datesampleinlab, Samplecondition, Finalcellcultureresult, Datefinalcultureresult,
                     proxy_date_infor_itd, FinalcombinedrRTPCRresults, DateFinalCombinedResult) |>
              group_by(Countrycode) |>
              mutate(
                is_itd = if_else(Finalcellcultureresult %in% c("1-Suspected Poliovirus", "4-Suspected Poliovirus + NPENT"), 1, 0),
                
                time_itd_results_7days = as.numeric(difftime(DateUpdated, proxy_date_infor_itd, units = "days")),
                
                is_itd_more_7days = if_else(
                  Finalcellcultureresult %in% c("1-Suspected Poliovirus", "4-Suspected Poliovirus + NPENT") &
                    is.na(FinalcombinedrRTPCRresults) & time_itd_results_7days >= 8, 1, 0) ) |>
              summarize(
                ITD_results = sum(is_itd, na.rm = TRUE),
                ITD_pending_7days = sum(is_itd_more_7days, na.rm = TRUE)), 
                by = c("Countrycode" = "Countrycode")) |>
  rename(Numb_Isolates = ITD_results, Pending_Isolates = ITD_pending_7days) |>
  ungroup() |>
  gt() |>
  tab_header(
    title = "ES Table"
  ) |>
  #edit some columns names
  cols_label(
    Numb_Isolates = "Number of Isolates",
    PV2 = "nOPV2-",
    nOPV2 = "nOPV2+",
    Pending_Isolates = "Pending Isolates >7 days"
  ) |>
  grand_summary_rows(
    columns = !Countrycode,
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
  tab_spanner(
    label = md('**Sabin Like PV**'),
    columns = 3:4) |>
  tab_spanner(
    label = md('**Discordant**'),
    columns = 5:6) |>
  tab_spanner(
    label = md('**PV2**'),
    columns = 7:8) |>
  #give a header to the table as well as a sub title
  tab_header(
    title = md(paste0("**INTRATYPIC DIFFERENTIATION (ITD 7 DAYS) OF ES ISOLATES** ")),
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
