# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages =====
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
p_load(tidyverse, RODBC,gt, gtExtras, webshot, openxlsx)

#Give the path to the AFP database
path_AFP <- "../data/dbs/AFP_160724.mdb" 
labname <- "CAE" # Replace with the actual labname you want to filter by
#dateDBreceived <- "2024-06-18 14:24:05"

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
                             (epiweek(as.Date(ymd(AFPtables$DateUpdated))) - 1) |> unique(), ", 2024")

# Analysis of databases =====
AFPkpis <-
  AFPtables |>
  filter(LabName == labname) |>
  mutate( FinalCellCultureResult = str_replace_all(FinalCellCultureResult, "Supected", "Suspected"),
          CountryCode = substr(EpidNumber, start = 1, stop = 3), .after = LabName) |>
  select(CountryCode, ICLabID, DateStoolReceivedinLab, FinalCellCultureResult, DateFinalCellCultureResults, DateUpdated) |>
  distinct(ICLabID, .keep_all = "TRUE") |>
  group_by(CountryCode) |>
  mutate(is_result = if_else(!is.na(FinalCellCultureResult), 1, 0),
         is_pv_positive = if_else((FinalCellCultureResult == "1-Suspected Poliovirus"), 1, 0),
         is_pv_positive_and_npent = if_else((FinalCellCultureResult == "4-Suspected Poliovirus + NPENT"), 1, 0),
         is_pv_negative = if_else((FinalCellCultureResult == "2-Negative"), 1, 0),
         is_npent = if_else((FinalCellCultureResult == "3-NPENT"), 1, 0),
         
         # cell culture < 14 days
         time_culture_results = as.numeric(difftime(DateFinalCellCultureResults, DateStoolReceivedinLab, units = "days")),
         is_culture_results_14days = if_else( (!is.na(FinalCellCultureResult) & time_culture_results < 15 & time_culture_results > 0), 1, 0),
         
         culture_results = sum(is_result),
         culture_results_14days = sum(is_culture_results_14days),
         
         # use a generic data based on the date the database was received
         #DateUpdated = if_else(is.na(DateUpdated), as.POSIXct(dateDBreceived), as.Date(ymd_hms(DateUpdated))),
         DateUpdated = as.Date(ymd(DateUpdated)),
         
         DateStoolReceivedinLab = as.Date(ymd(DateStoolReceivedinLab)),
         #DateUpdated = as.Date(ymd_hms(DateUpdated)),
         days_to_results = as.numeric(difftime(DateUpdated, DateStoolReceivedinLab, units = "days")),
         is_pending_culture_results = if_else(is.na(FinalCellCultureResult) & days_to_results >= 15, 1, 0),
         is_pending_culture_15_30 = if_else(is.na(FinalCellCultureResult) & (days_to_results >= 15 & days_to_results < 30), 1, 0),
         is_pending_culture_30_60 = if_else(is.na(FinalCellCultureResult) & (days_to_results >= 30 & days_to_results < 60), 1, 0),
         is_pending_culture_more_60 = if_else(is.na(FinalCellCultureResult) & days_to_results >= 60, 1, 0)
  ) |> 
  summarise(
    samples_with_results = sum(is_result, na.rm = TRUE),
    
    culture_results_14days = sum(is_culture_results_14days, na.rm = TRUE),
    Prop_culture_results_14days = 100 * culture_results_14days / samples_with_results,
    
    pv_positive = sum(is_pv_positive, na.rm = TRUE),
    prop_pv_positive = 100 * pv_positive / samples_with_results,
    
    pv_positive_and_npent = sum(is_pv_positive_and_npent, na.rm = TRUE),
    prop_pv_positive_and_npent = 100 * pv_positive_and_npent / samples_with_results,
    
    npent = sum(is_npent, na.rm = TRUE), 
    prop_npent = 100 * npent / samples_with_results,
    
    pv_negative = sum(is_pv_negative, na.rm = TRUE),
    prop_negative = 100 * pv_negative / samples_with_results,
    
    pending_culture_results = sum(is_pending_culture_results),
    pending_culture_15_30 = sum(is_pending_culture_15_30),
    pending_culture_30_60 = sum(is_pending_culture_30_60),
    pending_culture_more_60 = sum(is_pending_culture_more_60)
  ) |>
  ungroup() |>
  gt() |>
  #edit some columns names
  cols_label(
    "samples_with_results" = "# of Specimens with Results",
    "culture_results_14days" = "# of culture results in 14 days",
    "Prop_culture_results_14days" = "Prop cell cultures results 14 days",
    "pv_positive" = "Culture +ve for PV",
    "prop_pv_positive" = "Prop Culture +ve for PV",
    "pv_positive_and_npent" = "Culture +ve for PV & NPEV",
    "prop_pv_positive_and_npent" = "Prop Culture +ve for PV & NPEV",
    "npent" = "NPEV",
    "prop_npent" = "Prop NPEV",
    "pv_negative" = "Negative",
    "prop_negative" = "Prop Negative",
    "pending_culture_results" = "Total Pending",
    "pending_culture_15_30" = "15 - 30 days",
    "pending_culture_30_60" = "31 - 60 days",
    "pending_culture_more_60" = "> 60 days"
  ) |>
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
    title = md(paste0("**AFP : Timeliness and Results of Primary Isolation (14 days)** ")),
    subtitle = md(paste0("**",Specify_the_period,"**")) ) |>
  sub_missing(
    columns = 2:16,
    rows = everything(),
    missing_text = 0
    #missing_text = "---"
  ) |>
  # add percentage in cells
  fmt_number(
    columns = c(1:16),
    decimals = 0,
    pattern = "{x}"
  ) |>
  fmt_number(
    columns = c(Prop_culture_results_14days, prop_pv_positive, prop_pv_positive_and_npent, prop_npent, prop_negative ),
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
  #other themes 
  #gt_theme_excel()  |> gt_theme_pff() |> 
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


# export my table
gtsave(AFPkpis, "../data/outputs/AFPKpis.html")
# Convert HTML to PNG
webshot::webshot("../data/output/AFPtables.html", "../data/output/AFPKpis.png")

# convert html to excel file
#write.xlsx(AFPkpis_df, "output/AFPKpis.xlsx")


# open the table of our presentation change link ppt
Pres_ppt <- read_pptx(path = "../data/AFRO polio labs bulletin week 1-18_2024.pptx")

# Insert the image in a new slide
#Pres_ppt <- ph_with(on_slide(Pres_ppt, index = 3), external_img("output/AFPtables.png"), location = ph_location_fullsize())

# add the table in the 4th slide of the presentation
Pres_ppt <- ph_with(on_slide(Pres_ppt, index = 9), external_img("output/AFPKpis.png"), 
                    location = ph_location(left = 0.2, top = 1, width = 13, height = 8))

# Save the updated presentation
print(Pres_ppt, target = "data/AFRO polio labs bulletin week 1-18_2024.pptx")  





