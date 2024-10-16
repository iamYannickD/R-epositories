#check directory
#path <- getwd()

# Check if the package pacman is installed (pacman Install and load multiple desired packages at once)
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

#install library tidyverse, grammar of tables (gt) and gtExtras to have more options from the gt library
#webshot2 is to save gt tables as an images files saved from an html file
p_load(tidyverse, gt, gtExtras, webshot2)

#load dataset
#es_sites <- read_csv("data/dataset_desk_review/Location ES Sites.csv") 
Load_es_sites <- read.csv2("../data/data_dr/es_sites/ES_site_analysis_jul_sept_2024.csv", sep = ",", encoding="UTF-8")

# Classification of countries based on their level of risk
Very_high_risk <- c("Chad", "Democratic Republic of the Congo", "Madagascar", "Mozambique", "Niger", "Nigeria")

High_risk	<- c("Algeria", "Angola", "Benin", "Burkina Faso", "Cameroon", "Central African Republic", "Côte d’Ivoire", 
               "Ethiopia", "Kenya", "Malawi", "Mali", "Zambia")

Medium_high_risk <- c("Burundi", "Republic of Congo", "Equatorial Guinea", "Eritrea", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea Bissau", 
                      "Liberia", "Mauritania", "Namibia", "Rwanda", "Senegal", "Sierra Leone", "South Sudan", "United Republic of Tanzania", 
                      "Togo", "Uganda", "Zimbabwe")
Medium_Risk <- c("")

str(Load_es_sites) # Display the Structure of the Object

# Create a data frame or tibble containing values in the list provided
risk_level_by_country <- tibble(
  risk_level = c(rep("Very_high_risk", length(Very_high_risk)),
                 rep("High_risk", length(High_risk)),
                 rep("Medium_high_risk", length(Medium_high_risk)),
                 rep("Medium_Risk", length(Medium_Risk))),
  country = c(Very_high_risk, High_risk, Medium_high_risk, Medium_Risk) |> str_to_upper()
      ) |>
  mutate(
    country = str_replace_all(country, c("CHAD" = "TCHAD",
                                         "UNITED REPUBLIC OF TANZANIA" = "TANZANIA",
                                         "CÔTE D’IVOIRE" = "COTE D'IVOIRE",
                                         "GUINEA"  = "GUINEE",
                                         "MAURITANIA" = "MAURITANIE", 
                                         "EQUATORIAL GUINEE" = "EQUATORIAL GUINEA",
                                         "DEMOCRATIC REPUBLIC OF CONGO" = "Democratic Republic of The Congo"))
  )

# restructuring of the excel file
es_sites <-
  Load_es_sites |>
  as_tibble()  |>  
  left_join(y = risk_level_by_country, by = c("Countryname" = "country"))

risk_status <- 
  es_sites$risk_level |> 
  #replace(is.na(es_sites$risk_level), "Medium_Risk") |>
  unique()

# risk <- "Very_high_risk"

for (risk in risk_status) {
  #generate a table suited for the desk review
  table_es_sites <-
    #select (es_sites, Country, Province, District, Sitename, "# Samples received in the lab", "EV Isolation rate", "% Samples arrived in lab in good", "% samples <3 days shipment" , 
    #"Average # days samples arrive the lab" ) |>
    dplyr::select(es_sites, COUNTRY = Countryname, PROVINCE = Province, DISTRICT = District, Sitename, 
            "# Samples received in the lab" = starts_with("Num_Samples")[1], "EV Isolation rate" = starts_with("EV_isolation_Rate"), 
            "% Samples arrived in lab in good" = starts_with("prop_samples_good_condition"), 
            "% samples <3 days shipment" = starts_with("prop_samples_within_3day"), 
            "Average # days samples arrive the lab" = starts_with("median_days_to_lab"), risk_level
    ) |>
    filter(!is.na(COUNTRY)) |>
    filter(!is.na(`# Samples received in the lab`)) |>
    filter(risk_level == risk) |>
    dplyr::select (!risk_level) |>
    #add country to easily differentiate the columns and convert values to numeric values and to go to 100%
    mutate(COUNTRY = paste0("Country : ", COUNTRY)) |>
    # sort by province
    arrange(desc(PROVINCE)) |> 
    #group columns by country and ask to start straight at the province
    gt(groupname_col = 'COUNTRY', rowname_col = 'PROVINCE') |>

    #edit some columns names
    cols_label(
      "% Samples arrived in lab in good" = "% samples in good conditions",
      "% samples <3 days shipment" = "% samples in lab in <3 days",
      "Average # days samples arrive the lab" = "# days to reach the lab",
      "EV Isolation rate" = "EV Isolation rate (in %)"
    ) |>
    #center the values in the defined columns
    cols_align(
      align = "center",
      columns = c(`# Samples received in the lab`, `EV Isolation rate`, `% Samples arrived in lab in good`,
                  `% samples <3 days shipment`, `Average # days samples arrive the lab`)
    ) |> 
    # add percentage in cells
    fmt_number(
      columns = c(`EV Isolation rate`, `% Samples arrived in lab in good`, `% samples <3 days shipment`),
      decimals = 1,
      pattern = "{x} %"
    ) |>
    #add the title that covers the columns in the 7th and 8th row
    tab_spanner(
      label = md('**% Samples**'),
      columns = 7:8) |>
    #give a header to the table as well as a sub title
    tab_header(
      title = md(paste0("**ES Performance by Site during the past 12 months for** ", risk, " **Countries**")),
      subtitle = md("**Data source : AFRO ES Database**") ) |>
    #color the table based on the values in those cells
    tab_style(
      style = cell_fill(color = "#00B050"),
      locations = cells_body(
        columns = `EV Isolation rate`,
        rows = `EV Isolation rate` >= 50)
    )  |> 
    tab_style(
      style = cell_fill(color = "yellow"),
      locations = cells_body(
        columns = `EV Isolation rate`,
        rows = `EV Isolation rate` >= 25 & `EV Isolation rate` < 50)
    ) |>
    tab_style(
      style = cell_fill(color = "#FF0000"),
      locations = cells_body(
        columns = `EV Isolation rate`,
        rows = `EV Isolation rate` >= 0 & `EV Isolation rate` < 25)
    ) |> 
    tab_style(
      style = cell_fill(color = "#00B050"),
      locations = cells_body(
        columns = `EV Isolation rate`,
        rows = `EV Isolation rate` == 100)
    ) |>
    # Color in gray the table and beautify the formating
    opt_stylize(style = 6, color = 'gray') |>
    #call that theme
    #other themes gt_theme_excel()  |>gt_theme_pff() |>
    gt_theme_espn() |> 
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
  
  # Create the directory if it doesn't exist
  if (!file.exists( paste0("../data/data_dr/outputs/Table/", risk))) {
    dir.create(paste0("../data/data_dr/outputs/Table/", risk) , recursive = TRUE)
  }
  
  #save as an html file
  gtsave(table_es_sites, paste0("../data/data_dr/outputs/Table/", risk, "/table_es_desk_review.html"), inline_css = TRUE)
  
  #save as a png file
  gtsave(table_es_sites, paste0("../data/data_dr/outputs/Table/", risk, "/table_es_desk_review.png"), expand = 10
         , vheigh = 14)
  
  #save as a word document
  gtsave(table_es_sites, paste0("../data/data_dr/outputs/Table/", risk, "/table_es_desk_review.docx"))
  
}




