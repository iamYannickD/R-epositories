#check directory and assign it to a path
path <- getwd()

# Check if the package pacman is installed (pacman Install and load multiple desired packages at once)
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

#install library to import geojson, ggspatial enable R to read and manipulate geojson spatial features
#and ggrepel allows to edit the labels and avoid overlaps
p_load(tidyverse, sf, geojsonsf, ggspatial, ggrepel)

#load data

# Classification of countries based on their level of risk
Very_high_risk <- c("Chad", "Democratic Republic of Congo", "Madagascar", "Mozambique", "Niger", "Nigeria")

High_risk	<- c("Algeria", "Angola", "Benin", "Burkina Faso", "Cameroon", "Central African Republic", "Côte d’Ivoire", 
               "Ethiopia", "Kenya", "Malawi", "Mali", "Zambia")

Medium_high_risk <- c("Burundi", "Republic of Congo", "Equatorial Guinea", "Eritrea", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea Bissau", 
                      "Liberia", "Mauritania", "Namibia", "Rwanda", "Senegal", "Sierra Leone", "South Sudan", "Tanzania", 
                      "Togo", "Uganda", "Zimbabwe")
Medium_Risk <- c("")

# Create a tibble containing values in the list provided and their risk category
risk_level_by_country <- tibble(
  risk_level = c(rep(" Very High Risk", length(Very_high_risk)),
                 rep("High Risk", length(High_risk)),
                 rep("Medium High Risk", length(Medium_high_risk)),
                 rep("Medium Risk", length(Medium_Risk))),
  country = c(Very_high_risk, High_risk, Medium_high_risk, Medium_Risk) |> str_to_upper()
) |>
  mutate(
    country = str_replace_all(country, c("CHAD" = "TCHAD",
                                         "UNITED REPUBLIC OF TANZANIA" = "TANZANIA",
                                         "CÔTE D’IVOIRE" = "COTE D'IVOIRE",
                                         "GUINEA"  = "GUINEE",
                                         "MAURITANIA" = "MAURITANIE", 
                                         "EQUATORIAL GUINEE" = "EQUATORIAL GUINEA" ))
  )

#load_es_sites <- 
  read_csv("../data/data_dr/es_sites/ES_site_analysis_jan_jun_2024-07-09.csv") |>
  dplyr::select(Countryname, EV_isolation_Rate_3m, EV_isolation_Rate_6m) |>
    left_join(y = risk_level_by_country, by = c("Countryname" = "country")) |>
    filter(!is.na(risk_level)) |>
    group_by(risk_level, Countryname) |>
    mutate(
      country_3m = if_else(!is.na(EV_isolation_Rate_3m), 1, 0),
      country_3m_50ev = if_else( (!is.na(EV_isolation_Rate_3m) & EV_isolation_Rate_3m  >= 50) , 1, 0),
      country_3m_80ev = if_else( (!is.na(EV_isolation_Rate_3m) & EV_isolation_Rate_3m  >= 80), 1, 0),
      
      country_6m = if_else(!is.na(EV_isolation_Rate_6m), 1, 0),
      country_6m_50ev = if_else( (!is.na(EV_isolation_Rate_6m) & EV_isolation_Rate_6m  >= 50) , 1, 0),
      country_6m_80ev = if_else( (!is.na(EV_isolation_Rate_6m) & EV_isolation_Rate_6m  >= 80), 1, 0),
    ) |>
    summarise(
      `%Q1countries >= 50` = 100 * sum(country_3m_50ev, na.rm = TRUE) / sum(country_3m, na.rm = TRUE),
      `%Q2countries >= 50` = 100 * sum(country_6m_50ev, na.rm = TRUE) / sum(country_6m, na.rm = TRUE),
      
      `%Q1countries >= 80` = 100 * sum(country_3m_80ev, na.rm = TRUE) / sum(country_3m, na.rm = TRUE),
      `%Q2countries >= 80` = 100 * sum(country_6m_80ev, na.rm = TRUE) / sum(country_6m, na.rm = TRUE)
    ) |>
    arrange(risk_level, Countryname) |>
    gt(groupname_col = 'risk_level', rowname_col = 'Countryname') |>
    #edit some columns names
    cols_label(
      "%Q1countries >= 50" = "EV > 50, Q1, 2024",
      "%Q2countries >= 50" = "EV > 50, Q2, 2024",
      "%Q1countries >= 80" = "EV > 80, Q1, 2024",
      "%Q2countries >= 80" = "EV > 80, Q2, 2024"
    ) |>
    #center the values in the defined columns
    cols_align(
      align = "center",
      columns = c(`%Q1countries >= 50`, `%Q2countries >= 50`, `%Q1countries >= 80`,
                  `%Q2countries >= 80`)
    ) |> 
    # add percentage in cells
    fmt_number(
      columns = c(`%Q1countries >= 50`, `%Q2countries >= 50`, `%Q1countries >= 80`, `%Q2countries >= 80`),
      decimals = 0,
      pattern = "{x} %"
    ) |>
    #add the title that covers the columns in the 7th and 8th row
    tab_spanner(
      label = md('**% of ES sites with ≥50% EV isolation**'),
      columns = 2:4) |>
    tab_spanner(
      label = md('**% of ES sites with ≥80% EV isolation**'),
      columns = 5:6) 
  
  

