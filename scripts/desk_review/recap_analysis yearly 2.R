# Check if the package pacman is installed (pacman Install and load multiple desired packages at once)
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

#load libraries
p_load(tidyverse, gt, gtExtras)

# load data
es_sites <- read_csv("../data/data_dr/es_sites/ES_performance_from_2024-01-01_to_2024-12-31 Cumul_v2.csv") 

# Classification of countries based on their level of risk
Very_high_risk <- c("Chad", "Democratic Republic of Congo", "Madagascar", "Mozambique", "Niger", "Nigeria")

High_risk	<- c("Algeria", "Angola", "Benin", "Burkina Faso", "Cameroon", "Central African Republic", "Cote dIvoire", 
               "Ethiopia", "Kenya", "Malawi", "Mali", "Zambia")

Medium_high_risk <- c("Burundi", "Republic of Congo", "Equatorial Guinea", "Eritrea", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea Bissau", 
                      "Liberia", "Mauritania", "Namibia", "Rwanda", "Senegal", "Sierra Leone", "South Sudan", "Tanzania", 
                      "Togo", "Uganda", "Zimbabwe")

Medium_Risk <- c("Botswana", "Cabo Verde", "Eswatini", "Lesotho", "Mauritius", "Mauritius", "Seychelles", "South Africa")

# Color Categories
breaks <- c(0, 25, 50, 100)
colors <- c("red", "yellow", "darkgreen")

# Create a tibble containing values in the list provided and their risk category
risk_level_by_country <- tibble(
  risk_level = c(rep(" Very High Risk", length(Very_high_risk)),
                 rep("High Risk", length(High_risk)),
                 rep("Medium High Risk", length(Medium_high_risk)),
                 rep("Medium Risk", length(Medium_Risk))),
  country = c(Very_high_risk, High_risk, Medium_high_risk, Medium_Risk) |> str_to_upper()
    ) |>
  mutate(
    country = str_replace_all(country, c("UNITED REPUBLIC OF TANZANIA" = "TANZANIA",
                                         "CÔTE D’IVOIRE" = "COTE D'IVOIRE",
                                         "GUINEA"  = "GUINEE",
                                         #"MAURITANIA" = "MAURITANIE", 
                                         "EQUATORIAL GUINEE" = "EQUATORIAL GUINEA" ))
  )

recap_es_sites <- 
  es_sites |>
  dplyr::select(Countryname = Country, EV_isolation_Rate_3m, EV_isolation_Rate_6m, EV_isolation_Rate_9m, EV_isolation_Rate_12m) |>
    left_join(y = risk_level_by_country, by = c("Countryname" = "country")) |>
    filter(!is.na(risk_level)) |>
    group_by(risk_level, Countryname) |>
    mutate(
      country_3m = if_else(!is.na(EV_isolation_Rate_3m), 1, 0),
      country_3m_50ev = if_else( (!is.na(EV_isolation_Rate_3m) & EV_isolation_Rate_3m  >= 0.5) , 1, 0),
      country_3m_80ev = if_else( (!is.na(EV_isolation_Rate_3m) & EV_isolation_Rate_3m  >= 0.8), 1, 0),
      
      country_6m = if_else(!is.na(EV_isolation_Rate_6m), 1, 0),
      country_6m_50ev = if_else( (!is.na(EV_isolation_Rate_6m) & EV_isolation_Rate_6m  >= 0.5) , 1, 0),
      country_6m_80ev = if_else( (!is.na(EV_isolation_Rate_6m) & EV_isolation_Rate_6m  >= 0.8), 1, 0),
      
      country_9m = if_else(!is.na(EV_isolation_Rate_9m), 1, 0),
      country_9m_50ev = if_else( (!is.na(EV_isolation_Rate_9m) & EV_isolation_Rate_9m  >= 0.5) , 1, 0),
      country_9m_80ev = if_else( (!is.na(EV_isolation_Rate_9m) & EV_isolation_Rate_9m  >= 0.8), 1, 0),
      
      country_12m = if_else(!is.na(EV_isolation_Rate_12m), 1, 0),
      country_12m_50ev = if_else( (!is.na(EV_isolation_Rate_12m) & EV_isolation_Rate_12m  >= 0.5) , 1, 0),
      country_12m_80ev = if_else( (!is.na(EV_isolation_Rate_12m) & EV_isolation_Rate_12m  >= 0.8), 1, 0)
    ) |>
    summarise(
      `%Q4countries >= 50` = 100 * sum(country_12m_50ev, na.rm = TRUE) / sum(country_12m, na.rm = TRUE),
      `%Q3countries >= 50` = 100 * sum(country_9m_50ev, na.rm = TRUE) / sum(country_9m, na.rm = TRUE),
      `%Q2countries >= 50` = 100 * sum(country_6m_50ev, na.rm = TRUE) / sum(country_6m, na.rm = TRUE),
      `%Q1countries >= 50` = 100 * sum(country_3m_50ev, na.rm = TRUE) / sum(country_3m, na.rm = TRUE),
      
      `%Q4countries >= 80` = 100 * sum(country_12m_80ev, na.rm = TRUE) / sum(country_12m, na.rm = TRUE),
      `%Q3countries >= 80` = 100 * sum(country_9m_80ev, na.rm = TRUE) / sum(country_9m, na.rm = TRUE),
      `%Q2countries >= 80` = 100 * sum(country_6m_80ev, na.rm = TRUE) / sum(country_6m, na.rm = TRUE),
      `%Q1countries >= 80` = 100 * sum(country_3m_80ev, na.rm = TRUE) / sum(country_3m, na.rm = TRUE),
    ) |>
    mutate(
      #sparkline_50 = map2( `%Q1countries >= 50`, `%Q2countries >= 50`, ~ list(c(.x, .y))),
          sparkline_50 = map2( `%Q1countries >= 50`, `%Q2countries >= 50`, `%Q3countries >= 50`, `%Q4countries >= 50`, ~ list(c(.x, .y))),
          sparkline_80 = map2( `%Q1countries >= 80`, `%Q2countries >= 80`, `%Q3countries >= 80`, `%Q4countries >= 80`, ~ list(c(.x, .y)))
          
      #sparkline_80 = map2_dbl( `%Q1countries >= 80`, `%Q2countries >= 80`, ~ list(c(.x, .y)))
      #sparkline_80 = pmap(list(`%Q1countries >= 80`, `%Q2countries >= 80`), ~ c(..1, ..2))
          #sparkline_80 = pmap(list(`%Q1countries >= 80`, `%Q2countries >= 80`), `%Q3countries >= 80`, ~ c(..1, ..2))
      #sparkline_50 = as.list(paste0( round(`%Q1countries >= 50`, digits = 0), ", ", round(`%Q2countries >= 50`, digits = 0))),
      #sparkline_80 = as.list(paste0( round(`%Q1countries >= 80`, digits = 0), ", ", round(`%Q2countries >= 80`, digits = 0)))
    ) |>
    arrange(risk_level, Countryname) |>
    gt(groupname_col = 'risk_level', rowname_col = 'Countryname') |>
    #gt_color_rows(2:6, palette = c("darkred", "red", "yellow", "green", "darkgreen")) |> # domain = c(0, 0.50), plt =  "RdYlGn"
    #edit some columns names
    cols_label(
      "%Q1countries >= 50" = "Q4, 2024",
      "%Q2countries >= 50" = "Q3, 2024",
      "%Q3countries >= 50" = "Q2, 2024",
      "%Q4countries >= 50" = "Q1, 2024",
      "%Q1countries >= 80" = "Q4, 2024",
      "%Q2countries >= 80" = "Q3, 2024",
      "%Q3countries >= 80" = "Q2, 2024",
      "%Q4countries >= 80" = "Q1, 2024",
      sparkline_50 = "EV Isolation 50% Trend",
      sparkline_80 = "EV Isolation 80% Trend"
    ) |>
    