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

