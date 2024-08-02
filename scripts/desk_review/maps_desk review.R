#check directory and assign it to a path
#path <- getwd()

# Check if the package pacman is installed (pacman Install and load multiple desired packages at once)
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

#install library to import geojson, ggspatial enable R to read and manipulate geojson spatial features
#and ggrepel allows to edit the labels and avoid overlaps
p_load(tidyverse, sf, geojsonsf, ggspatial, ggrepel)

#load data
load_es_sites <- read_csv("../data/data_dr/es_sites/ES_site_analysis_apr_jun_2024-07-09.csv") |>
  # filter only samples with results 
  filter(if_any(starts_with("EV_isolation_Rate"), ~ !is.na(.))) |>
  # as the end will change, i select columns that starts with a specific string
  dplyr::select(Country = Countryname, Sitename, ev_rate = starts_with("EV_isolation_Rate"), Lat_Y, Long_X) |>
  # filter out null coordinates
  filter( (!is.na(Lat_Y) & !is.na(Long_X)) & (!is.null(Lat_Y) & !is.null(Long_X)) & (Lat_Y != 0 & Long_X != 0)) |>
  mutate(
    Country = str_replace_all(Country, c("DEMOCRATIC REPUBLIC OF CONGO" = "Democratic Republic of The Congo",
                                         "REPUBLIC OF CONGO" = "CONGO",
                                         "GUINEE" = "GUINEA",
                                         "MAURITANIE" = "MAURITANIA",
                                         "TANZANIA" = "United Republic of Tanzania",
                                         "COTE D'IVOIRE" = "Côte dIvoire",
                                         "ESWATINI" = "SWAZILAND",
                                         "TCHAD" = "CHAD"
                                         ))
      ) |>
  filter(Sitename != "KAKOBA SEWAGE TREATMENT PLANT")

# Classification of countries based on their level of risk
Very_high_risk <- c("Chad", "Democratic Republic of the Congo", "Madagascar", "Mozambique", "Niger", "Nigeria")

High_risk	<- c("Algeria", "Angola", "Benin", "Burkina Faso", "Cameroon", "Central African Republic", "Côte dIvoire", 
               "Ethiopia", "Kenya", "Malawi", "Mali", "Zambia")

Medium_high_risk <- c("Burundi", "Congo", "Equatorial Guinea", "Eritrea", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea Bissau", 
                      "Liberia", "Mauritania", "Namibia", "Rwanda", "Senegal", "Sierra Leone", "South Sudan", "United Republic of Tanzania", 
                      "Togo", "Uganda", "Zimbabwe")

# load administrative boundaries
admin0 <- geojsonsf::geojson_sf("../data/data_dr/sf/admin0_geo.geojson") |>
  mutate(ADM0_VIZ_N = str_replace_all(ADM0_VIZ_N, "Côte d'Ivoire", "Côte dIvoire"))
admin1 <- geojsonsf::geojson_sf("../data/data_dr/sf/admin1_geo.geojson")  |>
  mutate(ADM0_VIZ_N = str_replace_all(ADM0_VIZ_N, "Côte d'Ivoire", "Côte dIvoire"))
load_afro_pop <- geojsonsf::geojson_sf("../data/data_dr/sf/afro_pop.geojson")  |>
  mutate(ADM0_VIZ_N = str_replace_all(ADM0_VIZ_N, "Côte d'Ivoire", "Côte dIvoire"))

#create a column to categorize the EV Rate and factor, meaning it to convert the values 0,1,2 (double) to
#factor to easily make a series of discrete values
es_sites <- load_es_sites |>
  mutate(ev_rate = 
           #categorizes based on the ev rates
           case_when(
             ev_rate < 25 ~ 0,
             (ev_rate >= 25 & ev_rate < 50) ~ 1,
             (ev_rate >= 50 ) ~ 2
           )
  ) |> 
  #convert the categories (double) into factors for the classification
  mutate(ev_rate = factor(ev_rate, 
                          labels = c("< 25", "25 - 49", ">= 50"))) |>
  group_by(Country, ev_rate) |>
  add_count(ev_rate) |>
  mutate(n = paste0(ev_rate, " (n = ", n, ")")) 

#creating the column cat_pop and making that column a serie of discrete values with factor
#then assigning categories to those values, that will be integer. note that case_label does nothing
afro_pop <- load_afro_pop |>
  mutate(cat_pop = 
           case_when(
             #categorizes based on the population under 15
             Pop15 < 25000 ~ 0,
             (Pop15 >= 25000 & Pop15 < 50000) ~ 1,
             (Pop15 >= 50000 & Pop15 < 100000) ~ 2,
             (Pop15 >= 100000 & Pop15 < 500000) ~ 3,
             Pop15 >= 500000 ~ 4) ) |> 
  mutate(cat_pop = factor(cat_pop, 
                          labels = c("< 25,000", "25,000 - 50,000", 
                                     "50,000 - 100,000", "100,000 - 500,000", 
                                     "> 500,000"))) |>
  mutate(cat_label = 
           case_when(
             Pop15 < 25000 ~ "< 25,000",
             (Pop15 >= 25000 & Pop15 < 50000) ~ "25,000 - 50,000",
             (Pop15 >= 50000 & Pop15 < 100000) ~ "50,000 - 100,000",
             (Pop15 >= 100000 & Pop15 < 500000) ~ "100,000 - 500,000",
             Pop15 >= 500000 ~ "> 500,000",
           )
  )

#Create a 1 array vector that contains all the countries, it will serve in the for loop
country <- admin0$ADM0_VIZ_N |>
  unique() |> sort()

#indenting the initial value for the loop
#cntry = "South Sudan"

#Generates all the maps in the for loop
# Function to plot maps
plot_maps <- function(cntry, pop_by_country, admin1_by_country, admin_by_country, es_by_country, risk_level, path) {
  plot1 <- ggplot() +
    geom_sf(data = pop_by_country, aes(fill = cat_pop), color = NA) +
    geom_sf(data = admin1_by_country, fill = NA, color = "white", size = 1) +
    geom_sf(data = admin_by_country, fill = NA, color = "black", size = 1) +
    geom_point(data = es_by_country, aes(x = Long_X, y = Lat_Y), color = "black", fill = "blue", size = 1) +
    geom_text_repel(data = es_by_country, aes(x = Long_X, y = Lat_Y, label = Sitename, fontface = "bold"),
                    label.r = 0.015, label.size = 0.01, color = "black", bg.color = "white", bg.r = 0.15, size = 2) +
    scale_fill_brewer(palette = "Reds", name = "Population < 15 Yrs") +
    labs(x = "Longitude", y = "Latitude", title = paste0("ES Site Locations and Population <15 yrs in ", cntry)) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  ggsave(paste0("../data/data_dr/outputs/ES_and_population/", risk_level, "/", cntry, "_pop.png"), plot1)
  
  plot2 <- ggplot() +
    geom_sf(data = admin1_by_country, fill = NA, color = "gray") +
    geom_sf(data = admin_by_country, fill = NA, color = "black", size = 1) +
    geom_point(data = es_by_country, aes(x = Long_X, y = Lat_Y, size = 5, color = ev_rate),
               size = 1.5, stroke = 1) +
    geom_text_repel(data = es_by_country, aes(x = Long_X, y = Lat_Y, label = Sitename, fontface = "bold"),
                    label.r = 0.015, label.size = 0.01, color = "black", bg.color = "white", bg.r = 0.15, size = 2) +
    scale_color_manual(values = c("< 25" = "red", "25 - 49" = "yellow", ">= 50" = "green"), 
                       name = "EV Rate", 
                       breaks = c("< 25", "25 - 49", ">= 50"),
                       labels = c(paste0("< 25 (n = ", sum(es_by_country$ev_rate == "< 25"), ")"), 
                                  paste0("25 - 49 (n = ", sum(es_by_country$ev_rate == "25 - 49"), ")"), 
                                  paste0(">= 50 (n = ", sum(es_by_country$ev_rate == ">= 50"), ")")),
    )  +
    labs(x = "Longitude", y = "Latitude", title = paste0("Map and performance of ES sites in ", cntry)) +
    theme_bw() #+
    #theme(legend.position = "bottom")
  
  ggsave(paste0("../data/data_dr/outputs/ES_sites/", risk_level, "/", cntry, ".png"), plot2)
}

# Loop through countries
for (cntry in country) {
  pop_by_country <- afro_pop |> filter(ADM0_VIZ_N == cntry) 
  admin_by_country <- admin0 |> filter(ADM0_VIZ_N == cntry)
  admin1_by_country <- admin1 |> filter(ADM0_VIZ_N == cntry) 
  es_by_country <- es_sites |> mutate(Country = str_to_title(Country)) |> filter(Country == str_to_title(cntry))
  
  if (cntry %in% Very_high_risk) {
    plot_maps(cntry, pop_by_country, admin1_by_country, admin_by_country, es_by_country, "Very_High_Risk")
  } else if (cntry %in% High_risk) {
    plot_maps(cntry, pop_by_country, admin1_by_country, admin_by_country, es_by_country, "High_Risk")
  } else if (cntry %in% Medium_high_risk) {
    plot_maps(cntry, pop_by_country, admin1_by_country, admin_by_country, es_by_country, "Medium_High_Risk")
  } else {
    plot_maps(cntry, pop_by_country, admin1_by_country, admin_by_country, es_by_country, "Medium_Risk")
  }
}

