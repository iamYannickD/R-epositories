#check directory and assign it to a path
path <- getwd()

# Check if the package pacman is installed (pacman Install and load multiple desired packages at once)
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

#install library to import geojson, ggspatial enable R to read and manipulate geojson spatial features
#and ggrepel allows to edit the labels and avoid overlaps
p_load(tidyverse, sf, geojsonsf, ggspatial, ggrepel)

#load data
#load_es_sites <- read_csv("data/dataset_desk_review/Location ES Sites.csv")
load_es_sites <- read_csv("data/data_q1/Copy of ES_site_analysis_2024-04-23.csv") |>
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
                                         "COTE D'IVOIRE" = "Côte d’Ivoire",
                                         "ESWATINI" = "SWAZILAND",
                                         "TCHAD" = "CHAD"
                                         ))
      ) |>
  filter(Sitename != "KAKOBA SEWAGE TREATMENT PLANT")

