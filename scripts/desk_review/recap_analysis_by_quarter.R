#check directory and assign it to a path
path <- getwd()

# Check if the package pacman is installed (pacman Install and load multiple desired packages at once)
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

#install library to import geojson, ggspatial enable R to read and manipulate geojson spatial features
#and ggrepel allows to edit the labels and avoid overlaps
p_load(tidyverse, sf, geojsonsf, ggspatial, ggrepel)

#load data
load_es_sites <- read_csv("../data/data_dr/es_sites/ES_site_analysis_apr_jun_2024-07-09.csv") |>