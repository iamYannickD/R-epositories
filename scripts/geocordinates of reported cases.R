# Check if the package pacman is installed (pacman Install and load multiple desired packages at once)
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

#install library to import geojson, ggspatial enable R to read and manipulate geojson spatial features
#and ggrepel allows to edit the labels and avoid overlaps
#raster is for geographic data analysis, in this case to generate a random point within the district polygon
p_load(tidyverse, sf, readxl, geojsonsf, ggspatial, ggrepel, raster)

#load dataset
viruses_isolated <- read_excel("../data/data/linelist_apr_2024.xlsx") |>
  mutate(
    COUNTRY = str_replace_all(COUNTRY, "DRC", "DEMOCRATIC REPUBLIC OF THE CONGO"),
    COUNTRY = str_replace_all(COUNTRY, "COTE D'IVOIRE", "COTE D IVOIRE")
  )

# read the master list
# Load the repository containing the links
es_repo <- read_csv("../data/link/access.txt")

# load masterlist
masterlist <- es_repo$lien[1] 

es_sites <-   read_csv(masterlist) 
                  # #|> filter(STATUS == "ACTIVE") 

# load administrative boundaries
# load administrative boundaries =====
afro_Adm0 <- 
  read_rds("../data/global.ctry.rds") |> 
  filter(`WHO_REGION` == "AFRO" | 
           ADM0_NAME %in% c("EGYPT", "MOROCCO", "TUNISIA", "WESTERN SAHARA", "SUDAN", "LIBYA", "SOMALIA", "DJIBOUTI"))

afro_cntries <- 
  read_rds("../data/global.ctry.rds") |>
  filter(WHO_REGION %in% "AFRO")

afro_Adm1 <- read_rds("../data/global.prov.rds") |>filter(WHO_REGION == "AFRO")
#afro_Adm1_alt <- geojsonsf::geojson_sf("../automated_desk_review/data/updates_boundaries/Detailed_Boundary_ADM1_-6016301328393585066.geojson")



afro_Adm2 <- read_rds("../data/global.dist.rds") |> 
  filter(WHO_REGION == "AFRO")














