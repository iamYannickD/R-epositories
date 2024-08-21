# Check if the package pacman is installed (pacman Install and load multiple desired packages at once)
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

#install library to import geojson, ggspatial enable R to read and manipulate geojson spatial features
#raster is for geographic data analysis, in this case to generate a random point within the district polygon
p_load(tidyverse, sf, readxl, geojsonsf, ggspatial, ggrepel, raster)

# load dataset
cntry <- "DEMOCRATIC REPUBLIC OF THE CONGO"
sequences_results <-
  read_excel("../data/data_sequences/sequences/RDC_cVDPV(n)2_Since2016_Final.xlsx")

# load administrative boundaries =====
afro_Adm0 <- read_rds("../data/global.ctry.rds") |> 
      filter(`WHO_REGION` == "AFRO" | 
           ADM0_NAME %in% c("EGYPT", "MOROCCO", "TUNISIA", "WESTERN SAHARA", "SUDAN", "LIBYA", "SOMALIA", "DJIBOUTI"))

afro_cntries <- read_rds("../data/global.ctry.rds") |>
      filter(`WHO_REGION` == "AFRO" & ADM0_NAME == cntry)

afro_Adm1 <- read_rds("../data/global.prov.rds") |>
      filter(WHO_REGION == "AFRO" & ADM0_NAME == cntry & yr.end > 2015)

afro_Adm2 <- read_rds("../data/global.dist.rds") |> 
      filter(WHO_REGION == "AFRO" & ADM0_NAME == cntry & yr.end > 2015)
