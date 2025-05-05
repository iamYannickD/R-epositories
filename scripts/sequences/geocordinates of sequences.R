# Check if the package pacman is installed (pacman Install and load multiple desired packages at once)
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

#install library to import geojson, ggspatial enable R to read and manipulate geojson spatial features
#raster is for geographic data analysis, in this case to generate a random point within the district polygon
# zoo package support the formating of quaterly data as 2024 Q2 (quarters)
p_load(tidyverse, sf, readxl, geojsonsf, ggspatial, ggrepel, raster, zoo)

# load dataset
SEQUENCES <- read_csv("../data/data-polio-statistics/Administrative zones.csv")

# load administrative boundaries =====
afro_Adm0 <- read_rds("../data/global.ctry.rds") |> 
      filter(`WHO_REGION` == "AFRO" | 
           ADM0_NAME %in% c("EGYPT", "MOROCCO", "TUNISIA", "WESTERN SAHARA", "SUDAN", "LIBYA", "SOMALIA", "DJIBOUTI"))

# afro_cntries <- read_rds("../data/global.ctry.rds") |>
#       filter(ADM0_NAME == cntry)

afro_Adm1 <- read_rds("../data/global.prov.rds") |>
      filter(yr.end > 2015)

afro_Adm2 <- read_rds("../data/global.dist.rds") |> 
      filter(`WHO_REGION` == "AFRO" | ADM0_NAME %in% c("EGYPT", "SUDAN")) |>
      filter(yr.end > 2015) 

# 3. Join and validate
#    Replace "District" below with the actual column names in your datasets
afro_Adm2_validated <- SEQUENCES |>
  left_join(afro_Adm2 ,
    by = c("COUNTRYNAME" = "ADM0_NAME", "PROVINCENAME" = "ADM1_NAME", "DISTRICTCODE" = "ADM2_ALTCODE") ) |> tibble()

# 4. Export the validated table
write_csv(
  afro_Adm2_validated,
  "../data/data-polio-statistics/afro_Adm2_validated.csv"
)


write_csv(
  afro_Adm2,
  "../data/data-polio-statistics/admin2.csv"
)
