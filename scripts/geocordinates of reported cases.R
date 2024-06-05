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

# add geocordinates on afp samples and randomize if more than 1 sample was collected in the same district
#In this section, i generate a coordinate for all AFP cases based on their epid number (country code, province code, district code)
#then for each sample i have the geometry of the district were they are in, then i generate a random point inside that district polygon
#so that if more than a sample is collected in the same district, they are not on top of each other
afp_virus <- 
  viruses_isolated |>
  filter(SOURCE != "ENV") |>
  mutate(
    PROVINCE = str_replace_all(PROVINCE, "HAUT-KATANGA", "HAUT KATANGA"),
    DISTRICT = str_replace_all(DISTRICT, "TUDUN", "TUDUN WADA")
  ) |>
  left_join(y = afro_Adm2, by = c("COUNTRY" = "ADM0_NAME", "PROVINCE" = "ADM1_NAME", "DISTRICT" = "ADM2_NAME"),
            relationship = "many-to-many") |>
  dplyr::select(`EPID NUMBER`, VIRUS, SOURCE, COUNTRY, PROVINCE, DISTRICT, `ES SITE NAME`, `ONSET/ COLLECTION`,
                Lat_Y = CENTER_LAT, Long_X =  CENTER_LON, SHAPE)

# Function to generate random points within a polygon
generate_random_point <- function(polygon) {
  bbox <- as(extent(polygon), "SpatialPolygons")
  random_point <- spsample(bbox, 1, type = "random")
  return(random_point)
}

# Generate random points for each row
for (i in 1:nrow(afp_virus)) {
  district <- afp_virus$DISTRICT[i]
  district_polygon <- afro_Adm2 |>
    filter(afro_Adm2$ADM2_NAME == district) |>
    st_geometry() |>
    st_as_sf() 
  random_point <- generate_random_point(district_polygon)
  afp_virus$Long_X[i] <- random_point@coords[1]
  afp_virus$Lat_Y[i] <- random_point@coords[2]
}

# ES virus
es_virus <-
  viruses_isolated |>
  filter(SOURCE == "ENV") |>
  mutate(
    epid_match = str_sub(`EPID NUMBER`, 1, 19)
  ) |>
  mutate(
    epid_match = str_replace_all(epid_match, "ENV-ALG-TMR-TMR-RLV", "ENV-ALG-TAM-TAM-REL"),
    epid_match = str_replace_all(epid_match, "ENV-ANG-LUA-VIA-CFG", "ENV-ANG-LUA-VIA-CZG") 
  ) |>
  left_join(y = masterlist, by = c("epid_match" = "SITE_CODE")) |>
  dplyr::select(`EPID NUMBER`, VIRUS, SOURCE, COUNTRY = COUNTRY.x, PROVINCE = PROVINCE.x, 
                DISTRICT, `ES SITE NAME`,  `ONSET/ COLLECTION`, Lat_Y, Long_X) 

# merge the 2 viruses tables
virus <-
  bind_rows(afp_virus, es_virus)


epidemiology_map <- 
  ggplot(data = virus) +
  geom_sf(data = afro_Adm0, fill = "gray", color = "white") +
  geom_sf(data = afro_cntries, fill = "black", color = "white") +
  geom_point(shape = 16, size = 3, stroke = 2,  aes(x = Long_X, y = Lat_Y, color = VIRUS, fill = "black"), fill = "black") +
  scale_color_manual(values = c("cVDPV1" = "#F067A6", "cVDPV2" = "#2CBB9B")) +
  labs(x = "Longitude", y = "Latitude", color = "Virus", 
       title = "Geolocation of reported viruses") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")    # Center ggplot title and legend at bottom


# saving the plot as image png  
ggsave("Reported_cases.png", epidemiology_map, path = "output/")  #export population map + es sites













