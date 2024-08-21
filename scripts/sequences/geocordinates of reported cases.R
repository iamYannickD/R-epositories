# Check if the package pacman is installed (pacman Install and load multiple desired packages at once)
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

#install library to import geojson, ggspatial enable R to read and manipulate geojson spatial features
#raster is for geographic data analysis, in this case to generate a random point within the district polygon
p_load(tidyverse, sf, readxl, geojsonsf, ggspatial, ggrepel, raster)

# load dataset
cntry <- "DEMOCRATIC REPUBLIC OF THE CONGO"
cntry_code <- "RDC"
sequences_results <-
  read_excel("../data/data_sequences/sequences/RDC_cVDPV(n)2_Since2016_Final.xlsx")

# load administrative boundaries =====
afro_Adm0 <- read_rds("../data/global.ctry.rds") |> 
      filter(`WHO_REGION` == "AFRO" | 
           ADM0_NAME %in% c("EGYPT", "MOROCCO", "TUNISIA", "WESTERN SAHARA", "SUDAN", "LIBYA", "SOMALIA", "DJIBOUTI"))

afro_cntries <- read_rds("../data/global.ctry.rds") |>
      filter(ADM0_NAME == cntry)

afro_Adm1 <- read_rds("../data/global.prov.rds") |>
      filter(ADM0_NAME == cntry & yr.end > 2015)

afro_Adm2 <- read_rds("../data/global.dist.rds") |> 
      filter(ADM0_NAME == cntry & yr.end > 2015) 

# remove geometry from adm1
afro_Adm1_to_join <- read_rds("../data/global.prov.rds") |>
  filter(ADM0_NAME == cntry & yr.end > 2015) |>
  st_drop_geometry()

district_layer <-
  afro_Adm2 |>
    left_join(y = afro_Adm1_to_join, by = c("ADM1_NAME", "ADM0_NAME")) |>
    dplyr::select(WHO_REGION.x, ISO_2_CODE.x, ADM2_NAME, ADM1_NAME, ADM0_NAME, WHO_CODE.x, ADM2_ALTCODE, ADM1_ALTCODE, 
                  Lat_Y = CENTER_LAT.x, Long_X =  CENTER_LON.x) |>
    mutate(KEY = paste0(cntry_code, "-", ADM1_ALTCODE, "-",  ADM2_ALTCODE) ) |>
    as_tibble()
  
# merge sequences results with spatial (district) layer created
sequences_by_year <-
  sequences_results |>
  mutate(Year = format(COLLECTION_DATE, "%Y")) |>
  group_by(Year) |>
  count()

geom_sequences <-
  sequences_results |>
  mutate(Year = format(COLLECTION_DATE, "%Y")) |>
  mutate(KEY = substr(EPID, 1, 11)) |>
  left_join(y = district_layer, by = "KEY")  |>
  left_join(sequences_by_year, by = "Year")



# Function to generate random points within a polygon
generate_random_point <- function(polygon) {
  bbox <- as(extent(polygon), "SpatialPolygons")
  random_point <- spsample(bbox, 1, type = "random")
  return(random_point)
}

# Generate random points for each row
for (i in 1:nrow(geom_sequences)) {
  district <- geom_sequences$ADM2_NAME[i]
  district_polygon <- afro_Adm2 |>
    filter(afro_Adm2$ADM2_NAME == district) |>
    st_geometry() |>
    st_as_sf() 
  random_point <- generate_random_point(district_polygon)
  geom_sequences$Long_X[i] <- random_point@coords[1]
  geom_sequences$Lat_Y[i] <- random_point@coords[2]
}


#Sequences_map <- 
  ggplot(data = geom_sequences) +
  geom_sf(data = afro_Adm1, fill = "gray", color = "white") +
  geom_sf(data = afro_cntries, fill = NA, color = "black", size = 4) +
  #geom_sf(data = afro_Adm2, fill = "gray", color = "white") +
  geom_point(aes(x = Long_X, y = Lat_Y), shape = 16, size = 1.5, color = "red", fill = "black", stroke = 1.5) +
  geom_text(aes(x = Inf, y = Inf, label = paste("n =", n)), hjust = 2.8, vjust = 18, size = 4, color = "black") +
  
  #scale_color_manual(values = c("cVDPV1" = "#F067A6", "cVDPV2" = "#2CBB9B")) +
  labs(x = "Longitude", y = "Latitude", color = "Viruses Isolated", 
       title = paste0("Geolocation of reported viruses in ", cntry_code)) +
  facet_wrap(~ Year, ncol = 4) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")    # Center ggplot title and legend at bottom

  
  # saving the plot as image png  
  ggsave("Sequences_Map.png", Sequences_map, path = "output/")  #export population map + es sites  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    