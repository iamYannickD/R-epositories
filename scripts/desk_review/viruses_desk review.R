#check directory and assign it to a path
#setwd("C:/Users/OMEN/Documents/WHO/R_Projects/automated_desk_review")
path <- getwd()

# Check if the package pacman is installed (pacman Install and load multiple desired packages at once)
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

#install library to import geojson, ggspatial enable R to read and manipulate geojson spatial features
#and ggrepel allows to edit the labels and avoid overlaps
#raster is for geographic data analysis, in this case to generate a random point within the district polygon
p_load(tidyverse, sf, geojsonsf, ggspatial, ggrepel, raster)

#load dataset
viruses_isolated <- read_csv("../data/data_dr/viruses/linelist_virus_apr_june_2024.csv") |>
  mutate(
    Country = str_replace_all(Country, "DRC", "DEMOCRATIC REPUBLIC OF THE CONGO"),
    Country = str_replace_all(Country, "SIERRA LEON", "SIERRA LEONE")
        )
masterlist <- read_csv("../data/data_dr/es_sites/ES_Sites_Masterlist.csv")

# load administrative boundaries
afro_Adm0 <- geojsonsf::geojson_sf("../data/data_dr/sf/admin0_geo.geojson") |>
  mutate(ADM0_VIZ_N = str_replace_all(ADM0_VIZ_N, "Côte d'Ivoire", "COTE D'IVOIRE"))
  

afro_Adm1_alt <- geojsonsf::geojson_sf("../data/data_dr/sf/admin1_geo.geojson")
afro_Adm2 <- read_rds("../data/global.dist.rds") |> 
  filter(WHO_REGION == "AFRO")

# add geocordinates on afp samples and randomize if more than 1 sample was collected in the same district
#In this section, i generate a coordinate for all AFP cases based on their epid number (country code, province code, district code)
#then for each sample i have the geometry of the district were they are in, then i generate a random point inside that district polygon
#so that if more than a sample is collected in the same district, they are not on top of each other
afp_virus <- 
  viruses_isolated |>
    filter(Source != "ENV") |>
    mutate(
      Province = str_replace_all(Province, "HAUT-KATANGA", "HAUT KATANGA"),
      District = str_replace_all(District, "TUDUN", "TUDUN WADA")
          ) |>
    left_join(y = afro_Adm2, by = c("Country" = "ADM0_NAME", "Province" = "ADM1_NAME", "District" = "ADM2_NAME"),
              relationship = "many-to-many") |>
    dplyr::select(`EPID Number`, Virus, Source, Country, Province, DISTRICT = District, `ES Site Name`, `Onset/ Collection Date`,
                  CENTER_LON, CENTER_LAT, SHAPE)

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
                                  afp_virus$CENTER_LON[i] <- random_point@coords[1]
                                  afp_virus$CENTER_LAT[i] <- random_point@coords[2]
                                }

# ES virus
es_virus <-
  viruses_isolated |>
    filter(Source == "ENV") |>
    mutate(
      epid_match = str_sub(`EPID Number`, 1, 19)
         ) |>
  mutate(
    epid_match = str_replace_all(epid_match, "ENV-ALG-TMR-TMR-RLV", "ENV-ALG-TAM-TAM-REL"),
    epid_match = str_replace_all(epid_match, "ENV-ANG-LUA-VIA-CFG", "ENV-ANG-LUA-VIA-CZG") 
         ) |>
  left_join(y = masterlist, by = c("epid_match" = "SITE_CODE")) |>
  dplyr::select(`EPID Number`, Virus, Source, COUNTRY, PROVINCE, 
                District, `ES Site Name`,  `Onset/ Collection Date`, Lat_Y, Long_X)


#initialization
#cntry <- "ALGERIA" #to test
countries <- es_virus$COUNTRY |> 
             str_to_upper() |>  unique() |> sort()

for (cntry in countries) {
  
                          es_virus_cntry <-
                            es_virus |>
                            filter(es_virus$COUNTRY == cntry) |>
                            #remove duplicates
                            distinct(`ES Site Name`, Lat_Y, Long_X, .keep_all = TRUE)
                          
                          afro_Adm0_cntry <-
                            afro_Adm0 |>
                            mutate(ADM0_VIZ_N = str_to_upper(ADM0_VIZ_N)) |>
                            filter(ADM0_VIZ_N == cntry)
                          
                          afro_admin1_country <-
                            afro_Adm1_alt |>
                            mutate(ADM0_VIZ_N = str_to_upper(ADM0_VIZ_N)) |>
                            mutate(
                              ADM0_VIZ_N = str_replace_all(ADM0_VIZ_N, "CÔTE D'IVOIRE", "COTE D'IVOIRE")
                            ) |> filter(ADM0_VIZ_N == cntry)
                          
                         # Plot the map with the updated coordinates
                         plot1 <- ggplot(data = es_virus_cntry) +
                            geom_sf(data = afro_admin1_country, fill = NA, color = "gray") +
                            geom_sf(data = afro_Adm0_cntry, fill = NA, color = "black") +
                            geom_point(shape = 15, size = 2.5, stroke = 1,  aes(x = Long_X, y = Lat_Y, color = Virus)) +
                            scale_color_manual(values = c("cVDPV1" = "purple", "VDPV1" = "purple4", "cVDPV2" = "darkgreen", "VDPV2" = "green2")) +
                            labs(x = "Longitude", y = "Latitude", color = "Virus Type", 
                                 title = paste0("Map of all ", es_virus_cntry$Virus, " Isolated in ", str_to_title(afro_Adm0_cntry$ADM0_VIZ_N))) +
                            geom_text_repel(data = es_virus_cntry, 
                                            aes(x = Long_X, y = Lat_Y, label = `ES Site Name`,
                                                fontface = "bold", point.size = 10), 
                                            label.r = 0.015, label.size = 0.01, 
                                            color = "black", bg.color = "white", bg.r = 0.15, size = 2, point.size = 2,
                                            box.padding = 0.8, max.overlaps = Inf, force = 1, force_pull =  1, 
                                            direction = "both", max.time = 1, max.iter = 50000,
                                            #xlim = c(NA, Inf), ylim = c(-Inf, Inf), clip = "ON",
                                            min.segment.length = 0.5) +
                            theme_bw() +
                            theme(plot.title = element_text(hjust = 0.5))     # Center ggplot title
                         
                         ggsave(paste0("../data/data_dr/outputs/Map_Virus/", cntry,".png"), plot1) #export population map + es sites
                        }

