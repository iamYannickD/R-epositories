# Check if the package pacman is installed (pacman Install and load multiple desired packages at once)
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

#install library to import geojson, ggspatial enable R to read and manipulate geojson spatial features
#and ggrepel allows to edit the labels and avoid overlaps
#raster is for geographic data analysis, in this case to generate a random point within the district polygon
p_load(tidyverse, sf, geojsonsf, ggspatial, ggrepel, raster)

#load dataset
viruses_isolated <- read_csv("../data/data_dr/viruses/Linelist_viruses_july_sept_2024.csv") 
masterlist <- read_csv("../data/data_dr/es_sites/ES_Sites_Masterlist.csv")

# load administrative boundaries
afro_Adm0 <- geojsonsf::geojson_sf("../data/data_dr/sf/admin0_geo.geojson") |>
  mutate(ADM0_VIZ_N = str_replace_all(ADM0_VIZ_N, "Côte d'Ivoire", "COTE D'IVOIRE"))
  

afro_Adm1_alt <- geojsonsf::geojson_sf("../data/data_dr/sf/admin1_geo.geojson")
afro_Adm2 <- read_rds("../data/global.dist.rds") |> 
  filter(WHO_REGION == "AFRO")

# ES virus
es_virus <-
  viruses_isolated |>
    dplyr::select(`EPID NUMBER`, Virus, Source, Country, Province, 
                District, `ES Site Name`,  `Onset/ Collection Date`, Lat_Y, Long_X)


#initialization
#cntry <- "GHANA" #to test
countries <- es_virus$Country |> 
             str_to_upper() |>  unique() |> sort()

for (cntry in countries) {
  
                          es_virus_cntry <-
                            es_virus |>
                            #selection of the country of interest
                            filter(es_virus$Country == cntry) |>
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
                            geom_point(shape = 15, size = 2.5, stroke = 1, aes(x = Long_X, y = Lat_Y, color = Virus)) +
                            scale_color_manual(values = c("cVDPV1" = "#F067A6", "VDPV1" = "pink", "cVDPV2" = "#3ABB9C", "VDPV2" = "green4",
                                                          "cVDPV3" = "#8ED8F8", "VDPV3" = "#8ED8F8", "PV2 Pending" = "gray")) +
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
                            theme(plot.title = element_text(hjust = 0.5)) + # Center ggplot title
                            theme(legend.position = "bottom")
                         
                         ggsave(paste0("../data/data_dr/outputs/Map_Virus/", cntry,".png"), plot1) #export population map + es sites
                        }

