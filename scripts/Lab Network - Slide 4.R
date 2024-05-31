#check directory and assign it to a path =====
path <- getwd()

# Check if the package pacman is installed (pacman Install and load multiple desired packages at once) =====
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

#install library to import geojson, ggspatial enable R to read and manipulate geojson spatial features ====
#and ggrepel allows to edit the labels and avoid overlaps
#raster is for geographic data analysis, in this case to generate a random point within the district polygon
p_load(tidyverse, sf)

# Virus isolation/ITD laboratories in Afro =====
virus_isolation <- read_csv("data/polio_lab_network_afro.csv")
# Sequencing laboratories in Afro

# load administrative boundaries =====
afro_Adm0 <- 
  read_rds("../automated_desk_review/data/dataset_desk_review/global_dataset/global.ctry.rds") |> 
             filter(`WHO_REGION` == "AFRO" | 
                      ADM0_NAME %in% c("EGYPT", "MOROCCO", "TUNISIA", "WESTERN SAHARA", "SUDAN", "LIBYA", "SOMALIA", "DJIBOUTI"))

afro_cntries <- 
  read_rds("../automated_desk_review/data/dataset_desk_review/global_dataset/global.ctry.rds") |>
                filter(WHO_REGION %in% "AFRO" | ADM0_NAME == c("SOMALIA", "DJIBOUTI")) |>
                left_join(y = virus_isolation, by = c("ADM0_NAME" = "COUNTRY"))

# Virus isolation =====
virus_isolation_map <-
    ggplot() +
      geom_sf(data = afro_Adm0, aes(fill = "gray"), fill = "gray", color = "white") +
      geom_sf(data = afro_cntries, aes(fill = `Virus Isolation`), color = "black", size = 2) +
      scale_fill_manual(values = c("ALG" =  "darkblue", "SOA" =  "#d73027", "GHA" =  "#f46d43", "SEN" =  "#fdae61", 
                                   "UGA" =  "#fee08b", "CAE" =  "#ffffbf", "CAF" =  "#d9ef8b", "CIV" =  "cyan", 
                                   "RDC" =  "#1a9850", "ETH" =  "purple", "KEN" =  "lightgreen", "MAD" =  "#66bd63",
                                   "NIE" =  "#01665e", "ZAM" =  "orange", "ZIM" =  "#00683784"), na.value = "grey50") +
      
      scale_color_manual(values = c(values = c("ALG" =  "darkblue", "SOA" =  "#d73027", "GHA" =  "#f46d43", "SEN" =  "#fdae61", 
                                               "UGA" =  "#fee08b", "CAE" =  "#ffffbf", "CAF" =  "#d9ef8b", "CIV" =  "cyan", 
                                               "RDC" =  "#1a9850", "ETH" =  "purple", "KEN" =  "lightgreen", "MAD" =  "#66bd63",
                                               "NIE" =  "#01665e", "ZAM" =  "orange", "ZIM" =  "#00683784"), na.value = "grey50")
                                      ) +
      labs(fill = "Virus Isolation/ ITD Labs")
  
  # Sequencing labs =====
sequencing_map <-
      ggplot() +
          geom_sf(data = afro_Adm0, aes(fill = "gray"), fill = "gray", color = "white") +
          geom_sf(data = afro_cntries, aes(fill = `Sequencing Lab`), color = "black", size = 2) +
          scale_fill_manual(values = c("CDC" =  "#01665e", "NICD" =  "pink", 
                                       "IPP" =  "#fdae61", "GHA" =  "#ffffbf"), na.value = "grey50") +
          scale_color_manual(values = c("CDC" =  "#01665e", "NICD" =  "pink", 
                                        "IPP" =  "#fdae61", "GHA" =  "#ffffbf"), na.value = "grey50") +
          labs(fill = "Sequencing Labs and Refering countries")
 
  
# plots
virus_isolation_map
sequencing_map


# saving the plot as image png  
ggsave("virus_isolation.png", virus_isolation_map, path = "output/") 
ggsave("sequencing_map.png", sequencing_map, path = "output/") 


# open the table of our presentation
Pres_ppt <- read_pptx(path = "data/AFRO polio labs bulletin week 1-18_2024.pptx")

# Insert the image in a new slide
#Pres_ppt <- ph_with(on_slide(Pres_ppt, index = 3), external_img("output/AFPtables.png"), location = ph_location_fullsize())

# add the table in the 4th slide of the presentation
Pres_ppt <- ph_with(on_slide(Pres_ppt, index = 5), external_img("output/virus_isolation.png"), 
                    location = ph_location(left = 0.2, top = 1, width = 7, height = 5.5))

Pres_ppt <- ph_with(on_slide(Pres_ppt, index = 5), external_img("output/sequencing_map.png"), 
                    location = ph_location(left = 6.5, top = 1, width = 7, height = 5.5))

# Save the updated presentation
print(Pres_ppt, target = "data/AFRO polio labs bulletin week 1-18_2024.pptx")  



