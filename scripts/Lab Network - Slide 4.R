# Check if the package pacman is installed (pacman Install and load multiple desired packages at once) =====
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

#install library to import geojson, ggspatial enable R to read and manipulate geojson spatial features ====
#and ggrepel allows to edit the labels and avoid overlaps
#raster is for geographic data analysis, in this case to generate a random point within the district polygon
p_load(tidyverse, sf)

# Virus isolation/ITD laboratories in Afro =====
virus_isolation <- read_csv("../data/data/polio_lab_network_afro.csv")
polio_labs <- read_csv("../data/data/Geocoordinates of Labs in African region.csv")
# Sequencing laboratories in Afro

# virus count by lab
virusIsol_by_lab <-
  virus_isolation |>
  count(`Virus Isolation`) |>
  mutate(label_visolation = paste0(`Virus Isolation`, " (", n, ")"))

seq_by_lab <-
  virus_isolation |>
  count(`Sequencing Lab`) |>
  mutate(label_Seq = paste0(`Sequencing Lab`," (", n, ")"))

vec_labs_Seq <-
  setNames(seq_by_lab$label_Seq, seq_by_lab$`Sequencing Lab`)

# create a named vector with the values of virus by lab
virus_label <-
  setNames(virusIsol_by_lab$label_visolation, virusIsol_by_lab$`Virus Isolation`)

# load administrative boundaries =====
afro_Adm0 <- 
  read_rds("../data/global.ctry.rds") |> 
             filter(`WHO_REGION` == "AFRO" | 
                      ADM0_NAME %in% c("EGYPT", "MOROCCO", "TUNISIA", "WESTERN SAHARA", "SUDAN", "LIBYA", "SOMALIA", "DJIBOUTI"))

afro_cntries <- 
  read_rds("../data/global.ctry.rds") |>
                filter(WHO_REGION %in% "AFRO" | ADM0_NAME == c("SOMALIA", "DJIBOUTI")) |>
                left_join(y = virus_isolation, by = c("ADM0_NAME" = "COUNTRY"))

# Virus isolation =====
virus_isolation_map <-
    ggplot() +
      geom_sf(data = afro_Adm0, aes(fill = "gray"), fill = "gray", color = "white") +
      geom_sf(data = afro_cntries, aes(fill = `Virus Isolation`), color = "black", size = 2) +
      geom_point(data = polio_labs, aes(x = `X (LONG)` , y = `Y (LAT)`), color = "black", size = 3, shape = 8) +
      scale_fill_manual(values = c("ALG" =  "#a6cee3", "SOA" =  "#1f78b4", "GHA" =  "#b2df8a", "SEN" =  "#33a02c", 
                                   "UGA" =  "#fb9a99", "CAE" =  "#ffff99", "CAF" =  "#f768a1", "CIV" =  "cyan", 
                                   "RDC" =  "#ec7014", "ETH" =  "#fdbf6f", "KEN" =  "lightgreen", "MAD" =  "#66bd63",
                                   "IBD, MDG" =  "#01665e", "ZAM" =  "#cab2d6", "ZIM" =  "#7fcdbb"), na.value = "grey50",
                        labels = virus_label) +
      scale_shape_manual(name = "Polio Labs", values = rep(8, 10)) +
      labs(fill = " ") + # remove the old legend name - Virus Isolation/ ITD Labs
      theme_bw() +
      theme(legend.position = c(0.93, 0.5),
        legend.background = element_rect(fill = NA, color = NA)) #+
      #theme(legend.position = "bottom") +
      #guides(fill = guide_legend(nrow = 1), color = guide_legend(nrow = 1))
  
  # Sequencing labs =====
sequencing_map <-
      ggplot() +
          geom_sf(data = afro_Adm0, aes(fill = "gray"), fill = "gray", color = "white") +
          geom_sf(data = afro_cntries, aes(fill = `Sequencing Lab`), color = "black", size = 2) +
          #geom_point(data = polio_labs, aes(x = `X (LONG)` , y = `Y (LAT)`), color = "black", size = 3, shape = 10) +
          scale_fill_manual(values = c("CDC" =  "#377eb8", "NICD" =  "#4daf4a", 
                                       "IPP" =  "#984ea3", "GHA" =  "#ff7f00"), na.value = "grey50",
                            labels = vec_labs_Seq) +
          labs(fill = " ") + # remove the old legend name
      theme_bw() + 
      theme(legend.position = c(0.93, 0.5),
        legend.background = element_rect(fill = NA, color = NA))
      #theme(legend.position = "bottom")
  
# plots
virus_isolation_map
sequencing_map


# saving the plot as image png  
ggsave("virus_isolation.png", virus_isolation_map, path = "../data/outputs/") 
ggsave("sequencing_map.png", sequencing_map, path = "../data/outputs/") 


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



