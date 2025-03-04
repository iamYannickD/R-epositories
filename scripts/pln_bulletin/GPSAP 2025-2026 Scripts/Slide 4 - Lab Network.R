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
seq_labs <- read_csv("../data/data/Geocoordinates of Labs in African region.csv") |>
            filter(`Lab name` %in% c("UVRI", "Ibadan, Nigeria", "Ghana", "South Africa"))
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
      geom_point(data = polio_labs, aes(x = `X (LONG)` , y = `Y (LAT)`, shape = "Polio Labs"), color = "black", size = 3) +
      scale_fill_manual(values = c("ALG" =  "#a6cee3", "SOA" =  "#1f78b4", "GHA" =  "#b2df8a", "SEN" =  "#33a02c", 
                                   "UGA" =  "#fb9a99", "CAE" =  "#ffff99", "CAF" =  "#f768a1", "CIV" =  "cyan", 
                                   "RDC" =  "#ec7014", "ETH" =  "#fdbf6f", "KEN" =  "lightgreen", "MAD" =  "#66bd63",
                                   "IBD, MDG" =  "#01665e", "ZAM" =  "#cab2d6", "ZIM" =  "#7fcdbb"), na.value = "grey50",
                        labels = virus_label) +
      scale_shape_manual(name = "Polio Labs", values = c("Polio Labs" = 8)) + 
      labs(fill = " ") + # remove the old legend name - Virus Isolation/ ITD Labs
      theme_bw() +
      guides(fill = guide_legend(order = 1), 
            shape = guide_legend(order = 2)) +
      theme(legend.position = c(0.93, 0.5),
            axis.title.x=element_blank(),  # remove x and y titles
            axis.title.y=element_blank(),
            legend.background = element_rect(fill = NA, color = NA)) #+
      #theme(legend.position = "bottom") +
      #guides(fill = guide_legend(nrow = 1), color = guide_legend(nrow = 1))
  
  # Sequencing labs =====
sequencing_map <-
      ggplot() +
          geom_sf(data = afro_Adm0, aes(fill = "gray"), fill = "gray", color = "white") +
          geom_sf(data = afro_cntries, aes(fill = `Sequencing Lab`), color = "black", size = 2) +
          geom_point(data = seq_labs, aes(x = `X (LONG)` , y = `Y (LAT)`, shape = "Seq Labs"), color = "black", size = 5) +
          #geom_point(data = polio_labs, aes(x = `X (LONG)` , y = `Y (LAT)`), color = "black", size = 3, shape = 10) +
          scale_fill_manual(values = c("UGA" =  "#377eb8", "IBD" =  "#ffff75", "NICD" =  "#4daf4a", 
                                       "IPP" =  "#984ea3", "GHA" =  "#ff7f00"), na.value = "grey50",
                            labels = vec_labs_Seq) +
          scale_shape_manual(name = "Seq Labs", values = c("Seq Labs" = 8)) +
          labs(fill = " ") + # remove the old legend name
      theme_bw() + 
      theme(legend.position = c(0.93, 0.5),
        legend.background = element_rect(fill = NA, color = NA))
      #theme(legend.position = "bottom")
  
# plots
virus_isolation_map
sequencing_map


# saving the plot as image png  
ggsave("virus_isolation.png", virus_isolation_map, path = "../data/outputs/", width = 8, height= 8) 
ggsave("sequencing_map.png", sequencing_map, path = "../data/outputs/", width = 8, height= 8) 



