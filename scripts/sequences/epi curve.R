# libraries
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

p_load(tidyverse, sf, readxl, geojsonsf, ggspatial, ggrepel, raster, zoo)

# load data
AFP <- read_csv("../data/data_sequences/analysis/AFP.csv")

ES <- read_csv("../data/data_sequences/analysis/ES.csv")


# Ajouter une colonne "source" et combiner les deux datasets
AFP <- AFP %>%
  mutate(source = "AFP")

ES <- ES %>%
  mutate(source = "ES")

combined <- bind_rows(AFP, ES)

# Convertir les dates et grouper par mois
combined <- combined %>%
  mutate(
    COLLECTION_DATE = dmy(COLLECTION_DATE),
    month = floor_date(COLLECTION_DATE, "month")
  )

# Réorganiser en format long
df_long <- combined %>%
  pivot_longer(cols = c(PV1, PV2, PV3), names_to = "PolioType", values_to = "Cases") %>%
  group_by(source, month, PolioType) %>%
  summarise(Cases = sum(Cases, na.rm = TRUE), .groups = "drop")

# Palette de couleurs
polio_colors <- c(
  "PV1" = "#F8766D",
  "PV2" = "#00BA38",
  "PV3" = "#E69F00"
)

# Graphique
ggplot(df_long, aes(x = month, y = Cases, fill = PolioType)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~ source, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = polio_colors) +
  scale_x_date(
    date_labels = "%b %Y",  # format mois abrégé + année (ex: Jan 2018)
    date_breaks = "3 months",  # afficher tous les 2 mois
    expand = c(0.01, 0.01)
  ) +
  labs(
    x = NULL, y = "Number of cases",
    fill = "Polio type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )
