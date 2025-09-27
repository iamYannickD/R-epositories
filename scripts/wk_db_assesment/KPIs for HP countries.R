library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(scales)

# Lire les données
data <- read_csv("../data/data_sequences/data3.csv")

# Nettoyer les noms de colonnes
colnames(data) <- c("CountryCode", "Jul23_Jul24", "Jul24_Jul25")

# Convertir les pourcentages en nombres
data$Jul23_Jul24 <- as.numeric(sub("%", "", data$Jul23_Jul24))
data$Jul24_Jul25 <- as.numeric(sub("%", "", data$Jul24_Jul25))

# Convertir en format long pour ggplot
data_long <- data %>%
  pivot_longer(cols = c(Jul23_Jul24, Jul24_Jul25),
               names_to = "Period",
               values_to = "Percentage")

# Ajouter une colonne pour l'étiquette de la période
data_long <- data_long %>%
  mutate(Period_Label = case_when(
    Period == "Jul23_Jul24" ~ "1 Jul 2023 - 31 Jul 2024",
    Period == "Jul24_Jul25" ~ "1 Jul 2024 - 31 Jul 2025"
  ))

# Ordonner les pays par valeur croissante pour chaque période
data_long <- data_long %>%
  group_by(Period_Label) %>%
  mutate(CountryCode = reorder(CountryCode, Percentage)) %>%
  ungroup()

# Créer le graphique avec facet_wrap
ggplot(data_long, aes(x = CountryCode, y = Percentage, fill = Period_Label)) +
  geom_bar(stat = "identity", width = 0.7) +
  # Ajouter les valeurs à l'intérieur des barres (arrondies à 0 décimale)
  geom_text(aes(label = paste0(round(Percentage, 0), "%"), 
                y = Percentage/2),  # Position au milieu de la barre
            color = "white", 
            size = 3.5, 
            fontface = "bold",
            hjust = 0.5) +
  # Ligne de référence à 80%
  geom_hline(yintercept = 80, linetype = "dashed", color = "green", linewidth = 1) +
  # Annotation pour la ligne à 80%
  annotate("text", x = Inf, y = 80, label = "80%", 
           color = "green", hjust = 1.2, vjust = -0.5, fontface = "bold") +
  facet_wrap(~ Period_Label, ncol = 1, scales = "free_x") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  scale_y_continuous(limits = c(0, 105),
                     breaks = seq(0, 100, 20),
                     labels = function(x) paste0(x, "%")) +
  labs(#title = " ",
       #x = "Country Code",
       y = "% Samples arrived in the lab in <= 7 days**",
       fill = "Period") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "none",
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "grey90", color = NA)
  )

# Sauvegarder le graphique
ggsave("CollectionODK.png", width = 12, height = 8, dpi = 300, units = "in")

#=======================


# Version avec coord_flip pour une lecture horizontale plus facile
ggplot(data_long, aes(x = CountryCode, y = Percentage, fill = Period_Label)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage, 0), "%"), 
                y = Percentage/2),
            color = "white", 
            size = 3.5, 
            fontface = "bold",
            hjust = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "green", linewidth = 1) +
  annotate("text", x = Inf, y = 80, label = "80%", 
           color = "green", hjust = 1.2, vjust = -0.5, fontface = "bold") +
  facet_wrap(~ Period_Label, ncol = 1, scales = "free_x") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  scale_y_continuous(limits = c(0, 105),
                     breaks = seq(0, 100, 20),
                     labels = function(x) paste0(x, "%")) +
  labs(title = "Percentage of Samples Arrived in the Lab in ≤7 Days",
       x = "Country Code",
       y = "Percentage (%)",
       fill = "Period") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.y = element_text(size = 10, hjust = 1),
    axis.text.x = element_text(size = 10),
    legend.position = "none",
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "grey90", color = NA)
  ) +
  coord_flip()  # Inverser les axes pour une lecture horizontale
