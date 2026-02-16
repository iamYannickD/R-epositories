# Charger les bibliothèques nécessaires
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(scales)
library(ggtext)

# Lire les données depuis le fichier CSV
data <- read_csv("../data/data_sequences/data3_TAT2.csv")

# Nettoyer les noms de colonnes
colnames(data) <- c("COUNTRY", "Category", "Values")

# Supprimer les lignes avec des valeurs manquantes
data <- na.omit(data)

# Extraire la période de la colonne Category
data <- data %>%
  mutate(Period = case_when(
    grepl("Jul 23 to Jul 24", Category) ~ "1 Jul 2023 - 31 Jul 2024",
    grepl("Jul 24 to Jul 25", Category) ~ "1 Jul 2024 - 31 Jul 2025",
    TRUE ~ Category
  ))

# Calculer les médianes par pays et par période
median_tat <- data %>%
  group_by(COUNTRY, Period) %>%
  summarise(Median = median(Values), 
            Mean = mean(Values),
            n = n(),
            .groups = 'drop') %>%
  arrange(Period, Median)

# Ordonner les pays par médiane de toutes les périodes combinées
# This ensures all countries are included in the factor levels
country_order <- data %>%
  group_by(COUNTRY) %>%
  summarise(Overall_Median = median(Values)) %>%
  arrange(Overall_Median) %>%
  pull(COUNTRY)

data$COUNTRY <- factor(data$COUNTRY, levels = country_order)

# Couleurs personnalisées
colors <- c("#1f77b4", "#ff7f0e")  # Bleu et orange
background_color <- "white"
grid_color <- "#dee2e6"
text_color <- "#343a40"

# Créer le boxplot beautifié
p <- ggplot(data, aes(x = Values, y = COUNTRY, fill = Period)) +
  # Boxplots avec style amélioré
  geom_boxplot(
    outlier.shape = 21, 
    outlier.fill = "#dc3545",
    outlier.color = "white",
    outlier.size = 2,
    alpha = 0.8,
    width = 0.7,
    coef = 1.5  # Whiskers à 1.5*IQR
  ) +
  
  # Ligne de référence à 45 jours
  geom_vline(xintercept = 45, color = "#28a745", linetype = "dashed", 
             size = 1.2, alpha = 0.8) +
  
  # Annotation pour la ligne de référence
  annotate("text", x = 47, y = Inf, label = "45 days", 
           color = "#28a745", size = 4, fontface = "bold",
           hjust = 0.85, vjust = 1.5, angle = 0) +
  
  # Points pour les médianes
  geom_point(data = median_tat, 
             aes(x = Median, y = COUNTRY),
             color = "white",
             size = 3,
             shape = 21,
             fill = "#dc3545",
             stroke = 1) +
  
  # Facet wrap avec style
  facet_wrap(~ Period, ncol = 2, scales = "free_y") +
  
  # Échelles et couleurs
  scale_fill_manual(values = colors) +
  scale_x_continuous(
    limits = c(0, 350),
    breaks = seq(0, 350, 50),
    labels = comma_format(),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  
  # Labels et titre
  labs(
    x = "**TAT Sanger Results (days)**",
    y = "**GPEI HP Countries**",
    caption = "Red points represent median values, boxplots show distribution"
  ) +
  
  # Thème personnalisé
  theme_minimal(base_size = 12) +
  theme(
    # Général
    plot.background = element_rect(fill = background_color, color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    # Textes
    plot.title = element_markdown(hjust = 0.5, size = 16, face = "bold", 
                                  margin = margin(b = 10)),
    plot.subtitle = element_markdown(hjust = 0.5, size = 12, color = "#6c757d",
                                     margin = margin(b = 20)),
    plot.caption = element_markdown(size = 9, color = "#6c757d", hjust = 0,
                                    margin = margin(t = 15)),
    axis.title.x = element_markdown(face = "bold", size = 11),
    axis.title.y = element_markdown(face = "bold", size = 11),
    axis.text = element_text(color = text_color),
    axis.text.y = element_text(size = 9, face = "bold"),
    
    # Grille
    panel.grid.major = element_line(color = grid_color, linewidth = 0.3),
    panel.grid.minor = element_blank(),
    
    # Facets
    strip.text = element_text(face = "bold", size = 11, color = "white"),
    strip.background = element_rect(fill = "#495057", color = NA),
    
    # Légende
    legend.position = "none",
    
    # Marges
    plot.margin = margin(20, 20, 20, 20)
  )

# Afficher le plot
print(p)

# Sauvegarder le graphique en haute qualité
ggsave("TAT_Sanger_beautiful_facet.png", plot = p, 
       width = 14, height = 10, dpi = 300, units = "in",
       bg = "white")

# Statistiques descriptives formatées
cat("\n", strrep("=", 60), "\n")
cat("DESCRIPTIVE STATISTICS - TAT Sanger Sequencing\n")
cat(strrep("=", 60), "\n\n")

summary_stats_formatted <- median_tat %>%
  mutate(across(c(Median, Mean), ~round(., 1)),
         Stats = paste0("Median: ", Median, " days | Mean: ", Mean, " days | n: ", n)) %>%
  select(COUNTRY, Period, Stats) %>%
  pivot_wider(names_from = Period, values_from = Stats, names_sort = TRUE)

print(summary_stats_formatted, n = Inf)

# Résumé comparatif
cat("\n", strrep("=", 60), "\n")
cat("PERFORMANCE COMPARISON BETWEEN PERIODS\n")
cat(strrep("=", 60), "\n")

comparison <- median_tat %>%
  select(COUNTRY, Period, Median) %>%
  pivot_wider(names_from = Period, values_from = Median, names_sort = TRUE) %>%
  mutate(Difference = `1 Jul 2024 - 31 Jul 2025` - `1 Jul 2023 - 31 Jul 2024`,
         Improvement = ifelse(Difference < 0, "✓ Improved", "⚠️ Worsened"),
         Improvement = ifelse(is.na(Difference), "No comparison", Improvement)) %>%
  arrange(Difference)

print(comparison, n = Inf)
