# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages =====
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
p_load(tidyverse, RODBC, gt, gtExtras)

seq_result_AFP <- read_csv("../data/data/All PV list_from 2024.csv") |>
  #mutate(DATE_COLL = dmy(ONSET..COLLECTION)) |>
  filter(SOURCE != "ENV") # & (today() - DATE_COLL < 366)) |>
  # mutate(DATE_RECEIVED = dmy(DATE.RECEIVED)) |>
  # mutate(TAT = difftime(DATE_RECEIVED, DATE_COLL), 
  #        TAT_days = as.numeric(TAT)) |>
  # tibble()

# Préparation du jeu de données avec IST et CountryCode
data_AFP <- seq_result_AFP |>
  # mutate(
  #   CountryCode = substr(EPID.NUMBER, start = 1, stop = 3),
  #   .before = COUNTRY) |>
  mutate(
    IST = case_when(
      CountryCode %in% c("ALG", "BEN", "BFA", "CIV", "GAM", "GHA", "GUB", "GUI", "LIB", "MAI", "MAU",
                         "NIE", "NIG", "SEN", "SIL", "TOG") ~ "WEST",
      CountryCode %in% c("ANG", "CAE", "CAF", "CHA", "EQG", "GAB", "CNG", "RDC") ~ "CENTRAL",
      CountryCode %in% c("BOT", "BUU", "COM", "ETH", "KEN", "LES", "MAD", "MAL", "MOZ", "NAM", "RSS", "RWA",
                         "SOA", "SWZ", "TAN", "UGA", "ZAM", "ZIM") ~ "ESA"
    ), .before = CountryCode ) |>
  filter(CountryCode %in% c("GHA", "SOA", "UGA", "NIE"))

# Calcul des résumés par CountryCode pour l'annotation
summary_data <- data_AFP |>
  group_by(CountryCode) |>
            summarise(
              count_low = sum(TAT <= 35, na.rm = TRUE),
              count_high = sum(TAT > 35, na.rm = TRUE),
              total = n()
            ) |>
            mutate(
              annotation = paste0("Total Samples with Seq Results: ", total, "\n TurnAround Time <= 35 days: ", count_low, " \n TurnAround Time > 35 days: ", count_high, 
                                  "  \n Prop (<=35): ", round((count_low/total)*100, 1), "%  ")
            )

# Création du graphique
Sequencing.results.countries.with.labs <- data_AFP |> 
            filter(!is.na(TAT)) |>
            ggplot(aes(x = TAT)) +
            geom_bar(aes(fill = TAT <= 35), color = "black", width = 1.5) +
            scale_fill_manual(
              values = c("TRUE" = "green", "FALSE" = "red"), 
              labels = c("TRUE" = "<= 35 jours", "FALSE" = "> 35 jours")
            ) +
            geom_vline(xintercept = 35, linetype = "dashed", color = "red", linewidth = 1) +
            annotate("text", x = 35, y = Inf, label = "(35 days)", 
                     vjust = 2, hjust = -0.1, color = "red", size = 5) +
            facet_wrap(vars(CountryCode), ncol = 2, scales = "free") +
            labs(
              title = "Turnaround Time (TAT)",
              x = "Turnaround Time (in days)",
              y = "Number of Samples with Sequencing Results",
              fill = "TAT (in days)"
            ) +
            scale_x_continuous(
              breaks = seq(0, max(data_AFP$TAT, na.rm = TRUE) + 1, by = 15),
              limits = c(0, max(data_AFP$TAT, na.rm = TRUE) + 10)
            ) +
            #scale_y_continuous(
            #  breaks = seq(0, 5, by = 1)
            #) +
            theme_minimal()

# Ajout des annotations par facet
Sequencing.results.countries.with.labs <- Sequencing.results.countries.with.labs + geom_label(
            data = summary_data,
            mapping = aes(x = Inf, y = Inf, label = annotation),
            hjust = 1.1, vjust = 1.1,
            size = 4,
            fill = "white", #color = "black",
            inherit.aes = FALSE,
            #parse = TRUE
            label.padding = unit(0.3, "lines"),
            label.r = unit(0.2, "lines"))


Sequencing.results.countries.with.labs

ggsave("AFP_Countries_under_35_plot.png", Sequencing.results.countries.with.labs, path = "../data/outputs/", width = 13, height= 6) 



