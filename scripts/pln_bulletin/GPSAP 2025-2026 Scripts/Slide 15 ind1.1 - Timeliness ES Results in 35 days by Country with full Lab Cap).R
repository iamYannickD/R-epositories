# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages =====
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
p_load(tidyverse, RODBC,gt, gtExtras, webshot)

#Give the path to the Sequencing results file
seq_result_ES <- read.csv("../data/data/All virus sequencing results.csv") |>
  mutate(DATE_COLL = dmy(ONSET..COLLECTION)) |>
  filter(SOURCE == "ENV" & (today() - DATE_COLL < 366)) |>  # (year(DATE_COLL) > 2024 ) or #%in% c(2024, 2025)
  mutate(DATE_RECEIVED = dmy(DATE.RECEIVED)) |>
  mutate(TAT = difftime(DATE_RECEIVED, DATE_COLL)) |> tibble()


# Analysis of databases =====
Sequencing.results.countries.with.labs <- 
  seq_result_ES |>
  mutate(
    CountryCode = substr(EPID.NUMBER, start = 5, stop = 7), .before = COUNTRY) |>
  mutate(IST = case_when(CountryCode %in% c("ALG", "BEN", "BFA", "CIV", "GAM", "GHA", "GUB", "GUI", "LIB", "MAI", "MAU",
                                            "NIE", "NIG", "SEN", "SIL",  "TOG" ) ~ "WEST",
                         CountryCode %in% c( "ANG", "CAE", "CAF", "CHA",  "EQG", "GAB", "CNG", "RDC") ~ "CENTRAL",
                         CountryCode %in% c( "BOT", "BUU", "COM", "ETH", "KEN", "LES", "MAD", "MAL", "MOZ", "NAM", "RSS", "RWA",
                                             "SOA", "SWZ", "TAN", "UGA", "ZAM", "ZIM") ~ "ESA"), .before = CountryCode) |>
  select(IST, CountryCode, DATE_COLL, DATE_RECEIVED, TAT) |>
  group_by(IST, CountryCode) |>
  filter( (CountryCode %in% c("GHA", "SOA")) ) |> # "UGA", "NIE"
  mutate(TAT_days = as.numeric(TAT)) |>
  ggplot(aes(x = TAT_days)) +
  geom_bar(aes(fill = TAT_days <= 35), color = "black", width = 1.5) +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"), 
                    labels = c("TRUE" = "<= 35 jours", "FALSE" = "≥ 35 jours")) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 5) +
  geom_vline(xintercept = 35, linetype = "dashed", color = "red", linewidth = 1) +
  labs(
    title = " ",
    x = "Turnaround Time (in Days)",
    y = "Number of Samples with Sequencing Results",
    fill = "TAT (in Days)"
  ) +
  scale_x_continuous(
    breaks = c(0, 15, 30, 45, 60),  # Graduations de l'axe X
    limits = c(0, 60)  # Définir les limites de l'axe X
  ) +
  scale_y_continuous(
    breaks = seq(0, 5, by = 1)
  ) +
  theme_minimal()

Sequencing.results.countries.with.labs

# saving the plot as image png  
ggsave("ES_Countries_under_35_plot.png", Sequencing.results.countries.with.labs, path = "../data/outputs/", width = 13, height= 6) 





