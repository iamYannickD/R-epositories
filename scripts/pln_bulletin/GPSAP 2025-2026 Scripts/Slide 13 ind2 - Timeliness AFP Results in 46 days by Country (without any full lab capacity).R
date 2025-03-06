# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages =====
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
p_load(tidyverse, RODBC,gt, gtExtras, webshot)

#Give the path to the Sequencing results file
seq_result_AFP <- read.csv("../data/data/All virus sequencing results.csv") |>
  mutate(DATE_COLL = dmy(ONSET..COLLECTION)) |>
  filter(SOURCE != "ENV" & (today() - DATE_COLL < 366)) |>  # (year(DATE_COLL) > 2024 ) or #%in% c(2024, 2025)
  mutate(DATE_RECEIVED = dmy(DATE.RECEIVED)) |>
  mutate(TAT = difftime(DATE_RECEIVED, DATE_COLL)) |> tibble()


# Analysis of databases =====
Sequencing.results.countries.without.labs <- 
  seq_result_AFP |>
  mutate(
    CountryCode = substr(EPID.NUMBER, start = 1, stop = 3), .before = COUNTRY) |>
  mutate(IST = case_when(CountryCode %in% c("ALG", "BEN", "BFA", "CIV", "GAM", "GHA", "GUB", "GUI", "LIB", "MAI", "MAU",
                                            "NIE", "NIG", "SEN", "SIL",  "TOG" ) ~ "WEST",
                         CountryCode %in% c( "ANG", "CAE", "CAF", "CHA",  "EQG", "GAB", "CNG", "RDC") ~ "CENTRAL",
                         CountryCode %in% c( "BOT", "BUU", "COM", "ETH", "KEN", "LES", "MAD", "MAL", "MOZ", "NAM", "RSS", "RWA",
                                             "SOA", "SWZ", "TAN", "UGA", "ZAM", "ZIM") ~ "ESA"), .before = CountryCode) |>
  select(IST, CountryCode, EPID.NUMBER, DATE_COLL, DATE_RECEIVED, TAT) |>
  group_by(IST, CountryCode) |>
  filter( !(CountryCode %in% c("GHA", "SOA")) ) |> # "UGA", "NIE"
  mutate(workload_by_lab = n(),
         #time_itd_results_46days = as.numeric(difftime(proxy_date_itd_result, proxy_date_collection, units = "days")),
         seq_46days = if_else( (TAT < 47 & TAT >= 0), 1, 0)  
  ) |> 
  summarize(
    workload_by_lab,
    SEQ_46days = sum(seq_46days, na.rm = TRUE),
    Prop_SEQ_46days = 100 * SEQ_46days / workload_by_lab,
  ) |>
  filter(!is.na(Prop_SEQ_46days) & Prop_SEQ_46days >= 0) |>
  # For intermediary results
  #group_by(IST) |>  summarize(median_Prop_SEQ_46days = median(Prop_SEQ_46days, na.rm = TRUE)) #to know the proportion 35 days by IST
  
  dplyr::select(IST, CountryCode, Prop_SEQ_46days)  |>
  pivot_longer(
    cols = starts_with("Prop"),
    names_to = "Metric",
    values_to = "Value" ) |> # drop_na(Value) |>
  ggplot() +
  geom_bar(aes(x = interaction(CountryCode, IST), y = Value, fill = IST), stat = "identity", position = position_dodge(), width = .9, color = "black") +
  scale_fill_manual(
    values = c("Prop_SEQ_46days" = "gold"),
    labels = c("Prop_SEQ_46days" = "Among all samples (with results)")
  ) +
  scale_fill_manual(
    values = c("WEST" = "darkblue", "CENTRAL" = "brown4", "ESA" = "gold"),
    labels = c("WEST" = "IST West", "CENTRAL" = "IST Central", "ESA" = "IST - ESA")
  ) +
  labs(x = "Country Code", y = "% Samples with results", fill = "", title = "") +
  theme_minimal() +
  geom_hline(yintercept = 80, linetype = "dashed", color = "green", linewidth = 1.5) + # green line for the target
  scale_y_continuous(breaks = seq(0, 100, by = 20), expand = c(0, 0.1)) +  # Graduate y-axis by 20%
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(face = "bold", size = 10, color = "black"),
    axis.title = element_text(face = "bold", size = 12, color = "black"),
    axis.line = element_line(color = "black", size = 0.8),
    axis.ticks = element_line(color = "black", size = 0.8), 
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) + scale_x_discrete(labels = function(x) sub("\\..*$", "", x)) # To display only CountryCode on x-axis

Sequencing.results.countries.without.labs

# saving the plot as image png  
ggsave("AFP_Countries_under_35_plot.png", Sequencing.results.countries.without.labs, path = "../data/outputs/", width = 13, height= 6) 





