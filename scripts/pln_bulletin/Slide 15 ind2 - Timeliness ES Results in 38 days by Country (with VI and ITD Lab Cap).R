# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages =====
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
p_load(tidyverse, RODBC,gt, gtExtras)

#Give the path to the ES database
Specify_the_period <- "WEEK 1 - 38, 2024"
path_ES_2025 = "../data/dbs/ES_Week52_2024.mdb"

# Connect to the Microsoft Access database ====
ESdb2025 <- DBI::dbConnect(odbc::odbc(), 
                           .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                              DBQ=", path_ES_2025))
# load data in R =====
# Retrieve all data from the AFP database
EStables2025 <- DBI::dbGetQuery(ESdb2025, "SELECT * FROM Environmental ORDER BY IDNumber;", stringsAsFactors = FALSE) |>
  as_tibble() |>
  mutate(Labname = str_replace_all(Labname, c("ENTEBBE" = "UGA", "GHANA" = "GHA", "INRB" = "RDC", "IPD SEN" = "SEN",
                                              "IPM, MAD" = "MAD", "IPM,MAD" = "MAD", "KEMRI" = "KEN", "IBD, Nigeria" = "IBD",
                                              "MDG, Nigeria" = "MDG", "ZAM UTH" = "ZAM", "ZAM-UTH" = "ZAM")),
         date_result_to_lab = coalesce(Dateresultstolab, Datefinalcultureresult),
         date_itd_result = coalesce(DateFinalCombinedResult, DatefinalResultReported)
  )

#Specify_the_period <- paste0("WEEK 1 - ", (epiweek(as.Date(ymd_hms(AFPtables$DateUpdated))) - 1) |> unique(), ", 2024")


# Analysis of databases =====
ES_byCountry38 <-
 EStables2025 |> 
  filter(Countryname %in% c("Djibouti", "Somalia") == F) |> # removed djibouti and Somalia Countrycode
  mutate(IST = case_when(Countrycode %in% c("ALG", "BEN", "BFA", "CAV", "CIV", "GAM", "GHA", "GUB", "GUI", "LIB", "MAI", "MAU",
                                            "NIE", "NIG", "SEN", "SIL",  "TOG" ) ~ "WEST",
                         Countrycode %in% c( "ANG", "CAE", "CAF", "CHA",  "EQG", "GAB", "CNG", "RDC") ~ "CENTRAL",
                         Countrycode %in% c( "BOT", "BUU", "COM", "DJI", "ERI", "ETH", "KEN", "LES", "MAD", "MAL", "MAS", "MOZ", "NAM", "RSS", "RWA",
                                             "SOA", "SOM", "SWZ", "SYC", "TAN", "UGA", "ZAM", "ZIM") ~ "ESA"), .before = Countrycode) |>
  group_by(IST, Countrycode) |>
  mutate(Labname = str_replace_all(Labname, "ESWATINI", "SOA" ),
         Labname = if_else(Countryname == "Angola", "SOA", Labname)
  ) |>
  summarize(
    workload_by_lab = n(),
    
    ITD_results = sum(str_detect(Finalcellcultureresult, "^1") | str_detect(Finalcellcultureresult, "^4"), na.rm = TRUE),
    
    ITD_results_21days = sum( (str_detect(Finalcellcultureresult, "^1") | str_detect(Finalcellcultureresult, "^4")) & 
                                !is.na(FinalcombinedrRTPCRresults) & (date_itd_result - Dateofcollection) < 39 & 
                                (date_itd_result - Dateofcollection) >= 0, na.rm = TRUE),
    Prop_ITD_38days = round(ITD_results_21days / ITD_results * 100, 0),
  ) |>
  select(IST, Countrycode, Prop_ITD_38days) |>
  filter(!is.na(Prop_ITD_38days), Prop_ITD_38days > 0) |>
  pivot_longer(cols = c(Prop_ITD_38days), names_to = "Proportions", values_to = "Values") |>
  filter(!Countrycode %in% c("SOM", "DJI") ) |> #removed somalia and djibouti from the list of countries
  #filter( !(Countrycode %in% c("ALG", "CAE", "CAF", "CIV", "ETH", "GHA", "NIE", "KEN", "MAD", "RDC", "SEN", "SOA", "UGA", "ZAM")) ) |>
  filter( (Countrycode %in% c("ALG", "CAE", "CAF", "CIV", "ETH", "NIE", "KEN", "MAD", "RDC", "SEN", "UGA", "ZAM")) ) |>
  ggplot() +
  geom_bar(aes(x =  interaction(Countrycode, IST), y = Values, fill = IST), stat = "identity", position = position_dodge(), width = .9, color = "black") +
  scale_fill_manual(
    values = c("Prop_ITD_38days" = "gold", "Prop_ITD_38days" = "darkblue"),
    labels = c("Prop_ITD_38days" = "Among all samples (with results)", "Prop_ITD_38days" = "Among positive samples")
  ) +
  scale_fill_manual(
    values = c("WEST" = "darkblue", "CENTRAL" = "brown4", "ESA" = "gold"),
    labels = c("WEST" = "IST West", "CENTRAL" = "IST Central", "ESA" = "IST - ESA")
  ) +
  labs(x = "Country Code", y = "% Samples with results", fill = "", title = "") +
  theme_minimal() +
  geom_hline(yintercept = 80, linetype = "dotted", color = "green", linewidth = 2) + # green line for the target
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

ES_byCountry38

# saving the plot as image png  
ggsave("ESCountry38_plot.png", ES_byCountry38, path = "../data/outputs/", width = 13, height= 6)  



