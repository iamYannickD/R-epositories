# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages =====
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
p_load(tidyverse, RODBC,gt, gtExtras)

#Give the path to the ES database
Specify_the_period <- "WEEK 1 - 21, 2024"
path_ES_2024 = "../data/dbs/ES_160724.mdb"

# Connect to the Microsoft Access database ====
ESdb2024 <- DBI::dbConnect(odbc::odbc(), 
                           .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                              DBQ=", path_ES_2024))
# load data in R =====
# Retrieve all data from the AFP database
EStables2024 <- DBI::dbGetQuery(ESdb2024, "SELECT * FROM Environmental ORDER BY IDNumber;", stringsAsFactors = FALSE) |>
  as_tibble() |>
  mutate(Labname = str_replace_all(Labname, c("ENTEBBE" = "UGA", "GHANA" = "GHA", "INRB" = "RDC", "IPD SEN" = "SEN",
                                              "IPM, MAD" = "MAD", "IPM,MAD" = "MAD", "KEMRI" = "KEN", "IBD, Nigeria" = "IBD",
                                              "MDG, Nigeria" = "MDG", "ZAM UTH" = "ZAM", "ZAM-UTH" = "ZAM")),
         date_result_to_lab = coalesce(Dateresultstolab, Datefinalcultureresult),
         date_itd_result = coalesce(DateFinalCombinedResult, DatefinalResultReported)
  )

#Specify_the_period <- paste0("WEEK 1 - ", (epiweek(as.Date(ymd_hms(AFPtables$DateUpdated))) - 1) |> unique(), ", 2024")

# Analysis of databases =====
EStimeliness <- 
 EStables2024 |> 
  filter(Countryname %in% c("Djibouti", "Somalia") == F) |> # removed djibouti and Somalia
  group_by(Labname) |>
  mutate(Labname = str_replace_all(Labname, "ESWATINI", "SOA" ),
         Labname = if_else(Countryname == "Angola", "SOA", Labname)
         #Labname = if_else( (is.na(Labname) & Countryname == "ANGOLA"), "SOA", Labname)
    ) |>
  summarize(
    workload_by_lab = n(),
    
    ITD_results = sum(str_detect(Finalcellcultureresult, "^1") | str_detect(Finalcellcultureresult, "^4"), na.rm = TRUE),
    ITD_results_positive = sum(
            (str_detect(Finalcellcultureresult, "^1") | str_detect(Finalcellcultureresult, "^4")) &
             # (str_detect(FinalcombinedrRTPCRresults, "Discordant") | str_detect(FinalcombinedrRTPCRresults, "PV2")), na.rm = TRUE),
              (str_detect(FinalcombinedrRTPCRresults, "Discordant") | str_detect(FinalcombinedrRTPCRresults, "PV")), na.rm = TRUE),
    
    
    ITD_results_21days = sum( (str_detect(Finalcellcultureresult, "^1") | str_detect(Finalcellcultureresult, "^4")) & 
                                !is.na(FinalcombinedrRTPCRresults) & (date_itd_result - Datesampleinlab) < 22 & 
                                (date_itd_result - Datesampleinlab) >= 0, na.rm = TRUE),
    ITD_results_21days_positive = sum( (str_detect(Finalcellcultureresult, "^1") | str_detect(Finalcellcultureresult, "^4")) & 
                                  # (str_detect(FinalcombinedrRTPCRresults, "Discordant") | str_detect(FinalcombinedrRTPCRresults, "PV2")) & 
                                    (str_detect(FinalcombinedrRTPCRresults, "Discordant") | str_detect(FinalcombinedrRTPCRresults, "PV")) & 
                                !is.na(FinalcombinedrRTPCRresults) & (date_itd_result - Datesampleinlab) < 22 & 
                                (date_itd_result - Datesampleinlab) >= 0, na.rm = TRUE),
    Prop_ITD_21days = round(ITD_results_21days / ITD_results * 100, 0),
    Prop_ITD_21days_positive = round(ITD_results_21days_positive / ITD_results_positive * 100, 0)
    ) |> 
  # Intermediate results
  # summarise(
  #   Prop_ITD_21days_all_sample =  sum(ITD_results_21days, na.rm = TRUE) / sum(ITD_results, na.rm = TRUE),
  #   Prop_ITD_21days_positive_sample = sum(ITD_results_21days_positive, na.rm = TRUE) / sum(ITD_results_positive, na.rm = TRUE))

  select(Labname, Prop_ITD_21days, Prop_ITD_21days_positive) |>
  pivot_longer(cols = c(Prop_ITD_21days, Prop_ITD_21days_positive), names_to = "Proportions", values_to = "Values") |>
  filter(!is.na(Labname)) |> # remove zim as they dont analyze ES samples
  ggplot() +
  geom_bar(aes(x = Labname, y = Values, fill = Proportions), stat = "identity", position = position_dodge(), width = .9, color = "black") +
  scale_fill_manual(
    values = c("Prop_ITD_21days" = "gold", "Prop_ITD_21days_positive" = "darkblue"),
    labels = c("Prop_ITD_21days" = "Among all samples (with results)", "Prop_ITD_21days_positive" = "Among positive samples")
  ) +
  labs(x = "Lab Name", y = "% Samples with results", fill = "", title = "ES - ITD Results by Lab in 21 days") +
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
  )

EStimeliness

# saving the plot as image png  
ggsave("EStimeliness21_plot.png", EStimeliness, path = "../data/outputs/", width = 8, height= 8)  
