# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages =====
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
p_load(tidyverse, RODBC,gt, gtExtras, webshot, officer)

#Give the path to the AFP database
path_AFP <- "../data/dbs/AFP_09092024.mdb" 

# Connect to the Microsoft Access database =====
AFPdb <- DBI::dbConnect(odbc::odbc(), 
                        .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                              DBQ=", path_AFP))
# load data in R =====
# Retrieve all data from the AFP database
AFPtables <- DBI::dbGetQuery(AFPdb, "SELECT * FROM POLIOLAB ORDER BY LabName, EpidNumber;", stringsAsFactors = FALSE) |>
  tibble() |>  mutate(
    proxy_date_infor_itd = coalesce(DateIsolateinforITD, DateLarmIsolateRec, DateRarmIsolateSentforITD),
    proxy_date_itd_result = coalesce(DateFinalrRTPCRResults, DateFinalResultsSentReflabEPI)
    
    ) #|>
  # select samples collected in 2025 only
  #filter(substr(ICLabID, start = 5, stop = 6) == 25 )

Specify_the_period <- paste0("WEEK 1 - ", 
                             (epiweek(as.Date(ymd(AFPtables$DateUpdated))) - 1) |> unique(), ", 2025")

# Analysis of databases =====
AFPtables_gt35 <- 
AFPtables |>
  filter(LabName != "CDC", !is.na(DateOfOnset)) |>
  #filter(substr(EpidNumber, start = 1, stop = 3) %in% c("DJI", "SOM") == F ) |> #remove somalia and djibouti
  filter( !(LabName == "SOA" & substr(ICLabID, start = 1, stop = 3) %in% c("CIV", "MAD", "RDC", "UGA", "ZAM", "ZIM")) ) |> # remove sequencing data
  select(LabName, DateOfOnset, DateStoolReceivedinLab, StoolCondition, FinalCellCultureResult, DateFinalCellCultureResults,
         proxy_date_infor_itd, FinalITDResult, DateFinalrRTPCRResults, proxy_date_itd_result) |>
  mutate(FinalCellCultureResult = str_replace_all(FinalCellCultureResult, "Supected", "Suspected") ) |>
  #distinct(ICLabID, .keep_all = "TRUE") |>
  group_by(LabName) |>
  mutate(workload_by_lab = n(),
         time_itd_results_35days = as.numeric(difftime(proxy_date_itd_result, DateOfOnset, units = "days")),
         
         is_itd = if_else( (FinalCellCultureResult %in% c("1-Suspected Poliovirus", "4-Suspected Poliovirus + NPENT")), 1, 0),
        
         is_itd_35days = if_else( (FinalCellCultureResult %in% c("1-Suspected Poliovirus", "4-Suspected Poliovirus + NPENT")) &
                                    (!is.na(FinalITDResult) & time_itd_results_35days < 36 & time_itd_results_35days >= 0), 1, 0)
  ) |>
  summarize(
    ITD_results = sum(is_itd, na.rm = TRUE),
    ITD_results_35days = sum(is_itd_35days, na.rm = TRUE),
    Prop_ITD_35days = 100 * ITD_results_35days / ITD_results,
    ) |>
  # For intermediary results
  #summarise(ITD_results_35days = sum(ITD_results_35days) / sum(ITD_results) ) # result of the intermediary result
  dplyr::select(LabName, Prop_ITD_35days)  |>
  pivot_longer(
    cols = starts_with("Prop"),
    names_to = "Metric",
    values_to = "Value" ) |> # drop_na(Value) |>
  ggplot() +
  geom_bar(aes(x = LabName, y = Value, fill = Metric), stat = "identity", position = position_dodge(), width = .9, color = "black") +
  scale_fill_manual(
    values = c("Prop_ITD_35days" = "gold"),
    labels = c("Prop_ITD_35days" = "Among all samples (with results)")
  ) +
  labs(x = "Lab Name", y = "% Samples with results", fill = "", title = "ITD Results by Lab on AFP Isolates") +
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
  )

AFPtables_gt35

# saving the plot as image png  
ggsave("AFPtables35_plot.png", AFPtables_gt35, path = "../data/outputs/", width = 13, height= 6) 







