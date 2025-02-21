# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages =====
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
p_load(tidyverse, RODBC,gt, gtExtras, webshot, officer)

#Give the path to the AFP database
path_AFP <- "../data/dbs/AFP2025.mdb" 

# Connect to the Microsoft Access database =====
AFPdb <- DBI::dbConnect(odbc::odbc(), 
                        .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                              DBQ=", path_AFP))
# load data in R =====
# Retrieve all data from the AFP database
AFPtables <- DBI::dbGetQuery(AFPdb, "SELECT * FROM POLIOLAB ORDER BY LabName, EpidNumber;", stringsAsFactors = FALSE) |>
  tibble() |>  mutate(proxy_date_cellculture = coalesce(DateFinalCellCultureResults, DateFinalResultsSentNatLevel),
                      proxy_date_itd_result = coalesce(DateFinalrRTPCRResults, DateFinalResultsSentReflabEPI)) #|>
  
  # select samples collected in 2025 only
  #filter(substr(ICLabID, start = 5, stop = 6) == 24 )

Specify_the_period <- paste0("WEEK 1 - ", 
                             (epiweek(as.Date(ymd(AFPtables$DateUpdated))) - 1) |> unique(), ", 2025")

# Analysis of databases =====
AFPtables_14 <- 
AFPtables |>
  filter(LabName != "CDC") |>
  #filter(substr(EpidNumber, start = 1, stop = 3) %in% c("DJI", "SOM") == F) |> #remove somalia and djibouti
  select(LabName, DateStoolReceivedinLab, StoolCondition, FinalCellCultureResult, DateFinalCellCultureResults,
         FinalITDResult, DateFinalrRTPCRResults, proxy_date_cellculture) |>
  mutate( FinalCellCultureResult = str_replace_all(FinalCellCultureResult, "Supected", "Suspected") ) |>
  #distinct(ICLabID, .keep_all = "TRUE") |>
  group_by(LabName) |>
  mutate(workload_by_lab = n(),
         time_cell_culture_results_14days = as.numeric(difftime(proxy_date_cellculture, DateStoolReceivedinLab, units = "days")),

         is_cell_culture_positive = if_else( (FinalCellCultureResult %in% c("1-Suspected Poliovirus", "4-Suspected Poliovirus + NPENT")), 1, 0),
         is_cell_culture_14days = if_else( (!is.na(FinalCellCultureResult) & time_cell_culture_results_14days < 15 & time_cell_culture_results_14days >= 0), 1, 0),
         is_cell_culture_14days_positive_sample = if_else( (FinalCellCultureResult %in% c("1-Suspected Poliovirus", "4-Suspected Poliovirus + NPENT")) &
                                                     (!is.na(FinalCellCultureResult) & time_cell_culture_results_14days < 15 & time_cell_culture_results_14days >= 0), 1, 0)
         ) |>
  summarize(
    #cell_culture_results = sum(is_itd, na.rm = TRUE),
    cell_culture_results = sum(!is.na(FinalCellCultureResult)),
    cell_culture_results_positive_sample = sum(is_cell_culture_positive, na.rm = TRUE),
    cell_culture_results_14days = sum(is_cell_culture_14days, na.rm = TRUE),
    cell_culture_results_14days_positive_sample = sum(is_cell_culture_14days_positive_sample, na.rm = TRUE),
    Prop_cell_culture_14days = 100 * cell_culture_results_14days / cell_culture_results,
    Prop_cell_culture_14days_positive_sample = 100 * cell_culture_results_14days_positive_sample / cell_culture_results_positive_sample
        ) |>
  # Intermediary results
    # summarise(
    #   prop_lab_res_14days_all_samples = sum(cell_culture_results_14days, na.rm = TRUE) / sum(cell_culture_results, na.rm = TRUE),
    #   prop_lab_res_14days_positive_samples = sum(cell_culture_results_14days_positive_sample, na.rm = TRUE) / sum(cell_culture_results_positive_sample, na.rm = TRUE))

    dplyr::select(LabName, Prop_cell_culture_14days, Prop_cell_culture_14days_positive_sample)  |>
    pivot_longer(
    cols = starts_with("Prop"),
    names_to = "Metric",
    values_to = "Value" ) |> # drop_na(Value) |>
    ggplot() +
    geom_bar(aes(x = LabName, y = Value, fill = Metric), stat = "identity", position = position_dodge(), width = .9, color = "black") +
    scale_fill_manual(
      values = c("Prop_cell_culture_14days" = "gold", "Prop_cell_culture_14days_positive_sample" = "darkblue"),
      labels = c("Prop_cell_culture_14days" = "Among all samples (with results)", "Prop_cell_culture_14days_positive_sample" = "Among positive samples")
      ) +
    labs(x = "Lab Name", y = "% Samples with results", fill = "", title = "Timeliness Cell Culture Results by Lab (14 days)") +
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

AFPtables_14

# saving the plot as image png  
ggsave("AFPtables14_plot.png", AFPtables_14, path = "../data/outputs/", width = 12, height= 6)   





  