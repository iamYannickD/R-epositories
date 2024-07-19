#check directory
getwd()

# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Install and load multiple desired packages at once
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
# patch work to easily combine and display ggplot graphs
p_load(tidyverse, RODBC, patchwork)

#Give the path to the AFP database
path_AFP <- "../data/dbs/wk_24/afp_wk_24.mdb" 
labname <- "CAE" # Replace with the actual labname you want to filter by

# Connect to the Microsoft Access database
AFPdb <- DBI::dbConnect(odbc::odbc(), 
                        .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                              DBQ=", path_AFP))

# load data in R
# Retrieve all data from the AFP database
AFPtables <- DBI::dbGetQuery(AFPdb, "SELECT * FROM POLIOLAB ORDER BY LabName, EpidNumber;", stringsAsFactors = FALSE) |>
  tibble() |>
  # select samples collected in 2024 only
  filter(substr(ICLabID, start = 5, stop = 6) == 24 )

Specify_the_period <- paste0("WEEK 1 - ", 
                             (epiweek(as.Date(ymd_hms(AFPtables$DateUpdated))) - 1) |> unique(), ", 2024")

# bar chart of samples processed by labs
#AFP_plot <-
  AFPtables |>
  filter( AFPtables$LabName != "CDC") |>
  distinct(ICLabID, .keep_all = "TRUE") |>
  filter(LabName == labname) |>
  mutate(
    CountryCode = substr(EpidNumber, start = 1, stop = 3), .after = LabName,
    DateStoolReceivedinLab = as.Date(DateStoolReceivedinLab),
    Month = factor(month(DateStoolReceivedinLab, label = TRUE))
    ) |>
  group_by(CountryCode, Month) |>
  summarise(afp_workload_by_lab = n()) |>
  #arrange(Month, afp_workload_by_lab) |>
  ungroup() |>
    group_by(Month) |>
    mutate(total_workload = sum(afp_workload_by_lab)) |>
    arrange(Month, total_workload) |>
    ungroup() |>
  ggplot() +
  geom_bar(aes(x = Month, y = afp_workload_by_lab, fill = CountryCode), colour = "black", stat = "identity", position = position_stack(reverse = TRUE)) + 
  geom_text(aes(x = Month, y = total_workload, label = total_workload), size = 4, fontface = "bold", vjust = -0.5, nudge_y = -0.5) +
  geom_text(aes(x = Month, y = afp_workload_by_lab, label = afp_workload_by_lab), size = 3, fontface = "bold", position = position_stack(vjust = 0.5) ) + #vjust = 0.5
    #scale_colour_brewer(palette = "PiYG") +
    scale_fill_brewer(palette = "Pastel1") +
  labs(x = " ", y = "Number of AFP Samples Processed", title = paste0("Samples processed by Month and by Countries in ", labname), fill = "Countries" ) +
  theme_classic() + 
  theme(
    axis.text = element_text(face = "bold", size = 10, color = "black"),
    axis.title = element_text(face = "bold", size = 12, color = "black")
  )
