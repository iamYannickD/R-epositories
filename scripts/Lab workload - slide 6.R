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
path_AFP = "data/afp.mdb" 
path_ES_2024 = "data/es_2024.mdb"

# Connect to the Microsoft Access database
AFPdb <- DBI::dbConnect(odbc::odbc(), 
                        .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                              DBQ=", path_AFP))

ESdb2024 <- DBI::dbConnect(odbc::odbc(), 
                           .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                              DBQ=", path_ES_2024))

Specify_the_period <- paste0("WEEK 1 - ", 
                             (epiweek(as.Date(ymd_hms(AFPtables$DateUpdated))) - 1) |> unique(), ", 2024")

# load data in R
# Retrieve all data from the AFP database
AFPtables <- DBI::dbGetQuery(AFPdb, "SELECT * FROM POLIOLAB ORDER BY LabName, EpidNumber;", stringsAsFactors = FALSE) |>
  tibble() |>
  # select samples collected in 2024 only
  filter(substr(ICLabID, start = 5, stop = 6) == 24 )

EStables2024 <- DBI::dbGetQuery(ESdb2024, "SELECT * FROM Environmental ORDER BY IDNumber;", stringsAsFactors = FALSE) |>
  as_tibble()

# bar chart of samples processed by labs
AFP_plot <-
  AFPtables |>
  #filter( AFPtables$LabName != "CDC" & year(AFPtables$DateOfOnset) > 2023 ) |>
  filter( AFPtables$LabName != "CDC") |>
  distinct(ICLabID, .keep_all = "TRUE") |>
  group_by(LabName) |>
  summarise(afp_workload_by_lab = n()) |>
  ungroup() |>
  #summarise(total_workload = sum(workload_by_lab)) |>
  ggplot() +
  geom_bar(aes(x = LabName, y = afp_workload_by_lab, fill = "darkblue"), fill = "darkblue", stat = "identity") + 
  geom_text(aes(x = LabName, y = afp_workload_by_lab, label = afp_workload_by_lab), vjust = -0.5, size = 3) +
  #geom_label(mapping = LabName, data = AFPtables, stat = "identity") +
  labs(x = "AFRO Polio Labs", y = "Number of AFP Specimens", title = "AFP and other Human Samples" ) +
  theme_classic()


ES_plot <-
  EStables2024 |>
  dplyr::mutate(
    Labname = str_replace_all(Labname, c("ENTEBBE" = "UGA", "GHANA" = "GHA", "IBD, Nigeria" = "IBD",
                                         "INRB" = "RDC", "IPD SEN" = "SEN", "IPM,MAD" = "MAD",
                                         "IPM, MAD" = "MAD", "MDG, Nigeria" = "MDG", 
                                         "KEMRI" = "KEN", "ZAM-UTH" = "ZAM", "ZAM UTH" = "ZAM")
    ) ) |>
  arrange(Labname) |>
  distinct(IDNumber, .keep_all = "TRUE") |>
  group_by(Labname) |>
  summarise(es_workload_by_lab = n()) |>
  ungroup() |>
  ggplot() +
  geom_bar(aes(x = Labname, y = es_workload_by_lab, fill = "orange"), fill = "orange", stat = "identity") + 
  geom_text(aes(x = Labname, y = es_workload_by_lab, label = es_workload_by_lab), vjust = -0.5, size = 3) +
  #geom_label(mapping = LabName, data = AFPtables, stat = "identity") +
  labs(x = "AFRO Polio Labs", y = "Number of ES Specimens", title = "ES Samples" ) +
  theme_classic()


  





























