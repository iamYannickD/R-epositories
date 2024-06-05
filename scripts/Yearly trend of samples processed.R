# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Install and load multiple desired packages at once
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
# patch work to easily combine and display ggplot graphs
p_load(tidyverse, RODBC, ggrepel, patchwork)

# import dataset
workload <- read_csv("../data/data/yr_sample_workloads.csv")

#Give the path to the AFP database
path_AFP = "../data/dbs/afp_wk21.mdb" 
path_ES_2024 = "../data/dbs/es_2024.mdb"

# Connect to the Microsoft Access database
AFPdb <- DBI::dbConnect(odbc::odbc(), 
                        .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                              DBQ=", path_AFP))

ESdb2024 <- DBI::dbConnect(odbc::odbc(), 
                           .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                              DBQ=", path_ES_2024))

# load data in R
# Retrieve all data from the AFP database
AFPtables <- DBI::dbGetQuery(AFPdb, "SELECT * FROM POLIOLAB ORDER BY LabName, EpidNumber;", stringsAsFactors = FALSE) |>
  tibble() |>
  # select samples collected in 2024 only
  filter(substr(ICLabID, start = 5, stop = 6) == 24 )

EStables2024 <- DBI::dbGetQuery(ESdb2024, "SELECT * FROM Environmental ORDER BY IDNumber;", stringsAsFactors = FALSE) |>
  as_tibble()

Specify_the_period <- paste0("WEEK 1 - ", 
                             (epiweek(as.Date(ymd_hms(AFPtables$DateUpdated))) - 1) |> unique(), ", 2024")

# count samples processed by labs
#Sum of AFP samples
afp_total_samples <- 
  AFPtables |>
  filter(LabName != "CDC") |>
  distinct(ICLabID, .keep_all = TRUE) |>
  group_by(LabName) |>
  summarise(afp_workload_by_lab = n()) |>
  ungroup() |>
  summarise(afp_total = sum(afp_workload_by_lab)) 

#Sum of AFP samples
es_total_samples <- 
  #bind_rows(EStables2023, EStables2024) |>
  EStables2024 |>
  distinct(IDNumber, .keep_all = TRUE) |>
  group_by(Labname) |>
  summarise(es_workload_by_lab = n()) |>
  ungroup() |>
  summarise(es_total = sum(es_workload_by_lab))


# New rows to add
dynamic_rows <- tibble(
  TYPE = c("AFP", "ES"),
  YEAR = 2024,
  `# of Samples` = c( as.integer(afp_total_samples), as.integer(es_total_samples))
)

# Bind the new rows to the original tibble
afro_workload <- bind_rows(workload, dynamic_rows)

plot_AFP <-
  afro_workload |>
  filter(TYPE == "AFP") |>
  ggplot() +
  geom_line( aes(x = YEAR, y = `# of Samples`), color = "orange2", size = 2) +
  geom_ribbon(aes(x = YEAR, ymin = 0, ymax = `# of Samples`, xmin = 2018.75, xmax = 2024.5),
              fill = "orange4", alpha = 0.3) +
  geom_point(aes(x = YEAR, y = `# of Samples`), size = 3, color = "black") +
  geom_text_repel(aes(x = YEAR, y = `# of Samples`, label = `# of Samples` ),
                  min.segment.length = Inf, seed = 42, box.padding = 0.5,
                  point.padding = 0, position = position_nudge_repel(y = 2000)) +
  #color = "white",bg.color = "grey30", bg.r = 0.15, direction = "y" +
  
  labs(title = paste0("Workload Analysis for AFP as of ", Specify_the_period), x = "Year", y = "# of Samples")  +
  theme_minimal()

plot_ES <-
  afro_workload |>
  filter(TYPE == "ES") |>
  ggplot() +
  geom_line( aes(x = YEAR, y = `# of Samples`), color = "blue4", size = 2) +
  geom_ribbon(aes(x = YEAR, ymin = 0, ymax = `# of Samples`, xmin = 2018.75, xmax = 2024.5),
              fill = "lightblue", alpha = 0.3) +
  geom_point(aes(x = YEAR, y = `# of Samples`), size = 3, color = "black") +
  geom_text_repel(aes(x = YEAR, y = `# of Samples`, label = `# of Samples` ),
                  min.segment.length = Inf, seed = 42, box.padding = 0.5,
                  point.padding = 0, position = position_nudge_repel(y = 500)) +
  #color = "white",bg.color = "grey30", bg.r = 0.15, direction = "y" +
  
  labs(title = paste0("Workload Analysis for ES as of ", Specify_the_period), x = "Year", y = "# of Samples")  +
  theme_minimal()

combined <- plot_AFP / plot_ES
combined



