# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages =====
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
p_load(tidyverse, RODBC,gt, gtExtras)

#Give the path to the ES database
Specify_the_period <- "WEEK 1 - 28, 2024"
path_ES_2024 = "../data/dbs/ES_160724.mdb"
labname <- "CAE" # Replace with the actual labname you want to filter by

# load data in R ====
ESdb2024 <- DBI::dbConnect(odbc::odbc(), 
                           .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                              DBQ=", path_ES_2024))

# Retrieve all data from the AFP database
EStables2024 <- DBI::dbGetQuery(ESdb2024, "SELECT * FROM Environmental ORDER BY IDNumber;", stringsAsFactors = FALSE) |>
  as_tibble()

# bar chart of samples processed by labs
ES_plot <-
  EStables2024 |>
  dplyr::mutate(
    Labname = str_replace_all(Labname, c("ENTEBBE" = "UGA", "GHANA" = "GHA", "IBD, Nigeria" = "IBD",
                                         "INRB" = "RDC", "IPD SEN" = "SEN", "IPM,MAD" = "MAD",
                                         "IPM, MAD" = "MAD", "MDG, Nigeria" = "MDG", "ESWATINI" = "SOA",
                                         "KEMRI" = "KEN", "ZAM-UTH" = "ZAM", "ZAM UTH" = "ZAM") ),
    Labname = if_else( (is.na(Labname) & Countryname == "ANGOLA"), "SOA", Labname)
  ) |>
  filter(Labname == labname) |>
  mutate(
      Datesampleinlab = as.Date(Datesampleinlab),
      Month = factor(month(Datesampleinlab, label = TRUE))
    ) |>
  group_by(Countrycode, Month) |>
  summarise(es_workload_by_lab = n()) |>
  ungroup() |>
    group_by(Month) |>
    mutate(total_workload = sum(es_workload_by_lab)) |>
    arrange(Month, total_workload) |>
    ungroup() |>
  ggplot() +
  geom_bar(aes(x = Month, y = es_workload_by_lab, fill = Countrycode), colour = "black", stat = "identity", position = position_stack(reverse = TRUE)) + 
  geom_text(aes(x = Month, y = es_workload_by_lab, label = es_workload_by_lab), size = 3, fontface = "bold", position = position_stack(vjust = 0.5)) +
  geom_text(aes(x = Month, y = total_workload, label = total_workload), size = 4, fontface = "bold", vjust = -0.5) +
    #scale_fill_brewer(palette = "Pastel1") +
  
    #geom_label(mapping = LabName, data = AFPtables, stat = "identity") +
  labs(x = " ", y = "Number of ES Samples Processed", title = paste0("Samples processed by Month and by Countries in ", labname), fill = "Countries" ) +
  theme_classic() +
  theme(
    axis.text = element_text(face = "bold", size = 10, color = "black"),
    axis.title = element_text(face = "bold", size = 12, color = "black")
  )
  

  # saving the plot as image png  
  ggsave("es_slide1.png", ES_plot, path = "../data/outputs_lab_ass/", width = 8, height= 8) 
  
  
  
  
  
  
  
  