# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages =====
#RODBC to be able to work with microsoft access databases, allowing R to connect to Open Database Connectivity (ODBC) APIs
p_load(tidyverse, officer)

#Give the path to the csv error file
AFP_errors <- read.xlsx("../data/data/data_errors.xlsx", sheet = 1)

#data1 <-
AFP_errors_old |>
  pivot_longer(cols = !Cat.errors, names_to = "Months", values_to = "Proportion") |>
  ggplot() +
  geom_line(aes(x = Months, y = Proportion, color = Cat.errors, group = Cat.errors), size = 1) +
  geom_point(aes(x = Months, y = Proportion), size = 3) +
  geom_text(aes(x = Months, y = Proportion, label = scales::percent(Proportion / 100, accuracy = 0.1)), vjust = -0.5, size = 3.5) +
  theme_minimal() +
  labs(x = "Month", y = "Proportion of Errors (%)", title = "Proportion of Errors Over Time") +
  theme(axis.title.x = element_text(face = "bold", family = "Arial Black"),
        axis.title.y = element_text(face = "bold", family = "Arial Black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))


  # Define the order of the months
  month_order <- c("February_23", "March_23", "April_23", "May_23", "June_23", "July_23", "August_23", "September_23",
                   "October_23", "November_23", "December_23", "January_24", "February_24")
  
  # Convert Months to a factor with the correct order
  AFP_errors_long <- AFP_errors |>
    pivot_longer(cols = !Cat.errors, names_to = "Months", values_to = "Proportion") |>
    mutate(Months = factor(Months, levels = month_order))
  
  # Create the plot
    ggplot(AFP_errors_long, aes(x = Months, y = Proportion / 100, color = Cat.errors, group = Cat.errors)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    geom_text(aes(label = scales::percent(Proportion / 100, accuracy = 0.01)), 
              vjust = -0.5, size = 3.5) +
    theme_minimal() +
    labs(x = "Month", y = "Proportion of Errors (%)", title = "Proportion of Errors Over Time") +
    theme(axis.title.x = element_text(face = "bold", family = "Arial Black"),
          axis.title.y = element_text(face = "bold", family = "Arial Black"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank(),
          legend.position = "bottom") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  

