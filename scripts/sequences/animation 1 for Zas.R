install.packages("magick")  # Core image processing
install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source") 

# Load required packages
library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(sf)
library(gganimate)
library(magick)
library(gifski)
library(cowplot) 

# 1. Process and plot epi curve data --------------------------------------
monthly_data <- read_csv("../data/data_sequences/detections by months and years.csv", show_col_types = FALSE) %>%
  mutate(
    # Convert MonthYear to proper date
    Date = my(MonthYear),
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  arrange(Date)

# Create base epi curve plot
epi_plot <- ggplot(monthly_data, aes(x = Date, y = Detections, fill = factor(Year))) +
  geom_col(width = 20) +  # Width for monthly bars
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
  scale_fill_viridis_d(option = "plasma") +
  labs(title = "Outbreak Detection Timeline", 
       x = "Month", y = "Number of Detections",
       fill = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Process and prepare map data -----------------------------------------
country_data <- read_csv("../data/data_sequences/Number of detections by countries and by year.csv", show_col_types = FALSE) %>%
  rename(Year = Attribute) %>%
  mutate(CountryName = case_when(
    CountryName == "COTE D'IVOIRE" ~ "Ivory Coast",
    CountryName == "CENTRAL AFRICAN REPUBLIC" ~ "Central African Republic",
    TRUE ~ CountryName
  ))

# Get world map data and merge with our data
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(name, geometry) %>%
  mutate(name = str_to_upper(name)) |>
  left_join(country_data, by = c("name" = "CountryName"))

# 3. Create combined animation --------------------------------------------
# Create function to generate frames
create_animation_frames <- function() {
  all_frames <- list()
  
  for(i in 1:nrow(monthly_data)) {
    current_date <- monthly_data$Date[i]
    current_year <- year(current_date)
    
    # Filter data up to current date
    filtered_monthly <- monthly_data[1:i, ]
    
    # Get country data for current year
    current_country_data <- world %>%
      filter(Year == current_year) %>%
      replace_na(list(Number = 0))
    
    # Create map plot
    map_plot <- ggplot(current_country_data) +
      geom_sf(aes(fill = Number)) +
      scale_fill_viridis_c(option = "magma", limits = c(0, 600)) +
      labs(title = paste("Detections by Country -", current_year),
           fill = "Detections") +
      theme_void()
    
    # Create epi curve plot
    curve_plot <- epi_plot %+% filtered_monthly +
      geom_col(data = filtered_monthly[1:i, ], aes(x = Date, y = Detections),
               fill = "red", alpha = 0.3, width = 20)
    
    # Combine plots
    combined <- cowplot::plot_grid(curve_plot, map_plot, ncol = 1)
    
    # Save frame
    frame_path <- paste0("frame_", sprintf("%03d", i), ".png")
    ggsave(frame_path, combined, width = 10, height = 12, dpi = 100)
    all_frames[[i]] <- image_read(frame_path)
  }
  
  # Combine frames into GIF
  image_join(all_frames) %>%
    image_animate(fps = 4, optimize = TRUE) %>%
    image_write("../data/data_sequences/outputs/detection_animation.gif")
}

# Run the animation creation (may take several minutes)
create_animation_frames()

# Cleanup temporary frame files
file.remove(list.files(pattern = "frame_.*\\.png"))
