install.packages(c("tidyverse", "lubridate", "rnaturalearth", "sf", 
                   "gganimate", "magick", "cowplot", "gifski"))


# Load required packages
library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(sf)
library(gganimate)
library(magick)
library(cowplot)
library(gifski)

# Set output path
output_path <- "../data/data_sequences/outputs/detection_animation.gif"

# 1. Process monthly data -------------------------------------------------
monthly_data <- read_csv("../data/data_sequences/detections by months and years.csv", show_col_types = FALSE)|>
  mutate(
    Date = my(MonthYear),
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  )|>
  arrange(Date)|>
  # Create animation timeline
  mutate(frame_time = row_number())

# 2. Process country data -------------------------------------------------
country_data_old <- read_csv("../data/data_sequences/Number of detections by countries and by year.csv", show_col_types = FALSE)|>
  rename(Year = Attribute)|>
  # Harmonize country names with map data
  mutate(CountryName = case_when(
    CountryName == "COTE D'IVOIRE" ~ "Ivory Coast",
    CountryName == "CENTRAL AFRICAN REPUBLIC" ~ "Central African Republic",
    CountryName == "GAMBIA" ~ "The Gambia",
    TRUE ~ CountryName
  ))


country_data <- read_csv("../data/data_sequences/Detections by countries and by month - year.csv", show_col_types = FALSE)|>
  mutate(
    Date = my(MonthYear),
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) |>
  # Harmonize country names with map data
  mutate(CountryName = case_when(
    CountryName == "COTE D'IVOIRE" ~ "Ivory Coast",
    CountryName == "CENTRAL AFRICAN REPUBLIC" ~ "Central African Republic",
    CountryName == "GAMBIA" ~ "The Gambia",
    TRUE ~ CountryName
  ))

# 3. Get Africa map data --------------------------------------------------
africa <- ne_countries(continent = "Africa", scale = "medium", returnclass = "sf")|>
  select(name, geometry)|> 
  mutate(name = str_to_upper(name)) |>
  left_join(country_data, by = c("name" = "CountryName"))|>
  mutate(Number = replace_na(Number, 0))


# 4. Create animation function --------------------------------------------
create_animation <- function() {
  # Create output directory if needed
  if(!dir.exists(dirname(output_path))) {
    dir.create(dirname(output_path), recursive = TRUE)
  }
  
  all_frames <- list()
  
  for(i in 1:nrow(monthly_data)) {
    current_date <- monthly_data$Date[5]
    current_year <- monthly_data$Year[5]
    current_month <- monthly_data$Year[5]
    
    # Filter to current timeframe
    current_monthly <- monthly_data[1:i, ]
    
    # Get country data for current year
    current_countries <- africa|>
      filter(Year == current_year)|>
      filter(Month == current_month)|>
      mutate(Number = replace_na(Number, 0))
    
    # Create epi curve with progress line
    epi_plot <- ggplot(current_monthly, aes(x = Date, y = Detections)) +
      geom_col(fill = "steelblue", width = 20) +
      geom_vline(xintercept = current_date, color = "red", alpha = 0.7, size = 1) +
      scale_x_date(date_breaks = "1 months", date_labels = "%b-%y") + # 3 months to 1 month
      labs(title = paste("Detection Timeline:", format(current_date, "%b %Y")),
           x = NULL, y = "Detections") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Create synchronized map
    map_plot <- ggplot(current_countries) +
      geom_sf(aes(fill = Number)) +
      scale_fill_viridis_c(option = "magma", limits = c(0, 600)) +
      labs(title = "Country Detections") +
      theme_void() #+
      #theme(legend.position = "bottom")
    
    # Combine plots vertically
    combined <- cowplot::plot_grid(epi_plot, map_plot, ncol = 1, rel_heights = c(1, 2))
    
    # Save frame
    frame_path <- tempfile(fileext = ".png")
    ggsave(frame_path, combined, width = 12, height = 10, dpi = 100)
    all_frames[[i]] <- magick::image_read(frame_path)
  }
  
  # Create animation
  animation <- magick::image_join(all_frames)|>
    magick::image_animate(fps = 4, optimize = TRUE)
  
  # Save to specified path
  magick::image_write(animation, output_path)
}

# 5. Execute the animation creation ---------------------------------------
create_animation()
