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
monthly_data <- read_csv("../data/data_sequences/detections by months and years.csv", show_col_types = FALSE) %>%
  mutate(
    Date = my(MonthYear),
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  arrange(Date) %>%
  mutate(frame_time = row_number())

# 2. Process country data -------------------------------------------------
country_data <- read_csv("../data/data_sequences/Detections by countries and by month - year.csv", show_col_types = FALSE) %>%
  mutate(
    Date = my(MonthYear),
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  mutate(CountryName = case_when(
    CountryName == "COTE D'IVOIRE" ~ "Ivory Coast",
    CountryName == "CENTRAL AFRICAN REPUBLIC" ~ "Central African Republic",
    CountryName == "GAMBIA" ~ "The Gambia",
    TRUE ~ CountryName
  ))

# 3. Prepare map data -----------------------------------------------------
# Get full African continent outline
africa_continent <- ne_countries(continent = "Africa", scale = "medium", returnclass = "sf") %>%
  st_union() %>%
  st_as_sf() %>%
  mutate(name = "Africa")

# Country-level data
africa_countries <- ne_countries(continent = "Africa", scale = "medium", returnclass = "sf") %>%
  select(name, geometry) %>%
  mutate(name = str_to_upper(name)) %>%
  left_join(country_data, by = c("name" = "CountryName")) %>%
  mutate(Number = replace_na(Number, 0))

# 4. Calculate country counts ---------------------------------------------
country_counts <- country_data %>%
  group_by(MonthYear) %>%
  summarise(
    detecting_countries = n_distinct(CountryName[Number > 0], na.rm = TRUE),
    total_detections = sum(Number, na.rm = TRUE)
  ) %>%
  mutate(Date = my(MonthYear))

monthly_data <- monthly_data %>%
  left_join(country_counts, by = "Date")

# 5. Enhanced animation function ------------------------------------------
create_animation <- function() {
  if(!dir.exists(dirname(output_path))) {
    dir.create(dirname(output_path), recursive = TRUE)
  }
  
  all_frames <- list()
  
  for(i in 1:nrow(monthly_data)) {
    current_date <- monthly_data$Date[i]
    current_month_year <- format(current_date, "%b-%Y")
    
    # Filter to current timeframe
    current_monthly <- monthly_data[1:i, ]
    
    # Get country data for current month
    current_countries <- africa_countries %>%
      filter(floor_date(Date, "month") == floor_date(current_date, "month")) %>%
      mutate(
        detection_status = if_else(Number > 0, "Detecting", "No Detection"),
        label = if_else(Number > 0, name, "")
      )
    
    # Create epi curve with counter
    epi_plot <- ggplot(current_monthly, aes(x = Date, y = Detections)) +
      geom_col(fill = "steelblue", width = 20) +
      geom_vline(xintercept = current_date, color = "red", alpha = 0.7, size = 1) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
      labs(
        title = paste("Detection Timeline:", format(current_date, "%b %Y")),
        subtitle = paste(
          "Countries detecting:", current_monthly$detecting_countries[i], "|",
          "Total detections:", current_monthly$total_detections[i]
        ),
        x = NULL, y = "Detections"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.subtitle = element_text(size = 12, face = "bold")
      )
    
    # Create enhanced map
    map_plot <- ggplot() +
      # Continent base layer
      geom_sf(data = africa_continent, fill = "gray90", color = NA) +
      # Country detections
      geom_sf(data = current_countries, aes(fill = Number), color = "white", size = 0.2) +
      # Country labels
      geom_sf_text(
        data = filter(current_countries, Number > 0),
        aes(label = name), 
        size = 3,
        color = "darkgreen",
        check_overlap = TRUE
      ) +
      scale_fill_gradientn(
        colors = RColorBrewer::brewer.pal(9, "Greens"),
        limits = c(0, 600),
        na.value = "grey90"
      ) +
      labs(fill = "Cases") +
      theme_void() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold")
      )
    
    # Combine plots
    combined <- plot_grid(epi_plot, map_plot, ncol = 1, rel_heights = c(1, 2))
    
    # Save frame
    frame_path <- tempfile(fileext = ".png")
    ggsave(frame_path, combined, width = 14, height = 12, dpi = 100)
    all_frames[[i]] <- image_read(frame_path)
  }
  
  # Create and save animation
  image_join(all_frames) %>%
    image_animate(fps = 4, optimize = TRUE) %>%
    image_write(output_path)
}

# 6. Execute the animation creation ---------------------------------------
create_animation()
