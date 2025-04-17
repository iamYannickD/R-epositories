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
    CountryName == "COTE D'IVOIRE" ~ "IVORY COAST",
    TRUE ~ CountryName
  ))

# 3. Prepare map data -----------------------------------------------------
africa_continent <- ne_countries(continent = "Africa", scale = "medium", returnclass = "sf") %>%
  st_union() %>%
  st_as_sf() %>%
  mutate(name = "Africa")

africa_countries <- ne_countries(continent = "Africa", scale = "medium", returnclass = "sf") %>%
  select(name, geometry) %>%
  mutate(name = case_when(
                      name == "Côte d'Ivoire" ~ "Ivory Coast",
                      name == "Eq. Guinea" ~ "Equatorial Guinea",
                      name == "Central African Rep."~ "Central African Republic",
                      TRUE ~ name) ) |>
  mutate(name = str_to_upper(name)) %>%
  left_join(country_data, by = c("name" = "CountryName")) %>%
  mutate(Number = replace_na(Number, 0))

# 4. Enhanced country counts ----------------------------------------------
country_counts <- country_data %>%
  group_by(MonthYear) %>%
  summarise(
    detecting_countries = n_distinct(CountryName[Number > 0], na.rm = TRUE),
    #total_detections = sum(Number, na.rm = TRUE),
    top_country = CountryName[which.max(Number)],
    top_detections = max(Number, na.rm = TRUE)
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
    
    # Get current month data
    current_monthly <- monthly_data[1:i, ]
    current_top <- monthly_data[i, c("top_country", "top_detections")]
    
    # Prepare country data
    current_countries <- africa_countries %>%
      filter(floor_date(Date, "month") == floor_date(current_date, "month")) %>%
      mutate(
        label = if_else(Number > 0, name, ""),
        is_top = name == current_top$top_country
      )
    
    # Calculate color scale limits
    current_max <- max(current_countries$Number, na.rm = TRUE)
    
    # Enhanced epi curve plot
    epi_plot <- ggplot(current_monthly, aes(x = Date, y = Detections)) +
      geom_col(fill = "steelblue", width = 20) +
      geom_text(aes(label = Detections), vjust = -0.3, color = "black", size = 3.5) +
      geom_vline(xintercept = current_date, color = "red", alpha = 0.7, size = 1) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
      labs(
        title = paste("Detection Timeline:", format(current_date, "%b %Y")),
        subtitle = paste(
          "Detecting countries:", current_monthly$detecting_countries[i], "|",
          #"Total cases:", current_monthly$total_detections[i], "\n",
          "Top Country for the month: ", current_top$top_country, 
          " (", current_top$top_detections, " cases)"
        ),
        x = NULL, y = "Detections"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.subtitle = element_text(size = 11, face = "bold", color = "darkgreen")
      )
    
    # Dynamic map plot
    map_plot <- ggplot() +
      geom_sf(data = africa_continent, fill = "gray90", color = NA) +
      geom_sf(data = current_countries, aes(fill = Number), color = "white", size = 0.2) +
      geom_sf_text(
        data = filter(current_countries, Number > 0 & !is_top),
        aes(label = name), 
        size = 3.2, color = "darkgreen", check_overlap = TRUE
      ) +
      geom_sf_text(
        data = filter(current_countries, is_top),
        aes(label = name), 
        size = 4, color = "red3", fontface = "bold", check_overlap = TRUE
      ) +
      scale_fill_gradientn(
        colors = RColorBrewer::brewer.pal(9, "Greens"),
        limits = c(0, current_max),
        na.value = "grey90"
      ) +
      labs(fill = "Monthly Cases") +
      theme_void() +
      theme(
        #legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))
    
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
