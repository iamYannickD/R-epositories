#check directory and assign it to a path
path <- getwd()

# Check if the package pacman is installed (pacman Install and load multiple desired packages at once)
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

#install library to import geojson, ggspatial enable R to read and manipulate geojson spatial features
#and ggrepel allows to edit the labels and avoid overlaps
p_load(tidyverse, gt, gtExtras)

#load data

# Classification of countries based on their level of risk
Very_high_risk <- c("Chad", "Democratic Republic of Congo", "Madagascar", "Mozambique", "Niger", "Nigeria")

High_risk	<- c("Algeria", "Angola", "Benin", "Burkina Faso", "Cameroon", "Central African Republic", "Côte d’Ivoire", 
               "Ethiopia", "Kenya", "Malawi", "Mali", "Zambia")

Medium_high_risk <- c("Burundi", "Republic of Congo", "Equatorial Guinea", "Eritrea", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea Bissau", 
                      "Liberia", "Mauritania", "Namibia", "Rwanda", "Senegal", "Sierra Leone", "South Sudan", "Tanzania", 
                      "Togo", "Uganda", "Zimbabwe")
Medium_Risk <- c("")

# Color Categories
breaks <- c(0, 25, 50, 100)
colors <- c("red", "yellow", "darkgreen")

# Create a tibble containing values in the list provided and their risk category
risk_level_by_country <- tibble(
  risk_level = c(rep(" Very High Risk", length(Very_high_risk)),
                 rep("High Risk", length(High_risk)),
                 rep("Medium High Risk", length(Medium_high_risk)),
                 rep("Medium Risk", length(Medium_Risk))),
  country = c(Very_high_risk, High_risk, Medium_high_risk, Medium_Risk) |> str_to_upper()
) |>
  mutate(
    country = str_replace_all(country, c("CHAD" = "TCHAD",
                                         "UNITED REPUBLIC OF TANZANIA" = "TANZANIA",
                                         "CÔTE D’IVOIRE" = "COTE D'IVOIRE",
                                         "GUINEA"  = "GUINEE",
                                         "MAURITANIA" = "MAURITANIE", 
                                         "EQUATORIAL GUINEE" = "EQUATORIAL GUINEA" ))
  )

#recap_es_sites <- 
  read_csv("../data/data_dr/es_sites/ES_site_analysis_jan_jun_2024-07-09.csv") |>
  dplyr::select(Countryname, EV_isolation_Rate_3m, EV_isolation_Rate_6m) |>
    left_join(y = risk_level_by_country, by = c("Countryname" = "country")) |>
    filter(!is.na(risk_level)) |>
    group_by(risk_level, Countryname) |>
    mutate(
      country_3m = if_else(!is.na(EV_isolation_Rate_3m), 1, 0),
      country_3m_50ev = if_else( (!is.na(EV_isolation_Rate_3m) & EV_isolation_Rate_3m  >= 50) , 1, 0),
      country_3m_80ev = if_else( (!is.na(EV_isolation_Rate_3m) & EV_isolation_Rate_3m  >= 80), 1, 0),
      
      country_6m = if_else(!is.na(EV_isolation_Rate_6m), 1, 0),
      country_6m_50ev = if_else( (!is.na(EV_isolation_Rate_6m) & EV_isolation_Rate_6m  >= 50) , 1, 0),
      country_6m_80ev = if_else( (!is.na(EV_isolation_Rate_6m) & EV_isolation_Rate_6m  >= 80), 1, 0),
    ) |>
    summarise(
      `%Q1countries >= 50` = 100 * sum(country_3m_50ev, na.rm = TRUE) / sum(country_3m, na.rm = TRUE),
      `%Q2countries >= 50` = 100 * sum(country_6m_50ev, na.rm = TRUE) / sum(country_6m, na.rm = TRUE),
      
      `%Q1countries >= 80` = 100 * sum(country_3m_80ev, na.rm = TRUE) / sum(country_3m, na.rm = TRUE),
      `%Q2countries >= 80` = 100 * sum(country_6m_80ev, na.rm = TRUE) / sum(country_6m, na.rm = TRUE)
    ) |>
    mutate(
      sparkline_50 = map2( `%Q1countries >= 50`, `%Q2countries >= 50`, ~ list(c(.x, .y))),
      #sparkline_80 = map2_dbl( `%Q1countries >= 80`, `%Q2countries >= 80`, ~ list(c(.x, .y)))
      sparkline_80 = pmap(list(`%Q1countries >= 80`, `%Q2countries >= 80`), ~ c(..1, ..2))
      #sparkline_50 = as.list(paste0( round(`%Q1countries >= 50`, digits = 0), ", ", round(`%Q2countries >= 50`, digits = 0))),
      #sparkline_80 = as.list(paste0( round(`%Q1countries >= 80`, digits = 0), ", ", round(`%Q2countries >= 80`, digits = 0)))
    ) |>
    arrange(risk_level, Countryname) |>
    gt(groupname_col = 'risk_level', rowname_col = 'Countryname') |>
    #gt_color_rows(2:6, palette = c("darkred", "red", "yellow", "green", "darkgreen")) |> # domain = c(0, 0.50), plt =  "RdYlGn"
    #edit some columns names
    cols_label(
      "%Q1countries >= 50" = "Q1, 2024",
      "%Q2countries >= 50" = "Q2, 2024",
      "%Q1countries >= 80" = "Q1, 2024",
      "%Q2countries >= 80" = "Q2, 2024",
      sparkline_50 = "EV Isolation 50% Trend",
      sparkline_80 = "EV Isolation 80% Trend"
    ) |>
    # add percentage in cells
    fmt_number(
      columns = c(`%Q1countries >= 50`, `%Q2countries >= 50`, `%Q1countries >= 80`, `%Q2countries >= 80`),
      decimals = 0,
      pattern = "{x} %"
    ) |> 
      # #color the table based on the values in those cells (`%Q1countries >= 50`)
      # tab_style(
      #   style = cell_fill(color = "#00B050"),
      #   locations = cells_body(
      #     columns = `%Q1countries >= 50`,
      #     rows = `%Q1countries >= 50` >= 50)
      # )  |> 
      # tab_style(
      #   style = cell_fill(color = "yellow"),
      #   locations = cells_body(
      #     columns = `%Q1countries >= 50`,
      #     rows = `%Q1countries >= 50` >= 25 & `%Q1countries >= 50` < 50)
      # ) |>
      # tab_style(
      #   style = cell_fill(color = "#FF0000"),
      #   locations = cells_body(
      #     columns = `%Q1countries >= 50`,
      #     rows = `%Q1countries >= 50` >= 0 & `%Q1countries >= 50` < 25)
      # ) |> 
      # tab_style(
      #   style = cell_fill(color = "#00B050"),
      #   locations = cells_body(
      #     columns = `%Q1countries >= 50`,
      #     rows = `%Q1countries >= 50` == 100)
      # ) |> 
      # Color the table cells by category defined
      data_color(
        columns = c(`%Q1countries >= 50`, `%Q2countries >= 50`, 
                    `%Q1countries >= 80`, `%Q2countries >= 80`),
        fn = scales::col_bin(palette=colors, bins=breaks)
      ) |>
    # Add a nanoplot at the end of the 'group' in the table to show trends of ES Sites >= 50%
    gt::cols_nanoplot(
      columns = contains(">= 50"),
      autohide = FALSE,
      new_col_name = "nanoplots_50",
      
      new_col_label = md("*EV Trend 50%*"),
      before = 5
    ) |> 
    #Add a nanoplot at the end of the 'group' in the table to show trends of ES Sites >= 80%
    # gt::cols_nanoplot(
    #   columns = contains(">= 80"),
    #   autohide = FALSE,
    #   new_col_name = "nanoplots_80",
    #   new_col_label = md("*EV Trend 80%*"),
    #   after = 7
    # ) |>
    #give a header to the table as well as a sub title
    tab_header(
      title = md("**Summary of ES site sensitivity, Q1 & Q2, 2024**"),
      subtitle = md("**Data source : AFRO ES Database**") ) |>
    #add the title that covers specific columns
    tab_spanner(
      label = md('**% of ES sites with ≥50% EV isolation**'),
      columns = 2:5)  |>
    tab_spanner(
      label = md('**% of ES sites with ≥80% EV isolation**'),
      columns = 6:8) |>
    #center the values in the defined columns
    cols_align(
      align = "center",
      columns = c(`%Q1countries >= 50`, `%Q2countries >= 50`, `%Q1countries >= 80`,
                  `%Q2countries >= 80`, nanoplots_50)
    ) |>
    # Hide some unused columns
    cols_hide(
      columns = c(
        sparkline_50,
        sparkline_80
      )
    ) |>
    gt_theme_guardian() |>
    
    # Edition of the guardian theme
    tab_options(
      table.background.color = "white",
      column_labels.background.color = "white",
      table.border.top.color = "white",
    ) |>
    
    opt_align_table_header(align = "center") |>
    #reshape the table
    tab_options(
      data_row.padding = px(2),
      summary_row.padding = px(3), # A bit more padding for summaries
      row_group.padding = px(4)    # And even more for our groups
    ) |>
    tab_style(
      #style = cell_text(weight = "bold"),
      # Color each country in the group
      style = cell_fill(color = "lightgray"),
      locations = cells_row_groups(groups = everything()) 
    ) |>
    opt_css(
      css = "
    .cell-output-display {
      overflow-x: unset !important;
    }
    div#two {
      overflow-x: unset !important;
      overflow-y: unset !important;
    }
    #two .gt_col_heading {
      position: sticky !important;
      top: 0 !important;
    }
    "
    )

  # Create the directory if it doesn't exist
  if (!file.exists( paste0("../data/data_dr/outputs/Recap/"))) {
    dir.create(paste0("../data/data_dr/outputs/Recap/") , recursive = TRUE)
  }
  
  #save as an html file
  gtsave(recap_es_sites, paste0("../data/data_dr/outputs/Recap/", "/recap_es_desk_review.html"), inline_css = TRUE)

  #save as a png file
  gtsave(recap_es_sites, paste0("../data/data_dr/outputs/Recap/", "/recap_es_desk_review.png"), expand = 10
         , vheigh = 14)
  
  #save as a word document
  gtsave(recap_es_sites, paste0("../data/data_dr/outputs/Recap/", "/recap_es_desk_review.docx"))
 
  
  # Add sparklines for 50% EV isolation
    # Add sparklines for 50% EV isolation
  gt_plt_sparkline(
    column = sparkline_50,
    label = "EV Isolation 50% Trend"
  ) |>
  # Add sparklines for 80% EV isolation
  gt_plt_sparkline(
    column = sparkline_80,
    label = "EV Isolation 80% Trend"
  )
     
  
    # Add sparklines
    gt_plt_sparkline(
      data = list(`%Q1countries >= 50`, `%Q2countries >= 50`, `%Q1countries >= 80`, `%Q2countries >= 80`),
      label = "EV Isolation 50% Trend",
      columns = c(`%Q1countries >= 50`, `%Q2countries >= 50`, `%Q1countries >= 80`, `%Q2countries >= 80`)
    )
  
  
    gt_plt_sparkline(
      data80 = list(`%Q1countries >= 80`, `%Q2countries >= 80`),
      label = "EV Isolation 80% Trend",
      columns = c(`%Q1countries >= 80`, `%Q2countries >= 80`)
    )
    
    
    



    gt_sparkline_tab <- mtcars %>%
      dplyr::group_by(cyl) %>%
      # must end up with list of data for each row in the input dataframe
      dplyr::summarize(mpg_data = list(mpg), .groups = "drop") %>%
      gt() %>%
      gt_plt_dist(mpg_data)


    
   
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    