# Check if the package pacman is installed (pacman Install and load multiple desired packages at once)
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

#load libraries
p_load(tidyverse, gt, gtExtras)

# load data
es_sites <- read_csv("../data/data_dr/es_sites/ES_performance_by Year_1.csv") 

# Classification of countries based on their level of risk
Very_high_risk <- c("Chad", "Democratic Republic of Congo", "Madagascar", "Mozambique", "Niger", "Nigeria")

High_risk	<- c("Algeria", "Angola", "Benin", "Burkina Faso", "Cameroon", "Central African Republic", "Cote dIvoire", 
               "Ethiopia", "Kenya", "Malawi", "Mali", "Zambia")

Medium_high_risk <- c("Burundi", "Republic of Congo", "Equatorial Guinea", "Eritrea", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea Bissau", 
                      "Liberia", "Mauritania", "Namibia", "Rwanda", "Senegal", "Sierra Leone", "South Sudan", "Tanzania", 
                      "Togo", "Uganda", "Zimbabwe")

Medium_Risk <- c("Botswana", "Cabo Verde", "Eswatini", "Lesotho", "Mauritius", "Mauritius", "Seychelles", "South Africa")

# Color Categories
breaks <- c(0, 50, 80, 100)
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
    country = str_replace_all(country, c("UNITED REPUBLIC OF TANZANIA" = "TANZANIA",
                                         "CÔTE D’IVOIRE" = "COTE D'IVOIRE",
                                         "GUINEA"  = "GUINEE",
                                         #"MAURITANIA" = "MAURITANIE", 
                                         "EQUATORIAL GUINEE" = "EQUATORIAL GUINEA" ))
  )

recap_es_sites <- 
  es_sites |>
  dplyr::select(Countryname = Country, "2020", "2021", "2022", "2023", "2024") |>
    left_join(y = risk_level_by_country, by = c("Countryname" = "country")) |>
    filter(!is.na(risk_level)) |>
    group_by(risk_level, Countryname) |>
    mutate(
          sparkline_50 = map2( `2020`, `2021`, `2022`, `2023`, `2024`, ~ list(c(.x, .y))),
    ) |>
    arrange(risk_level, Countryname) |>
    gt(groupname_col = 'risk_level', rowname_col = 'Countryname') |>
    #gt_color_rows(2:6, palette = c("darkred", "red", "yellow", "green", "darkgreen")) |> # domain = c(0, 0.50), plt =  "RdYlGn"
    #edit some columns names
    cols_label(
      "2020" = "2020",
      "2021" = "2021",
      "2022" = "2022",
      "2023" = "2023",
      "2024" = "2024",
      sparkline_50 = "EV Isolation 50% Trend",
    ) |>
    # add percentage in cells
    fmt_number(
      columns = c(`2020`, `2021`, `2022`, `2023`, `2024`),
      decimals = 0,
      pattern = "{x} %"
    ) |> 
    summary_rows(
      groups = everything(),
      columns = c(`2020`, `2021`, `2022`, `2023`, `2024`),
      fns = list(
        "SUB TOTAL (MEAN)" = ~ mean(.x, na.rm = TRUE)
      ),
      formatter = fmt_number,
      decimals = 0
    ) |>
    grand_summary_rows(
      columns = c(`2020`, `2021`, `2022`, `2023`, `2024`),
      fns = list(
        "GRAND TOTAL (MEAN)" = ~ mean(.x, na.rm = TRUE)
      ),
      formatter = fmt_number,
      decimals = 0
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
        columns = c(`2020`, `2021`, `2022`, `2023`, `2024`),
        fn = scales::col_bin(palette=colors, bins=breaks)
      ) |>
    # Add a nanoplot at the end of the 'group' in the table to show trends of ES Sites >= 50%
    gt::cols_nanoplot(
      columns = c(`2020`, `2021`, `2022`, `2023`, `2024`),
      autoscale = TRUE,
      autohide = FALSE,
      #plot_type = "bar",
      new_col_name = "nanoplots_50",
      new_col_label = md("EV Trend 50%"),
      before = 7
    ) |>
    #Add a nanoplot at the end of the 'group' in the table to show trends of ES Sites >= 80%
    # gt::cols_nanoplot(
    #   columns = contains(">= 80"),
    #   #columns = c(`%Q1countries >= 80`, `%Q2countries >= 80`, `%Q3countries >= 80`),
    #   #autoscale = TRUE,
    #   autohide = FALSE,
    #   new_col_name = "nanoplots_80",
    #   new_col_label = md("*EV Trend 80%*"),
    #   #options = nanoplot_options_list,
    #   before = 11
    # ) |>
    #give a header to the table as well as a sub title
    tab_header(
      title = md("**ES Country Performance from 2020 to 2024**"),
      subtitle = md("**Data source : AFRO ES Database**") ) |>
    #add the title that covers specific columns
    tab_spanner(
      label = md('**% of ES sites with ≥50% EV isolation**'),
      columns = 2:8)  |>
    #center the values in the defined columns
    cols_align(
      align = "center",
      columns = c(`2020`, `2021`, `2022`, `2023`, `2024`, nanoplots_50) #nanoplots_80)
    ) |>
    # Hide some unused columns
    cols_hide(
      columns = c(
        sparkline_50
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
      style = cell_fill(color = "darkgray"),
      locations = cells_row_groups(groups = everything()) 
    ) |>
    # adjust background color of summary tables
    tab_style(
      style = list(
        cell_fill(color = "darkgray"),
        cell_text(weight = "bold")
      ),
      locations = cells_grand_summary()
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

recap_es_sites

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
 
  
    
   
  
    
    
    
    
    
    
    
    
    
    