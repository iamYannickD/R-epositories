# libraries
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

p_load(tidyverse, gt, gtExtras, sf, readxl, geojsonsf, ggspatial, ggrepel, raster, zoo)

# load data
Table_Regional <- read_csv("../data/data_sequences/analysis/Table_Regional_Overview.csv")

# Nettoyer la date
Table_Regional <- Table_Regional %>%
  mutate(
    COLLECTION_DATE = dmy(COLLECTION_DATE),
    YEAR = year(COLLECTION_DATE),
    SOURCE_CLEAN = case_when(
      SOURCE == "AFP" ~ "AFP",
      SOURCE == "ENV" ~ "ENV",
      TRUE ~ "Other"
    )
  )

df_counts <- Table_Regional %>%
  filter(YEAR %in% 2016:2025) %>%
  group_by(SEROTYPE, COUNTRY_NAME, SOURCE_CLEAN, YEAR) %>%
  summarise(n = n(), .groups = "drop") |>
  arrange(SOURCE_CLEAN, YEAR)

df_wide <- df_counts %>%
  pivot_wider(names_from = c(SOURCE_CLEAN, YEAR), values_from = n, values_fill = 0)

last_date <- Table_Regional %>%
  mutate(YEAR = year(COLLECTION_DATE)) %>%
  filter(YEAR %in% 2016:2025) %>%
  group_by(SEROTYPE, COUNTRY_NAME, SOURCE_CLEAN) %>%
  summarise(Last_Detection = max(COLLECTION_DATE), .groups = "drop") %>%
  pivot_wider(names_from = SOURCE_CLEAN, values_from = Last_Detection, names_prefix = "Last_")

spark_data <- df_counts %>%
  group_by(SEROTYPE, COUNTRY_NAME, YEAR) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = YEAR, values_from = total, values_fill = 0) %>%
  rowwise() %>%
  mutate(Sparkline = list(c_across(`2016`:`2025`))) %>%
  ungroup() %>%
  dplyr::select(SEROTYPE, COUNTRY_NAME, Sparkline)

final_df <- df_wide %>%
  left_join(last_date, by = c("SEROTYPE", "COUNTRY_NAME")) %>%
  left_join(spark_data, by = c("SEROTYPE", "COUNTRY_NAME"))

final_df %>%
  # may stop here
  arrange(SEROTYPE, COUNTRY_NAME) %>%
  gt(groupname_col = 'SEROTYPE', rowname_col = 'COUNTRY') |>
  cols_align(
    align = "center",
    columns = 1:23) |>
  #gt(groupname_col = "SEROTYPE") %>%
  gt_color_rows(columns = contains("AFP_"), palette = "Reds") %>%
  #gt_color_rows(columns = contains("Other_"), palette = "Reds") %>%
  gt_color_rows(columns = contains("ENV_"), palette = "Reds") %>%
  fmt_date(columns = starts_with("Last_"), date_style = "iso") %>%
  gt_plt_sparkline(Sparkline, same_limit = TRUE) %>%
  cols_label_with(
    .fn = ~ gsub("_", " ", .x)
  ) %>%
  tab_options(table.font.size = 12, data_row.padding = px(2)) %>%
  tab_header(
    title = "Polio Virus Surveillance Summary (2016–2025)"
  )


#save  the tible 
write_csv(final_df, "../data/data_sequences/analysis/outputs/export.csv")

