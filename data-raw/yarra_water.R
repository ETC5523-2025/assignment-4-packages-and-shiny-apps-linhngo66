library(tidyverse)
library(janitor)
library(readxl)

# Read data
yarra_wq <- read_excel("data-raw/yarra_wq.xls", col_types = "text") |> clean_names()

# Create a helper function to check if a value is a number
is_numeric_string <- function(x) {
  !is.na(suppressWarnings(as.numeric(x)))
}

# Convert data types
yarra_wq <- yarra_wq |>
  mutate(
    datetime = case_when(
      is_numeric_string(datetime) ~ as.POSIXct(as.numeric(datetime) * 86400, origin = "1899-12-30", tz = "UTC"),
      TRUE ~ parse_date_time(datetime, orders = c("mdy", "dmy", "ymd", "Y-m-d H:M:S"))
    ),
    year = year(datetime),
    month = month(datetime),
    weekday = wday(datetime, label = TRUE),
    value = as.numeric(value),
    across(c("site_id", "name", "data_type", "parameter_id", "parameter"), as.factor),
  )

yarra_water <- yarra_wq |> 
  filter(year >= 1990) |> 
  mutate(
    period = ifelse(year <= 2000, "1990s", "2020s") |> factor(levels = c("1990s", "2020s")),
    season = case_when(
      month %in% c(9, 10, 11) ~ "Spring",
      month %in% c(12, 1, 2) ~ "Summer",
      month %in% c(3, 4, 5) ~ "Autumn",
      month %in% c(6, 7, 8) ~ "Winter"
    ) |> factor(levels = c("Spring", "Summer", "Autumn", "Winter")),
    parameter_type = case_when(
      parameter_id %in% c("2335", "2331", "2332", "2333", "2336", "2337", "2374", "2364", "2363") ~ "Nutrient",
      parameter_id %in% c("141.5", "2054", "215", "210", "820", "810", "450") ~ "Physical/Chemical Stressor",
      TRUE ~ "Toxicant"
    ) |> factor(levels = c("Physical/Chemical Stressor", "Nutrient", "Toxicant"))
  ) |> 
  arrange(desc(parameter_type), parameter) |> 
  mutate(parameter = factor(parameter, levels = unique(parameter))) |> 
  filter(value != 0)  |> 
  select(-c("resolution", "parameter_id", "quality"))

usethis::use_data(yarra_water, overwrite = TRUE)
