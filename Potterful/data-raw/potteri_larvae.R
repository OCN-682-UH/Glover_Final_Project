## code to prepare `potteri_larvae` dataset goes here
# data-raw/process_larval_data.R
library(dplyr)
library(lubridate)

raw_data <- read.csv("data-raw/potteri_rearing_log_2024.csv")

potteri_larvae <- raw_data %>%
  mutate(
    date = as.Date(date, format = "%m/%d/%Y"),
    hatch_date = as.Date(hatch_date, format = "%m/%d/%Y"),
    dph = as.numeric(date - hatch_date),
    # Standardize Protocol names
    protocol = case_when(
      protocol == "A" ~ "Protocol A (Rotifer)",
      protocol == "C" ~ "Protocol C (Copepod/Algae)",
      TRUE ~ protocol
    )
  ) %>%
  select(tank_id, dph, protocol, survival_count, mean_length_mm, temp_c)

# Save to data/ directory
usethis::use_data(potteri_larvae, overwrite = TRUE)
