# data-raw/process_data.R
library(tidyverse)
library(dplyr)
library(lubridate)
library(readr)
library(tidyr)

# --- 1. Process Spawning Data ---
# Read the raw CSV from the data-raw folder
spawn_raw <- read_csv("data-raw/pott_spawn_data.csv", show_col_types = FALSE)

# Clean the data (Logic adapted from your original Shiny app)
spawn_data <- spawn_raw %>%
  select(Date, Viable, Unviable, Total, `Viability %...5`) %>%
  mutate(
    Date = mdy(Date),
    # Remove commas and convert to numeric
    Viable = as.numeric(gsub(",", "", Viable)),
    Unviable = as.numeric(gsub(",", "", Unviable)),
    Total = as.numeric(gsub(",", "", Total))
  ) %>%
  rename(Viability_Percent = `Viability %...5`) %>%
  filter(!is.na(Total) & Total > 0)

# --- 2. Process Growth Data ---
growth_raw <- read_csv("data-raw/pott_growth_data.csv", show_col_types = FALSE)

growth_data <- growth_raw %>%
  na.omit() %>%
  mutate(
    # Standardize column names for easier coding later
    protocol = as.character(Protocol),
    flexion = as.factor(`Flexion 0/1/2`),
    mean_length_mm = length
  ) %>%
  select(id, dph, mean_length_mm, depth, flexion, protocol) %>%
  filter(!is.na(id))

# --- 3. Save to Package ---
usethis::use_data(spawn_data, growth_data, overwrite = TRUE)

