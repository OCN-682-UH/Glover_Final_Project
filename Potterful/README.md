# Kent Glover's Final Project
An R package for aquaculture data mangement and vizualization for reef species (*Centropyge potteri*).

## **[Shiny App!](https://thefishlg.shinyapps.io/Potterful)** 🐟

# Potterful: Aquaculture Data Management Tools

<img src="https://raw.githubusercontent.com/OCN-682-UH/Glover_Final_Project/main/Potterful/inst/shiny/Potterful/www/potters_angel.jpg" align="center" height="400" alt="Potters Angelfish" />

## Overview

**Potterful** is an R package designed to streamline the workflow for rearing reef fish larvae, specifically focusing on Potter's Angelfish (*Centropyge potteri*). 

Aquaculture generates massive amounts of "messy" data—daily feedings, environmental logs, mortality counts, and growth measurements. **Potterful** solves this problem by providing a standardized suite of tools for:
1.  **Ingestion**: Cleaning and merging messy Excel datasheets.
2.  **Analysis**: Calculating key aquaculture metrics like Specific Growth Rate (SGR) and Thermal Growth Coefficient (TGC).
3.  **Visualization**: Generating publication-quality survival curves and growth plots with custom themes.
4.  **Interaction**: Exploring data dynamically via a bundled Shiny dashboard.

## Installation

You can install the development version of Potterful from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("OCN-682-UH/Glover_Final_Project")
````

## Usage

### 1\. Calculate Growth Metrics

Calculate standard aquaculture metrics like Specific Growth Rate (SGR) using built-in functions.

```r
library(Potterful)

# Calculate SGR for a single tank
sgr <- calc_sgr(
  final_size = 3.884,   # mm
  initial_size = 1.489, # mm
  days = 18
)

print(paste0("Specific Growth Rate: ", round(sgr, 2), "% per day"))
#> "Specific Growth Rate: 5.32% per day"
```

### 2\. Visualize Growth

Use the custom `theme_potter()` to maintain consistent aesthetics across your research outputs.

```r
library(ggplot2)
library(dplyr)

# Load package dataset
data("growth_data")

growth_data %>%
  filter(!is.na(length)) %>%
  ggplot(aes(x = dph, y = length, color = Protocol)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE, method = "loess") +
  labs(title = "Larval Growth by Protocol",
       x = "Days Post Hatch (DPH)",
       y = "Length (mm)") +
  theme_potter()
```

### 3\. Survival Analysis

Easily generate Kaplan-Meier survival curves to compare rearing protocols.

```r
# Plot survival using the package's plotting function
plot_survival_km(
  data = potteri_larvae,
  time_col = "dph",
  status_col = "status",
  group_col = "protocol"
)
```

## Interactive Dashboard

**Potterful** includes a Shiny application for interactive data exploration. You can run it locally from within the package:

```r
run_potterful()
```

Or visit the hosted version here: [TheFishLG.shinyapps.io/Potterful](https://thefishlg.shinyapps.io/Potterful/)

## Data Dictionary

This package includes several datasets sourced from Oceanic Institute 2023-2024 Research Tanks.

| Dataset | Description |
|:---|:---|
| `growth_data` | Longitudinal length measurements (mm) of larvae over days post hatch (dph). |
| `potteri_larvae` | Survival data including status (0=alive, 1=dead) and protocol groups. |
| `spawn_data` | Daily records of spawn viability, total egg counts, and date. |


| Column Name | Description |
|:---|:---|
| `id` | Photo/Larvae ID from sampling. |
| `dph` | Days Post Hatch. |
| `length` | Notochord Length or Body Length, depending on larvae size. |
| `depth` | Larvae Depth, measured from anus to top of myomeres. |
| `Flexion` | 0/1/2 indicaiting not begun (0), incomplete (1), or complete (2). |
| `Protocol` | Rearing Protocol the larvae was raised on. |
| `Date` | Date of egg collection, usually morning after evening spawns. |
| `Viable` | Number of viable eggs. |
| `Unviable` | Number of unviable eggs. |
| `Total` | Total number of unviable and viable eggs. |
| `Viability %` | Percent viability (viable/total x 100) |




## Authors

  * **Kent Glover** - *Initial work* - [GitHub Profile](https://github.com/thefishlg)

