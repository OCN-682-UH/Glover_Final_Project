# data-raw/process_data.R
library(dplyr)

# --- Simulate Data (Replace this block with read.csv("data-raw/your_file.csv") ---
# This ensures you have data even if your CSV isn't ready yet
set.seed(123)
potteri_larvae <- data.frame(
  tank_id = rep(c("T1", "T2", "T3"), each = 60),
  dph = rep(1:60, 3),
  protocol = rep(c("Protocol A", "Protocol C", "Protocol A"), each = 60),
  temp_c = rnorm(180, mean = 26.5, sd = 0.5),
  mean_length_mm = c(seq(1.5, 4.5, length.out=60), seq(1.5, 3.8, length.out=60), seq(1.5, 4.6, length.out=60)),
  survival_count = c(seq(1000, 50, length.out=60), seq(1000, 10, length.out=60), seq(1000, 60, length.out=60))
)
# --------------------------------------------------------------------------------

# Save the data to the package
usethis::use_data(potteri_larvae, overwrite = TRUE)
