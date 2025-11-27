# generate_outputs.R
library(Potterful)
library(dplyr)
library(ggplot2)
library(survival)
library(survminer)
library(knitr)

# Load the data from your package
data("potteri_larvae", package = "Potterful")

# Ensure the output directory exists
if(!dir.exists("outputs")) {
  dir.create("outputs")
}

# --- OUTPUT 1: Survival Plot ---

# Create a "death event" dataframe for survival analysis
# (Assuming survival_count decreases over time)
survival_data <- potteri_larvae %>%
  group_by(tank_id) %>%
  arrange(dph) %>%
  mutate(
    # If count drops, it's a death event (status=1), else censored/alive (status=0)
    status = ifelse(survival_count < lag(survival_count, default = first(survival_count)), 1, 0)
  ) %>%
  ungroup()

# Generate the plot using your package function
surv_plot <- plot_survival_km(
  data = survival_data,
  time_col = "dph",
  status_col = "status",
  group_col = "protocol"
)

# Save the plot
png("outputs/survival_plot.png", width = 800, height = 600)
print(surv_plot) # print() is needed for survminer objects
dev.off()

message("Output 1: Survival plot saved to outputs/survival_plot.png")



# --- OUTPUT 2: Growth Summary Table ---

# Summarize data by Tank to get Initial and Final values
growth_summary <- potteri_larvae %>%
  group_by(tank_id, protocol) %>%
  summarise(
    initial_len = first(mean_length_mm),
    final_len = last(mean_length_mm),
    days = max(dph),
    avg_temp = mean(temp_c),
    final_survival = last(survival_count),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    # Use your package functions!
    SGR = calc_sgr(final_len, initial_len, days),
    TGC = calc_tgc(final_len, initial_len, rep(avg_temp, days), days)
  ) %>%
  select(tank_id, protocol, SGR, TGC, final_survival)

# Print the table to console (copy-paste this into your README or report)
print(kable(growth_summary, digits = 2, caption = "Growth Metrics by Tank"))

# Save as a CSV for your records
write.csv(growth_summary, "outputs/growth_summary.csv", row.names = FALSE)

message("Output 2: Growth table saved to outputs/growth_summary.csv")



# --- OUTPUT 3: Growth Animation ---
# Note: This requires the 'gganimate' and 'gifski' packages.
# install.packages("gganimate")
# install.packages("gifski")

if(require(gganimate) && require(gifski)) {

  anim <- ggplot(potteri_larvae, aes(x = mean_length_mm, y = survival_count, color = protocol)) +
    geom_point(size = 3, alpha = 0.7) +
    theme_potter() + # Use your custom theme
    labs(title = 'Day Post Hatch: {frame_time}',
         x = 'Mean Length (mm)',
         y = 'Survivors') +
    transition_time(dph) +
    ease_aes('linear')

  # Save the animation
  anim_save("outputs/growth_animation.gif", anim)

  message("Output 3: Animation saved to outputs/growth_animation.gif")
} else {
  message("Skipping animation (install 'gganimate' and 'gifski' to run this part).")
}



