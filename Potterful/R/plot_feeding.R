#' Visualize Feeding Schedule
#'
#' @param tidy_data Output from read_hatchery_data()
#' @return A ggplot object
#' @import ggplot2
#' @export
plot_feeding_schedule <- function(tidy_data) {

  library(ggplot2)

  ggplot(tidy_data, aes(x = age, y = feed_category, fill = feed_type)) +
    # Use geom_tile to show duration of feeding
    geom_tile(color = "white", height = 0.8) +

    # Labels and Theme
    labs(
      title = "Hatchery Feeding Schedule",
      x = "Culture Age (Days Post Hatch)",
      y = "Feed Category",
      fill = "Feed Type"
    ) +
    scale_x_continuous(breaks = seq(0, max(tidy_data$age, na.rm=TRUE), by = 2)) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}
