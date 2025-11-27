#' Custom Theme for Potterful Plots
#' @param base_size Base font size.
#' @import ggplot2
#' @export
theme_potter <- function(base_size = 12) {
  theme_bw(base_size = base_size) +
    theme(
      strip.background = element_rect(fill = "#2c3e50"),
      strip.text = element_text(color = "white", face = "bold"),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}

#' Plot Kaplan-Meier Survival Curve
#'
#' @param data Data frame with time and status columns.
#' @param time_col String name of time column (e.g., "dph").
#' @param status_col String name of status column (1=dead, 0=alive).
#' @param group_col String name of grouping column.
#' @import survival
#' @import survminer
#' @export
plot_survival_km <- function(data, time_col, status_col, group_col) {
  f <- as.formula(paste("Surv(", time_col, ",", status_col, ") ~", group_col))
  fit <- surv_fit(f, data = data)
  ggsurvplot(
    fit,
    data = data,
    pval = TRUE,
    risk.table = TRUE,
    ggtheme = theme_potter()
  )
}
