#' Calculate Specific Growth Rate (SGR)
#'
#' @param final_size Numeric. Final weight or length.
#' @param initial_size Numeric. Initial weight or length.
#' @param days Numeric. Duration of growth period.
#' @return Numeric SGR value.
#' @export
calc_sgr <- function(final_size, initial_size, days) {
  if(any(days <= 0)) stop("Days must be > 0")
  sgr <- (log(final_size) - log(initial_size)) / days * 100
  return(sgr)
}

#' Calculate Thermal-Unit Growth Coefficient (TGC)
#'
#' @param final_size Numeric. Final size.
#' @param initial_size Numeric. Initial size.
#' @param temp Numeric vector of daily temperatures.
#' @param days Numeric. Number of days.
#' @return Numeric TGC value.
#' @export
calc_tgc <- function(final_size, initial_size, temp, days) {
  avg_temp <- mean(temp, na.rm = TRUE)
  tgc <- ((final_size^(1/3) - initial_size^(1/3)) / (avg_temp * days)) * 1000
  return(tgc)
}

#' Calculate Feed Conversion Ratio (FCR)
#'
#' @param feed_input Numeric. Total dry weight of feed.
#' @param biomass_gain Numeric. Total biomass gain.
#' @return Numeric FCR value.
#' @export
calc_fcr <- function(feed_input, biomass_gain) {
  if (biomass_gain <= 0) return(NA)
  return(feed_input / biomass_gain)
}
