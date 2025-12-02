#' Larval Rearing Data for Centropyge potteri
#'
#' A dataset containing growth and survival metrics for Potter's Angelfish.
#'
#' @format A data frame with rows and variables:
#' \describe{
#'   \item{tank_id}{Unique identifier for the rearing vessel}
#'   \item{dph}{Days Post Hatch}
#'   \item{protocol}{Experimental feeding regime}
#'   \item{mean_length_mm}{Mean total length in mm}
#'   \item{survival_count}{Number of surviving larvae}
#'   \item{temp_c}{Average daily water temperature in Celsius}
#' }
"potteri_larvae"

#' Potter's Angelfish Spawning Data
#'
#' Historical spawning data for Centropyge potteri, recording daily egg production and viability.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{Date}{Date of collection}
#'   \item{Viable}{Count of viable eggs}
#'   \item{Unviable}{Count of unviable eggs}
#'   \item{Total}{Total eggs collected}
#'   \item{Viability_Percent}{Percentage of eggs that were viable}
#' }
"spawn_data"

#' Potter's Angelfish Growth Data
#'
#' Larval growth measurements including length and flexion stage over time.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{id}{Unique specimen identifier}
#'   \item{dph}{Days Post Hatch}
#'   \item{mean_length_mm}{Total length of the larvae in mm}
#'   \item{depth}{Body depth in mm}
#'   \item{flexion}{Flexion stage (0, 1, or 2)}
#'   \item{protocol}{Experimental rearing protocol used}
#' }
#' @export
"growth_data"

#' Master Hatchery Dataset
#'
#' A combined dataset of all tank runs from the 2024 season.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{age}{Age of the culture in days (DPH)}
#'   \item{date}{Calendar date of the observation}
#'   \item{feed_category}{Type of feed (Algae, Rotifers, etc.)}
#'   \item{feed_type}{Specific strain or product name (e.g., "Tiso", "Parvo")}
#'   \item{source_file}{Original filename, used to identify the specific Tank ID}
#' }
"hatchery_master"
