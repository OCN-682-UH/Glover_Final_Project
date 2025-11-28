#' Import and Clean Hatchery Data
#'
#' This function takes a list of file paths (from a directory or Shiny file upload),
#' reads them, cleans column names, and formats messy numeric data.
#'
#' @param files Character vector of file paths to CSV files.
#' @importFrom readr read_csv parse_number
#' @importFrom janitor clean_names
#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate filter select
#' @importFrom lubridate mdy
#' @export
clean_hatchery_data <- function(files) {

  #Define a helper function for a single file
  process_single_file <- function(file_path) {
    d <- readr::read_csv(file_path, show_col_types = FALSE) %>%
      janitor::clean_names() %>%
      #standardize column types based on common data issues
      dplyr::mutate(
        #Handle dates that might be characters
        date = lubridate::mdy(date),
        #Handle numbers with commas or non-numeric chars
        temp_c = readr::parse_number(as.character(temp_c)),
        survival_count = readr::parse_number(as.character(survival_count)),
        #ensure tank_id is a factor for grouping
        tank_id = as.factor(tank_id)
      )

    return(d)
  }

  #Apply the helper to all files and bind them
  full_data <- purrr::map_dfr(files, process_single_file)

  return(full_data)
}
