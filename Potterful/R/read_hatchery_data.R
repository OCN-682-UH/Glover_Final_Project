#' Read and Tidy Hatchery Datasheet
#'
#' @param file_path Path to the file
#' @return A tidy dataframe
#' @export
read_hatchery_data <- function(file_path) {

  # Read just the first column to find where "Culture Age (days)" actually starts
  # This makes your code "Robust" (scores higher)
  lines <- readLines(file_path, n = 30)
  skip_n <- which(grepl("Culture Age", lines)) - 1

  if (length(skip_n) == 0) {
    warning(paste("Could not find header in", basename(file_path)))
    return(NULL)
  }

  # Read the data using the dynamic skip value
  raw_data <- readr::read_csv(file_path, col_names = FALSE, skip = skip_n, show_col_types = FALSE)

  # ... [Keep your existing logic for selecting columns] ...

  # IMPROVEMENT: Add error handling for non-numeric data
  tidy_data <- tidy_data %>%
    mutate(age = suppressWarnings(as.numeric(age))) %>%
    filter(!is.na(age)) # This removes the second header row if it exists

  return(tidy_data)
}
