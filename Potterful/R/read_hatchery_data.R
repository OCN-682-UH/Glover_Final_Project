#' Read and Tidy Hatchery Datasheet
#'
#' @param file_path Path to the file (.csv or .xlsx)
#' @return A tidy dataframe ready for plotting
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom tools file_ext
#' @importFrom dplyr select mutate filter
#' @importFrom tidyr pivot_longer
#' @importFrom lubridate mdy ymd
#' @export
read_hatchery_data <- function(file_path) {

  # 1. Detect file extension
  ext <- tools::file_ext(file_path)

  # 2. Read based on file type
  # We use col_names = FALSE to get a raw matrix-like read
  if (tolower(ext) %in% c("xlsx", "xls")) {
    # Check if readxl is installed
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is needed for this function to work on Excel files. Please install it.")
    }
    # Note: 'skip' in read_excel handles hidden metadata rows nicely
    raw_data <- readxl::read_excel(file_path, col_names = FALSE, skip = 12)
  } else if (tolower(ext) == "csv") {
    raw_data <- readr::read_csv(file_path, col_names = FALSE, skip = 12, show_col_types = FALSE)
  } else {
    stop("Unsupported file type. Please provide a .csv or .xlsx file.")
  }

  # 3. Select by INDEX (Position), not name.
  # This is crucial because read_csv names columns "X1" but read_excel names them "...1"
  # Indices based on your specific template structure:
  tidy_data <- raw_data %>%
    dplyr::select(
      age = 1,           # Column A
      date = 2,          # Column B
      Algae = 13,        # Column M
      Live_Feed = 17,    # Column Q
      Rotifers = 25,     # Column Y
      Dry_Feed = 39,     # Column AM
      Frozen_Feed = 42   # Column AP
    ) %>%
    # Filter out empty rows (where Age is NA)
    dplyr::filter(!is.na(age)) %>%

    # 4. Clean up types
    dplyr::mutate(
      # Excel dates often come in as weird numbers or POSIXct, CSVs as text
      # This logic handles both:
      date = if (inherits(date, "POSIXt")) as.Date(date) else lubridate::ymd(date),
      age = as.numeric(age)
    ) %>%

    # 5. Pivot to "Long" format for plotting
    tidyr::pivot_longer(
      cols = c(Algae, Live_Feed, Rotifers, Dry_Feed, Frozen_Feed),
      names_to = "feed_category",
      values_to = "feed_type"
    ) %>%

    # 6. Clean up values
    dplyr::filter(!is.na(feed_type), feed_type != "0") %>%
    dplyr::mutate(feed_type = trimws(feed_type))

  return(tidy_data)
}
