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

  # 2. Dynamic Skip: Find the row where data actually starts
  # We read the first 30 lines just to search for the header
  header_search <- readLines(file_path, n = 30)

  # Look for the unique keyword "Culture Age"
  header_row <- grep("Culture Age", header_search)

  # Safety check: if keyword isn't found, warn and skip file
  if (length(header_row) == 0) {
    warning(paste("Skipping file: Could not find 'Culture Age' header in", basename(file_path)))
    return(NULL)
  }

  # The 'skip' argument needs the number of lines to skip BEFORE the header
  # So if header is on line 16, we skip 15.
  n_skip <- header_row[1] - 1

  # 3. Read based on file type using the dynamic skip
  if (tolower(ext) %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is needed. Please install it.")
    }
    raw_data <- readxl::read_excel(file_path, col_names = FALSE, skip = n_skip)
  } else if (tolower(ext) == "csv") {
    raw_data <- readr::read_csv(file_path, col_names = FALSE, skip = n_skip, show_col_types = FALSE)
  } else {
    stop("Unsupported file type.")
  }

  # 4. Select by INDEX (Position)
  # Now that we landed exactly on the header row, the columns should align perfectly.
  # We wrap select in tryCatch to handle files that might be formatted weirdly.
  tryCatch({
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
      dplyr::filter(!is.na(age)) %>%
      # Remove the header row itself if it got read in as data (happens if types match)
      dplyr::filter(age != "Culture Age (days)") %>%

      dplyr::mutate(
        # Handle mixed date formats (Excel numbers vs text)
        date = if (inherits(date, "POSIXt")) as.Date(date) else lubridate::ymd(date),
        age = suppressWarnings(as.numeric(age))
      ) %>%
      # Drop rows where Age didn't parse (cleans up extra headers/footers)
      dplyr::filter(!is.na(age)) %>%

      tidyr::pivot_longer(
        cols = c(Algae, Live_Feed, Rotifers, Dry_Feed, Frozen_Feed),
        names_to = "feed_category",
        values_to = "feed_type"
      ) %>%
      dplyr::filter(!is.na(feed_type), feed_type != "0") %>%
      dplyr::mutate(feed_type = trimws(feed_type))

    return(tidy_data)

  }, error = function(e) {
    warning(paste("Error processing", basename(file_path), ":", e$message))
    return(NULL)
  })
}
