#' Read and Tidy Hatchery Datasheet
#'
#' @param file_path Path to the file
#' @return A tidy dataframe
#' @importFrom readr read_csv
#' @importFrom dplyr select mutate filter
#' @importFrom tidyr pivot_longer
#' @export
read_hatchery_data <- function(file_path) {

  # 1. Dynamic Skip: Find where the data starts
  lines <- readLines(file_path, n = 30)
  skip_n <- which(grepl("Culture Age", lines)) - 1

  if (length(skip_n) == 0) {
    warning(paste("Could not find 'Culture Age' header in", basename(file_path)))
    return(NULL)
  }

  # 2. Read the data
  raw_data <- readr::read_csv(file_path, col_names = FALSE, skip = skip_n, show_col_types = FALSE)

  # 3. Tidy and Validate (The tryCatch goes here!)
  tryCatch({

    # VALIDATION: Check if column 1 looks correct
    col_1_name <- as.character(raw_data[1, 1])
    # Check if it contains "Age" OR if the second row is a number (common in these sheets)
    if (!grepl("Age", col_1_name, ignore.case = TRUE) && !is.numeric(raw_data[2,1])) {
      stop("File structure invalid: Column 1 is not 'Age'.")
    }

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
      # Ensure age is numeric (removes header rows that got read in)
      dplyr::mutate(age = suppressWarnings(as.numeric(age))) %>%
      dplyr::filter(!is.na(age)) %>%
      # Pivot the feeding columns (Rest of pipeline)
      tidyr::pivot_longer(
        cols = c(Algae, Live_Feed, Rotifers, Dry_Feed, Frozen_Feed),
        names_to = "feed_category",
        values_to = "feed_type"
      ) %>%
      dplyr::filter(!is.na(feed_type), feed_type != "0") %>%
      dplyr::mutate(feed_type = trimws(feed_type))

    return(tidy_data)

  }, error = function(e) {
    # If anything above fails (missing columns, wrong format), this runs:
    warning(paste("Skipping", basename(file_path), ":", e$message))
    return(NULL)
  })
}
