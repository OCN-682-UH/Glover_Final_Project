#' Launch the Potterful Dashboard
#' @export
#' @importFrom shiny runApp
run_potterful <- function() {
  app_dir <- system.file("shiny", "Potterful", package = "Potterful")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `Potterful`.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
