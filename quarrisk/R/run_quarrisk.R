#' Launch the quarrisk Shiny app
#'
#' Opens the interactive dashboard showing breach probability time-series
#' for different Delta quarantine scenarios.
#'
#' @export
run_quarrisk <- function() {
  app_dir <- system.file("app", package = "quarrisk")
  shiny::runApp(app_dir, display.mode = "normal")
}

