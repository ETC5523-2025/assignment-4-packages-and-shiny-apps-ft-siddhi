#' Quarantine breach probability time series (Delta scenarios)
#'
#' Combined time-series from four scenarios:
#' - Traveller, unvaccinated, home quarantine 7d
#' - Worker, unvaccinated, hotel quarantine 14d
#' - Traveller, AZ1 vaccinated, hotel quarantine 14d
#' - Traveller, AZ2 vaccinated, hotel quarantine 14d
#'
#' Column names follow the original CSVs (e.g., time/day index and probability/hazard);
#' a `scenario` column labels each series.
#'
#' @format A tibble/data.frame with >= 2 numeric columns plus `scenario`.
#' @source Mike Lydeamore, COVIDQuarantine (quarantinemodel branch), data/penetration/.
"breach_data"
