#' Simulated quarantine breach time-series
#'
#' A cleaned tibble combining four scenarios from the Science Advances
#' quarantine model repository.
#'
#' @format A tibble with columns including:
#' \describe{
#'   \item{days_infectious_community}{numeric}
#'   \item{FoI_max}{numeric}
#'   \item{integrated_FoI}{numeric}
#'   \item{scenario}{character, labelled scenario name}
#'   \item{...}{other model fields}
#' }
#' @source GitHub: MikeLydeamore/COVIDQuarantine (quarantinemodel/data/penetration)
"breach_data"

