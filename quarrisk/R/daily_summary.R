#' Summarise daily mean FoI by scenario
#' @param x data frame with columns: integrated_FoI, days_infectious_community, scenario
#' @return tibble with scenario, day, mean_integrated_FoI
#' @export
daily_summary <- function(x) {
  needed <- c("integrated_FoI","days_infectious_community","scenario")
  stopifnot(all(needed %in% names(x)))
  dplyr::filter(x, !is.na(.data$integrated_FoI)) |>
    dplyr::mutate(day = floor(.data$days_infectious_community)) |>
    dplyr::summarise(mean_integrated_FoI = mean(.data$integrated_FoI, na.rm = TRUE),
                     .by = c(scenario, day))
}
