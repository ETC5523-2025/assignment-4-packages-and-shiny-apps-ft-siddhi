## code to prepare `breach_data` dataset goes here

usethis::use_data(breach_data, overwrite = TRUE)

daily_summary <- breach_data |>
  dplyr::filter(!is.na(.data$integrated_FoI)) |>
  dplyr::mutate(day = floor(.data$days_infectious_community)) |>
  dplyr::summarise(
    mean_integrated_FoI = mean(.data$integrated_FoI, na.rm = TRUE),
    .by = c(scenario, day)
  )

usethis::use_data(daily_summary, overwrite = TRUE)
