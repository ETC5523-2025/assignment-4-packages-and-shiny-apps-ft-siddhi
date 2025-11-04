# Load & clean data
dataset <- quarrisk::breach_data

# Check NA counts
na_summary <- sapply(dataset, function(x) sum(is.na(x)))
print(na_summary)

# Keep only rows needed for plotting and that have complete data
dataset <- dplyr::filter(
  dataset,
  !is.na(days_infectious_community),
  !is.na(integrated_FoI),
  !is.na(FoI_max),
  !is.na(scenario)
)

cat("After cleaning, dataset has", nrow(dataset), "rows remaining.\n")

# UI
ui <- shiny::fluidPage(
  shiny::titlePanel("Quarantine Breach Time Series"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::selectInput(
        "view",
        "What to plot?",
        choices = c("Integrated FoI" = "integrated_FoI",
                    "Max FoI"        = "FoI_max"),
        selected = "integrated_FoI"
      ),
      shiny::checkboxInput("facet_all", "Show all scenarios", value = TRUE),
      shiny::conditionalPanel(
        condition = "!input.facet_all",
        shiny::selectInput(
          "scenario",
          "Select Scenario",
          choices = sort(unique(dataset$scenario))
        )
      )
    ),
    shiny::mainPanel(
      shiny::plotOutput("ts"),
      shiny::tags$hr(),
      shiny::tags$p(shiny::em(shiny::textOutput("caption"))),
      shiny::tableOutput("stats")
    )
  )
)

# SERVER
server <- function(input, output, session) {

  # Use the cleaned dataset
  bd <- dataset

  # Summarise data for plotting
  daily_summary <- shiny::reactive({
    x <- bd
    x <- dplyr::filter(x, !is.na(.data$integrated_FoI))
    x <- dplyr::mutate(x, day = floor(.data$days_infectious_community))
    x <- dplyr::group_by(x, .data$scenario, .data$day)
    dplyr::summarise(
      x,
      mean_integrated_FoI = mean(.data$integrated_FoI, na.rm = TRUE),
      mean_FoI_max        = mean(.data$FoI_max,        na.rm = TRUE),
      .groups = "drop"
    )
  })

  # Reactives for metric and filtering
  metric <- shiny::reactive(input$view)

  filtered <- shiny::reactive({
    df <- daily_summary()
    if (!isTRUE(input$facet_all)) {
      df <- dplyr::filter(df, .data$scenario == input$scenario)
    }
    df
  })

  # Plot output
  output$ts <- shiny::renderPlot({
    df <- filtered()
    mcol <- paste0("mean_", metric())

    ggplot2::ggplot(
      df,
      ggplot2::aes(x = .data$day, y = .data[[mcol]], colour = .data$scenario)
    ) +
      ggplot2::geom_line(linewidth = 1) +
      (if (isTRUE(input$facet_all)) ggplot2::facet_wrap(~scenario) else NULL) +
      ggplot2::labs(
        x = "Days infectious in community",
        y = if (metric() == "integrated_FoI") "Average integrated FoI" else "Average maximum FoI",
        colour = "Scenario",
        title  = "Quarantine breach risk (simulation summaries)"
      ) +
      ggplot2::theme_minimal()
  })

  # Caption text
  output$caption <- shiny::renderText({
    label <- if (metric() == "integrated_FoI") "Average integrated FoI" else "Average maximum FoI"
    if (isTRUE(input$facet_all)) {
      sprintf("%s across all scenarios (%d rows).", label, nrow(filtered()))
    } else {
      sprintf("%s for %s (%d rows).", label, input$scenario, nrow(filtered()))
    }
  })

  # Stats table
  output$stats <- shiny::renderTable({
    df <- filtered()
    mcol <- paste0("mean_", metric())
    if (nrow(df) == 0) return(NULL)
    data.frame(
      Scenarios = dplyr::n_distinct(df$scenario),
      Days      = dplyr::n_distinct(df$day),
      Peak      = max(df[[mcol]], na.rm = TRUE),
      PeakDay   = df$day[which.max(df[[mcol]])],
      Mean      = mean(df[[mcol]], na.rm = TRUE),
      check.names = FALSE
    )
  }, digits = 3)
}

# APP
shiny::shinyApp(ui, server)
