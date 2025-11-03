# UI ----
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
          choices = sort(unique(quarrisk::breach_data$scenario))
        )
      )
    ),
    shiny::mainPanel(
      shiny::plotOutput("ts")
    )
  )
)

# SERVER ----
server <- function(input, output, session) {

  # use packaged data with explicit namespace
  bd <- quarrisk::breach_data

  daily_summary <- reactive({
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

  output$ts <- shiny::renderPlot({
    df <- daily_summary()
    metric <- input$view

    if (!isTRUE(input$facet_all)) {
      df <- dplyr::filter(df, .data$scenario == input$scenario)
    }

    ggplot2::ggplot(df, ggplot2::aes(x = .data$day,
                                     y = .data[[paste0("mean_", metric)]],
                                     colour = .data$scenario)) +
      ggplot2::geom_line(linewidth = 1) +
      (if (isTRUE(input$facet_all)) ggplot2::facet_wrap(~scenario) else NULL) +
      ggplot2::labs(
        x = "Days infectious in community",
        y = if (metric == "integrated_FoI") "Average integrated FoI"
        else "Average maximum FoI",
        colour = "Scenario",
        title  = "Quarantine breach risk (simulation summaries)"
      ) +
      ggplot2::theme_minimal()
  })
}

# APP ----
shiny::shinyApp(ui, server)
