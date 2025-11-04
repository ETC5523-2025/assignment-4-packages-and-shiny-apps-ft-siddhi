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
      ),

      shiny::tags$hr(),
      shiny::h4("Field Descriptions"),
      shiny::tags$p(
        shiny::strong("Days infectious in community:"),
        "Number of days an infected traveller or worker was infectious in the community after quarantine."
      ),
      shiny::tags$p(
        shiny::strong("Integrated FoI:"),
        "Total infectiousness accumulated over the infectious period (area under the FoI curve)."
      ),
      shiny::tags$p(
        shiny::strong("Max FoI:"),
        "Maximum instantaneous infectiousness reached during the infectious period."
      ),
      shiny::tags$p(
        shiny::strong("Scenario:"),
        "Simulation category describing quarantine setting (e.g. hotel vs home, vaccinated vs unvaccinated)."
      )
    ),

    shiny::mainPanel(
      shiny::plotOutput("ts"),
      shiny::tags$hr(),
      shiny::p(shiny::em(shiny::textOutput("caption"))),
      shiny::tableOutput("stats"),
      shiny::tags$hr(),
      shiny::h4("How to interpret the outputs"),
      shiny::uiOutput("guide")
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


 # Interpretation guide

  output$guide <- shiny::renderUI({
    lbl <- if (metric() == "integrated_FoI") {
      "Average integrated FoI = total infectiousness over time (area under the curve)."
    } else {
      "Average maximum FoI = peak instantaneous infectiousness (height of the highest spike)."
    }

    bullets <- if (isTRUE(input$facet_all)) {
      c(
        "Each panel is a different scenario; compare heights and shapes across panels.",
        "Higher lines mean higher average risk on that day.",
        "The highest point marks the day of greatest average risk.",
        "Flat or near-zero segments indicate little/no community infectiousness."
      )
    } else {
      c(
        paste0("Scenario: ", input$scenario, "."),
        "Look for the highest point → day of greatest average risk.",
        "Earlier, narrow peaks = short, sharp risk window; wider curves = prolonged risk window.",
        "Switch “What to plot?” to compare total risk (Integrated FoI) vs peak risk (Max FoI)."
      )
    }

    html <- paste0(
      "<p><b>", lbl, "</b></p>",
      "<ul><li>", paste(bullets, collapse = "</li><li>"), "</li></ul>"
    )
    HTML(html)
  })
}

# APP
shiny::shinyApp(ui, server)
