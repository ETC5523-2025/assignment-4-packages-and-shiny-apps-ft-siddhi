# inst/app/app.R
if (!requireNamespace("shiny", quietly = TRUE)) stop("Please install 'shiny'.")
if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install 'ggplot2'.")
if (!requireNamespace("dplyr", quietly = TRUE)) stop("Please install 'dplyr'.")

ui <- shiny::fluidPage(
  shiny::titlePanel("Quarantine Breach Time Series"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::selectInput(
        "measure", "Select Measure",
        choices = c(
          "Integrated Force of Infection" = "integrated_FoI",
          "Maximum Force of Infection"    = "FoI_max"
        ),
        selected = "integrated_FoI"
      ),
      shiny::checkboxInput("show_all", "Show all scenarios", value = TRUE),
      shiny::uiOutput("scenario_ui")
    ),
    shiny::mainPanel(
      shiny::uiOutput("note"),
      shiny::plotOutput("plot", height = "520px")
    )
  )
)

server <- function(input, output, session) {
  bd <- quarrisk::breach_data

# Which scenarios have data for the measure?
  scenarios_with_data <- function(measure) {
    bd |>
      dplyr::filter(!is.na(.data[[measure]])) |>
      dplyr::distinct(scenario) |>
      dplyr::pull(scenario) |>
      sort()
  }

# Scenario dropdown appears only when "Show all" is OFF
  output$scenario_ui <- shiny::renderUI({
    if (isTRUE(input$show_all)) return(NULL)
    valid_scens <- scenarios_with_data(input$measure)
    shiny::selectInput(
      "scenario", "Select Scenario",
      choices = valid_scens,
      selected = if (length(valid_scens)) valid_scens[[1]] else character(0)
    )
  })

  output$note <- shiny::renderUI({
    valid_scens <- scenarios_with_data(input$measure)
    if (!length(valid_scens)) {
      shiny::tags$div(
        style = "margin:10px 0;color:#b33;",
        "No scenarios have non-missing values for this measure. ",
        "Try switching the Measure or check your dataset columns."
      )
    } else {
      shiny::HTML("")
    }
  })

  output$plot <- shiny::renderPlot({
    req(input$measure)
    valid_scens <- scenarios_with_data(input$measure)
    shiny::validate(
      shiny::need(length(valid_scens) > 0, "No scenarios available for this measure.")
    )

    x <- "days_infectious_community"
    y <- input$measure

# Choose data: either all scenarios with data, or a single selected scenario
    df <- if (isTRUE(input$show_all)) {
      bd |>
        dplyr::filter(scenario %in% valid_scens)
    } else {
      req(input$scenario)
      bd |>
        dplyr::filter(scenario == input$scenario)
    }

# Aggregate to whole days for smoother curves
    df_plot <- df |>
      dplyr::filter(!is.na(.data[[x]]), !is.na(.data[[y]])) |>
      dplyr::mutate(day = floor(.data[[x]])) |>
      dplyr::group_by(scenario, day) |>
      dplyr::summarise(value = mean(.data[[y]], na.rm = TRUE), .groups = "drop")

    shiny::validate(
      shiny::need(nrow(df_plot) > 0, "No non-missing values to plot.")
    )

    p <- ggplot2::ggplot(df_plot, ggplot2::aes(day, value)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::labs(
        title = if (isTRUE(input$show_all)) {
          paste0("All scenarios — ", y)
        } else {
          paste0(unique(df_plot$scenario), " — ", y)
        },
        x = "Days infectious in community (binned)",
        y = y
      ) +
      ggplot2::theme_minimal(base_size = 14)

    if (isTRUE(input$show_all)) {
      p + ggplot2::facet_wrap(~ scenario, scales = "free_y", ncol = 2)
    } else {
      p
    }
  })
}

shiny::shinyApp(ui, server)
