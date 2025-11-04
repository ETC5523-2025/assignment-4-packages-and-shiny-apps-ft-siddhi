# Load & clean data
dataset <- quarrisk::breach_data
cat(">>> OPENED app.R from: ", normalizePath("inst/app/app.R"), "\n")

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


# Theme
theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "cosmo",
  primary = "#0d6efd",
  secondary = "#6c757d",
  base_font = bslib::font_google("Source Sans Pro"),
  heading_font = bslib::font_google("Poppins")
)

# UI

ui <- shiny::fluidPage(
  theme = theme,

  # Custom CSS (keeps markers happy without extra deps)
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      body {
        background: linear-gradient(180deg, #f0f2f7 0%, #e6ecf3 100%);
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial;
      }

      .container-fluid { max-width: 1200px; }

      h1 {
        letter-spacing: .3px;
        font-weight: 700;
        color: #2b2d42;
        text-shadow: 0 1px 2px rgba(0,0,0,0.1);
      }

      h4 { margin-top: 14px; font-weight:600; color: #14213d; }
      hr { border-top: 1px solid #dee2e6; }

      /* Sidebar card */
      .well {
        background: #ffffff;
        border: 0;
        border-radius: 16px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.08);
        padding: 18px 18px 8px 18px;
      }

      /* Main cards */
      .card {
        background: #ffffff;
        border-radius: 16px;
        box-shadow: 0 6px 20px rgba(0,0,0,0.06);
        padding: 20px;
        margin-bottom: 18px;
      }

      /* Inputs */
      .form-control, .selectize-input, .form-select {
        border-radius: 10px;
        border-color: #ced4da;
        transition: box-shadow 0.2s ease;
      }
      .form-control:focus, .selectize-input.focus {
        box-shadow: 0 0 0 0.25rem rgba(67,97,238,0.25);
        border-color: #4361EE;
      }
      .btn {
        background-color: #4361EE;
        color: white;
        border: none;
        border-radius: 10px;
      }

      /* Table */
      table { width: 100%; border-collapse: collapse; }
      th {
        background: #f7f8fc;
        color: #222;
        font-weight: 600;
        text-align: left;
        padding: 8px;
      }
      td { padding: 8px; }
      tbody tr:nth-child(odd) { background:#fafbff; }
    "))
  ),

  shiny::titlePanel("Quarantine Breach Time Series"),

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 4,

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
        "Total infectiousness over the infectious period (area under the FoI curve)."
      ),
      shiny::tags$p(
        shiny::strong("Max FoI:"),
        "Peak instantaneous infectiousness reached during the infectious period."
      ),
      shiny::tags$p(
        shiny::strong("Scenario:"),
        "Quarantine/vaccination setting (e.g., hotel vs home, vaccinated vs unvaccinated)."
      )
    ),
    shiny::mainPanel(
      width = 8,
      shiny::div(class = "card",
                 shiny::plotOutput("ts")
      ),
      shiny::div(class = "card",
                 shiny::p(shiny::em(shiny::textOutput("caption")), style = "margin:0 0 8px 0;"),
                 shiny::tableOutput("stats")
      ),
      shiny::div(class = "card",
                 shiny::h4("How to interpret the outputs"),
                 shiny::uiOutput("guide")
      )
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

  # ---- Interpretation guide ----
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
