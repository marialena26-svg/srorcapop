#' Southern Resident killer whale population
#'
#' Gives an interactive shiny application for exploring Southern Resident
#' killer whale population trends, pod counts, births and mortality, over
#' 50 years.
#'
#'@details
#' The app uses the SRKW data set included in the package and provides
#' interactive visualizations of population trends, births and mortality across
#' years (1975-2024). Data provided by Center for Whale Research (CWR) and
#' National Oceanic and Atmospheric Administration (NOAA).
#'
#' @references Center for Whale Research. Southern Resident Killer Whale
#' population data.\url{https://www.whaleresearch.com/orca-population/}
#'
#' National Oceanic and Atmospheric Administration (NOAA).
#' \url{https://www.noaa.gov/}
#'
#'@return
#' A shiny app launched in the browser or Viewer pane.
#'
#'@examples
#' if (interactive()) {
#' srkw_shiny_app()
#' }
#'
#' @import shiny ggplot2 bslib scales
#' @importFrom DT renderDT
#'
#' @export
srkw_shiny_app <- function() {

  ##load data
  data <- load_srkw_data()

  ##UI

  ui <- bslib::page_sidebar(
    title = "Southern Resident Orca Population",

  ##inputs

  ##year range

    sidebar = bslib::sidebar(
      shiny::sliderInput(
        inputId = "year_range",
        label = "Select year range",
        min = min(data$year, na.rm = TRUE),
        max = max(data$year, na.rm = TRUE),
        value = c(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE)),
        step = 1,
        sep = "" #remove commas in years
      ),

  ##pod selection
      shiny::checkboxGroupInput(
        inputId = "pods",
        label = "Select pods to display",
        choices = c("J_pod", "K_pod", "L_pod"),
        selected = c("J_pod", "K_pod", "L_pod")
      )
    ),

  ##outputs

  ##total population
  shiny::h2("Total population"),
  shiny::textOutput("population_text"),
  shiny::plotOutput("total_population_plot"),

  ##pod population
  shiny::h2("Population per pod"),
  shiny::plotOutput("pod_population_plot"),

  ##births and deaths
  shiny::h2("Births and mortalities"),
  shiny::plotOutput("births_deaths_plot"),

  ##interactive data table
  shiny::h2("Data Table"),
  DT::DTOutput("data_table")
  )

  ##server
  server <- function(input, output, session) {

  ##year filtered data
  filtered_data <- shiny::reactive({
      data[data$year >= input$year_range[1] &
             data$year <= input$year_range[2], ]
    })

    ##total population text
    output$population_text <- shiny::renderText({
      latest_year <- max(filtered_data()$year, na.rm = TRUE)
      latest_population <- filtered_data()$total_population[
        filtered_data()$year == latest_year
      ]

      paste("Population in", latest_year, ":", latest_population)
    })

    ##total population plot
    output$total_population_plot <- shiny::renderPlot({
      ggplot2::ggplot(
        filtered_data(),
        ggplot2::aes(x = year, y = total_population)
      ) +
        ggplot2::geom_line(linewidth = 1.2, color = "#0072B2") +
        ggplot2::geom_point(size = 3, color = "#F0E442") +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::scale_x_continuous(labels = function(x) x) +
        ggplot2::labs(
          title = "Southern Resident Orca Population",
          x = "Year",
          y = "Population"
        )
    })

    ##pod population plot
    output$pod_population_plot <- shiny::renderPlot({
      plot_data <- filtered_data()

      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = year)) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::scale_x_continuous(labels = function(x) x) +
        ggplot2::labs(
          title = "Population per pod",
          subtitle = "Trends for selected pods",
          x = "Year",
          y = "Population",
          color = "Pod"
        )

      #J pod
      if ("J_pod" %in% input$pods) {
        p <- p + ggplot2::geom_line(
          ggplot2::aes(y = J_pod, color = "J pod"),
          linewidth = 1.2
        ) +
          ggplot2::geom_point(
            ggplot2::aes(y = J_pod, color = "J pod"),
            size = 2.5
          )
      }

      #K pod
      if ("K_pod" %in% input$pods) {
        p <- p + ggplot2::geom_line(
          ggplot2::aes(y = K_pod, color = "K pod"),
          linewidth = 1.2
        ) +
          ggplot2::geom_point(
            ggplot2::aes(y = K_pod, color = "K pod"),
            size = 2.5
          )
      }

      #L pod
      if ("L_pod" %in% input$pods) {
        p <- p + ggplot2::geom_line(
          ggplot2::aes(y = L_pod, color = "L pod"),
          linewidth = 1.2
        ) +
          ggplot2::geom_point(
            ggplot2::aes(y = L_pod, color = "L pod"),
            size = 2.5
          )
      }

      p +
        ggplot2::scale_color_manual(values = c(
          "J pod" = "#CC79A7",
          "K pod" = "#28E2E5",
          "L pod" = "#A8D098"
        ))

  })

    ##births and deaths plot
    output$births_deaths_plot <- shiny::renderPlot({
      ggplot2::ggplot(filtered_data(), ggplot2::aes(x = year)) +
        ggplot2::geom_line(
          ggplot2::aes(y = births, color = "Births"),
          linewidth = 1.2
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = births, color = "Births"),
          size = 2.5
        ) +

        ggplot2::geom_line(
          ggplot2::aes(y = deaths, color = "Deaths"),
          linewidth = 1.2
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = deaths, color = "Deaths"),
          size = 2.5
        ) +

        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::scale_x_continuous(labels = function(x) x) +
        ggplot2::scale_color_manual(values = c(
          "Births" = "#B79F00",
          "Deaths" = "#7A7A7A"
        )) +

        ggplot2::labs(
          title = "Births and mortalities over time",
          subtitle = "Annual births and deaths in the SRKW population",
          x = "Year",
          y = "Count",
          color = "Event"
        )
    })

    ##interactive data table
    output$data_table <- DT::renderDT({
      filtered_data()
    })

  }

  shiny::shinyApp(ui = ui, server = server)
}

