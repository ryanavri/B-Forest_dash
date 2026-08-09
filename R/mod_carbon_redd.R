# ---- Carbon Stock: REDD++ Potential -----------------------------------------
# Models avoided deforestation/degradation as the gap between a business-
# as-usual baseline and the actual (patrol-protected) forest-loss trajectory
# - the standard REDD+ crediting logic. Illustrative only until a validated
# Reference Emission Level is available - see the sidebar note.

mod_carbon_redd_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      title = "Assumptions",
      width = 280,
      sliderInput(ns("horizon"), "Crediting period (years)", min = 5, max = 20, value = 10, step = 1),
      tags$hr(),
      tags$p(
        class = "small text-muted",
        "Baseline (business-as-usual) deforestation rate is a placeholder regional",
        "average; the actual rate reflects a modeled patrol-protection effect.",
        "Replace both with a registry-validated Reference Emission Level and",
        "monitored activity data when available."
      )
    ),
    layout_columns(
      class = "mb-4 mt-3",
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Forest Area Protected", value = textOutput(ns("kpi_area")),
                 p("ha", class = "small text-muted mb-0"),
                 showcase = icon("shield-halved"), theme = "success"),
      value_box(title = "Avoided Deforestation to Date", value = textOutput(ns("kpi_avoided_ha")),
                 p("ha", class = "small text-muted mb-0"),
                 showcase = icon("tree-city"), theme = "primary"),
      value_box(title = "Projected Avoided Emissions to Date", value = textOutput(ns("kpi_avoided_co2")),
                 p("tCO2e", class = "small text-muted mb-0"),
                 showcase = icon("cloud"), theme = "secondary"),
      value_box(
        title = "Equivalent To",
        value = textOutput(ns("kpi_equiv")),
        p("cars off the road for a year", class = "small text-muted mb-0"),
        showcase = icon("car"), theme = "info"
      )
    ),
    layout_columns(
      class = "mb-4",
      col_widths = 12,
      uiOutput(ns("verification_banner"))
    ),
    layout_columns(
      class = "mb-4",
      col_widths = 12,
      card(
        full_screen = TRUE,
        card_header("Baseline vs. Actual Forest Loss (avoided deforestation)"),
        plotlyOutput(ns("plot_wedge"), height = "380px")
      )
    ),
    layout_columns(
      class = "mb-4",
      col_widths = c(7, 5),
      card(
        full_screen = TRUE,
        card_header("Cumulative Avoided Emissions (projected)"),
        plotlyOutput(ns("plot_avoided_co2"), height = "320px")
      ),
      card(
        full_screen = TRUE,
        card_header("Threats Intercepted by SMART Patrols"),
        plotlyOutput(ns("plot_threats"), height = "320px")
      )
    )
  )
}

mod_carbon_redd_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    cp <- data$carbon_params
    verif <- dplyr::filter(data$carbon_verification, mechanism == "REDD++")
    years_elapsed <- as.numeric(difftime(data$today, data$study_start, units = "days")) / 365.25

    projection <- reactive({
      t <- seq(0, input$horizon, by = 0.25)
      baseline_loss <- cp$protected_forest_area_ha * (1 - (1 - cp$baseline_deforestation_rate)^t)
      actual_loss <- cp$protected_forest_area_ha * (1 - (1 - cp$actual_deforestation_rate)^t)
      tibble::tibble(
        year = t,
        baseline_loss_ha = baseline_loss,
        actual_loss_ha = actual_loss,
        avoided_ha = baseline_loss - actual_loss,
        avoided_tco2e = (baseline_loss - actual_loss) * cp$forest_carbon_density_tco2_ha
      )
    })

    to_date <- reactive({
      df <- projection()
      df[which.min(abs(df$year - pmin(years_elapsed, input$horizon))), ]
    })

    output$kpi_area <- renderText(scales::comma(cp$protected_forest_area_ha))
    output$kpi_avoided_ha <- renderText(scales::comma(round(to_date()$avoided_ha)))
    output$kpi_avoided_co2 <- renderText(scales::comma(round(to_date()$avoided_tco2e)))
    output$kpi_equiv <- renderText(scales::comma(round(to_date()$avoided_tco2e / cp$car_tco2_per_year)))

    output$plot_wedge <- renderPlotly({
      df <- projection()
      long <- df |>
        tidyr::pivot_longer(c(baseline_loss_ha, actual_loss_ha), names_to = "scenario", values_to = "loss_ha") |>
        dplyr::mutate(scenario = ifelse(scenario == "baseline_loss_ha",
                                          "Baseline (business-as-usual)", "Actual (protected)"))

      p <- ggplot() +
        geom_ribbon(data = df, aes(x = year, ymin = actual_loss_ha, ymax = baseline_loss_ha),
                     fill = "#2c6e49", alpha = 0.15) +
        geom_line(data = long, aes(x = year, y = loss_ha, color = scenario), linewidth = 1) +
        scale_color_manual(values = c("Baseline (business-as-usual)" = "#b45309",
                                        "Actual (protected)" = "#2c6e49"), name = NULL) +
        labs(x = "Project year", y = "Cumulative forest loss (ha)") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "top")

      if (years_elapsed <= max(df$year)) {
        p <- p + geom_vline(xintercept = years_elapsed, linetype = "dashed", color = "#888888")
      }
      ggplotly(p)
    })

    output$verification_banner <- renderUI({
      div(
        class = "alert alert-success d-flex align-items-center gap-2 mb-0",
        icon("circle-check"),
        tags$span(
          tags$strong(sprintf("%s tCO2e verified", scales::comma(verif$verified_tco2e))),
          sprintf(" as of %s by %s — the projected figures above are modeled and unverified until the next audit.",
                  format(verif$verification_date, "%d %b %Y"), verif$verifier)
        )
      )
    })

    output$plot_avoided_co2 <- renderPlotly({
      df <- projection()
      p <- ggplot(df, aes(x = year, y = avoided_tco2e)) +
        geom_area(fill = "#1f77b4", alpha = 0.25) +
        geom_line(color = "#1f77b4", linewidth = 1) +
        labs(x = "Project year", y = "Cumulative avoided tCO2e") +
        theme_minimal(base_size = 12)

      if (verif$verification_year <= input$horizon) {
        p <- p +
          geom_point(data = verif, aes(x = verification_year, y = verified_tco2e),
                      inherit.aes = FALSE, color = "#1a7f37", size = 3) +
          annotate("text", x = verif$verification_year, y = verif$verified_tco2e,
                    label = "Verified", vjust = -1, size = 3.3, color = "#1a7f37")
      }
      ggplotly(p)
    })

    output$plot_threats <- renderPlotly({
      df <- data$patrol_observations |>
        dplyr::filter(finding_type %in% c("Illegal Logging", "Encroachment")) |>
        dplyr::mutate(month = lubridate::floor_date(as.Date(datetime), "month")) |>
        dplyr::count(month, finding_type)
      req(nrow(df) > 0)

      p <- ggplot(df, aes(x = month, y = n, fill = finding_type)) +
        geom_col() +
        scale_fill_manual(values = c("Illegal Logging" = "#b45309", "Encroachment" = "#c2410c"), name = NULL) +
        labs(x = NULL, y = "Threats intercepted") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "top")
      ggplotly(p)
    })

    unsuspend_all(output, c(
      "kpi_area", "kpi_avoided_ha", "kpi_avoided_co2", "kpi_equiv",
      "plot_wedge", "verification_banner", "plot_avoided_co2", "plot_threats"
    ))
  })
}
