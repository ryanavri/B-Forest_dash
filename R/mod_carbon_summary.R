# ---- Carbon Stock Summary tab --------------------------------------------
# Rollup across ARR and REDD++ - headline potential vs. what's actually been
# third-party verified so far, so the two mechanisms' detail tabs don't have
# to be visited just to get the big picture.

mod_carbon_summary_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      class = "mb-4",
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Active Restoration Sites", value = textOutput(ns("kpi_sites_active")),
                 p("ARR basis", class = "small text-muted mb-0"),
                 showcase = icon("tree"), theme = "success"),
      value_box(title = "Forest Area Protected", value = textOutput(ns("kpi_forest_area")),
                 p("ha — REDD++ basis", class = "small text-muted mb-0"),
                 showcase = icon("shield-halved"), theme = "primary"),
      value_box(title = "Combined Long-Term Potential", value = textOutput(ns("kpi_combined")),
                 p("tCO2e — projected, unverified", class = "small text-muted mb-0"),
                 showcase = icon("cloud"), theme = "secondary"),
      value_box(title = "Total Verified to Date", value = textOutput(ns("kpi_verified")),
                 p("tCO2e — third-party audited", class = "small text-muted mb-0"),
                 showcase = icon("circle-check"), theme = "info")
    ),
    layout_columns(
      class = "mb-4",
      col_widths = 12,
      card(
        full_screen = TRUE,
        card_header("Projected vs. Verified, by Mechanism (same point in time)"),
        plotlyOutput(ns("plot_compare"), height = "360px")
      )
    )
  )
}

mod_carbon_summary_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    cp <- data$carbon_params

    output$kpi_sites_active <- renderText({
      scales::comma(sum(data$restoration_sites$status != "Established"))
    })
    output$kpi_forest_area <- renderText(scales::comma(cp$protected_forest_area_ha))

    output$kpi_combined <- renderText({
      arr_total <- chapman_richards(cp$arr_horizon_years, cp$arr_asymptote_tco2_ha, cp$arr_k, cp$arr_p) *
        sum(data$restoration_sites$area_ha)
      baseline <- cp$protected_forest_area_ha * (1 - (1 - cp$baseline_deforestation_rate)^cp$redd_horizon_years)
      actual <- cp$protected_forest_area_ha * (1 - (1 - cp$actual_deforestation_rate)^cp$redd_horizon_years)
      redd_total <- (baseline - actual) * cp$forest_carbon_density_tco2_ha
      scales::comma(round(arr_total + redd_total))
    })

    output$kpi_verified <- renderText(scales::comma(sum(data$carbon_verification$verified_tco2e)))

    output$plot_compare <- renderPlotly({
      df <- data$carbon_verification |>
        tidyr::pivot_longer(c(modeled_tco2e_at_verification, verified_tco2e),
                              names_to = "type", values_to = "tco2e") |>
        dplyr::mutate(type = ifelse(type == "modeled_tco2e_at_verification",
                                      "Projected (modeled)", "Verified (audited)"))

      p <- ggplot(df, aes(x = mechanism, y = tco2e, fill = type)) +
        geom_col(position = position_dodge(width = 0.6), width = 0.55) +
        scale_fill_manual(values = c("Projected (modeled)" = "#9dbf9e", "Verified (audited)" = "#1a7f37"), name = NULL) +
        labs(x = NULL, y = sprintf("tCO2e (as of %s)", format(data$carbon_verification$verification_date[1], "%d %b %Y"))) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "top")
      ggplotly(p, tooltip = c("x", "y", "fill"))
    })

    unsuspend_all(output, c(
      "kpi_sites_active", "kpi_forest_area", "kpi_combined", "kpi_verified", "plot_compare"
    ))
  })
}
