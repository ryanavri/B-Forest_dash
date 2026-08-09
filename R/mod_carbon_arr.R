# ---- Carbon Stock: ARR (Afforestation/Reforestation/Restoration) Potential ---
# Models projected carbon sequestration from active restoration blocks using
# a Chapman-Richards growth curve. Illustrative only until real biomass/
# allometric field data is available - see the sidebar note and card
# captions for the placeholder assumptions in play.

mod_carbon_arr_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      title = "Assumptions",
      width = 280,
      sliderInput(ns("horizon"), "Project horizon (years)", min = 5, max = 30, value = 30, step = 1),
      selectizeInput(ns("sites"), "Restoration sites", choices = NULL, multiple = TRUE,
                      options = list(placeholder = "All sites")),
      tags$hr(),
      tags$p(
        class = "small text-muted",
        "Modeled with a Chapman-Richards growth curve calibrated to typical tropical",
        "reforestation biomass accumulation — a widely used forestry growth model,",
        "but with placeholder parameters. Replace with field-measured allometric or",
        "remote-sensing biomass data for a bankable estimate."
      )
    ),
    layout_columns(
      class = "mb-4 mt-3",
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Area Under Restoration", value = textOutput(ns("kpi_area")),
                 showcase = icon("tree"), theme = "success"),
      value_box(title = "Peak Annual Sequestration", value = textOutput(ns("kpi_peak")),
                 p("tCO2e / year", class = "small text-muted mb-0"),
                 showcase = icon("arrow-trend-up"), theme = "primary"),
      value_box(
        title = "Projected Cumulative",
        value = textOutput(ns("kpi_cumulative")),
        p(textOutput(ns("kpi_cumulative_sub")), class = "small text-muted mb-0"),
        showcase = icon("cloud"), theme = "secondary"
      ),
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
        card_header("Projected Cumulative Carbon Sequestration"),
        plotlyOutput(ns("plot_curve"), height = "380px")
      )
    ),
    layout_columns(
      class = "mb-4",
      col_widths = c(5, 7),
      card(
        full_screen = TRUE,
        card_header("Potential Carbon Stock at Maturity, by Species"),
        plotlyOutput(ns("plot_species"), height = "360px")
      ),
      card(
        full_screen = TRUE,
        card_header("Restoration Sites by Carbon Potential"),
        leafletOutput(ns("map"), height = "360px")
      )
    )
  )
}

mod_carbon_arr_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cp <- data$carbon_params
    verif <- dplyr::filter(data$carbon_verification, mechanism == "ARR")

    observe({
      updateSelectizeInput(session, "sites",
        choices = stats::setNames(data$restoration_sites$site_id, data$restoration_sites$site_name),
        server = TRUE)
    })

    sites_f <- reactive({
      ids <- if (length(input$sites) > 0) input$sites else data$restoration_sites$site_id
      dplyr::filter(data$restoration_sites, site_id %in% ids)
    })

    total_area <- reactive(sum(sites_f()$area_ha))

    curve_df <- reactive({
      req(total_area() > 0)
      t <- seq(0, input$horizon, by = 0.5)
      per_ha <- chapman_richards(t, cp$arr_asymptote_tco2_ha, cp$arr_k, cp$arr_p)
      tibble::tibble(year = t, cum_tco2e = per_ha * total_area())
    })

    output$kpi_area <- renderText(scales::comma(round(total_area(), 1)))

    output$kpi_peak <- renderText({
      df <- curve_df()
      req(nrow(df) > 1)
      annual <- diff(df$cum_tco2e) / diff(df$year)
      scales::comma(round(max(annual)))
    })

    output$kpi_cumulative <- renderText({
      df <- curve_df()
      req(nrow(df) > 0)
      scales::comma(round(max(df$cum_tco2e)))
    })
    output$kpi_cumulative_sub <- renderText(sprintf("tCO2e by year %d", input$horizon))

    output$kpi_equiv <- renderText({
      df <- curve_df()
      req(nrow(df) > 0)
      scales::comma(round(max(df$cum_tco2e) / cp$car_tco2_per_year))
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

    output$plot_curve <- renderPlotly({
      df <- curve_df()
      req(nrow(df) > 0)
      p <- ggplot(df, aes(x = year, y = cum_tco2e)) +
        geom_area(fill = "#2c6e49", alpha = 0.25) +
        geom_line(color = "#2c6e49", linewidth = 1.1) +
        labs(x = "Project year", y = "Cumulative tCO2e sequestered") +
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

    output$plot_species <- renderPlotly({
      top <- dplyr::arrange(data$tree_species, carbon_potential_tco2_ha)
      p <- ggplot(top, aes(x = carbon_potential_tco2_ha, y = reorder(common_name, carbon_potential_tco2_ha),
                             fill = type, text = scientific_name)) +
        geom_col() +
        labs(x = "tCO2e / ha at maturity", y = NULL, fill = "Growth type") +
        theme_minimal(base_size = 12)
      ggplotly(p, tooltip = c("x", "y", "fill", "text"))
    })

    output$map <- renderLeaflet({
      sites <- sites_f() |> dplyr::mutate(potential_tco2 = area_ha * cp$arr_asymptote_tco2_ha)
      req(nrow(sites) > 0)
      pal <- leaflet::colorNumeric("Greens", domain = sites$potential_tco2)

      leaflet(sites) |>
        addProviderTiles(providers$OpenStreetMap) |>
        addCircleMarkers(
          lng = ~lon, lat = ~lat, radius = ~pmax(6, sqrt(area_ha) * 3),
          color = ~pal(potential_tco2), fillOpacity = 0.85, stroke = FALSE,
          popup = ~sprintf("<b>%s</b><br>Area: %.1f ha<br>Potential at maturity: %s tCO2e",
                            site_name, area_ha, scales::comma(round(potential_tco2)))
        ) |>
        addLegend(position = "bottomright", pal = pal, values = sites$potential_tco2,
                   title = "Potential tCO2e") |>
        add_kphp_boundary(data$kphp_boundary)
    })

    unsuspend_all(output, c(
      "kpi_area", "kpi_peak", "kpi_cumulative", "kpi_cumulative_sub", "kpi_equiv",
      "verification_banner", "plot_curve", "plot_species", "map"
    ))
  })
}
