# ---- Restoration Summary tab -------------------------------------------------
# Rollup across the restoration-site and nursery pipelines - a quick "how is
# the whole restoration program doing" view before drilling into Restoration
# Effort or Nursery individually.

mod_restoration_summary_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      class = "mb-4",
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Active Restoration Sites", value = textOutput(ns("kpi_sites_active")),
                 p(textOutput(ns("kpi_sites_active_sub")), class = "small text-muted mb-0"),
                 showcase = icon("tree"), theme = "success"),
      value_box(title = "Total Area Restored", value = textOutput(ns("kpi_area")),
                 p("ha", class = "small text-muted mb-0"),
                 showcase = icon("map"), theme = "primary"),
      value_box(title = "Active Nursery Batches", value = textOutput(ns("kpi_batches_active")),
                 showcase = icon("seedling"), theme = "secondary"),
      value_box(title = "Seedlings Ready for Planting", value = textOutput(ns("kpi_ready")),
                 showcase = icon("box-open"), theme = "info")
    ),
    layout_columns(
      class = "mb-4",
      col_widths = c(5, 7),
      card(
        full_screen = TRUE,
        card_header("Restoration & Nursery Pipeline (cumulative totals)"),
        plotlyOutput(ns("plot_pipeline"), height = "360px")
      ),
      card(
        full_screen = TRUE,
        card_header("Restoration Sites & Nursery Facility"),
        leafletOutput(ns("map"), height = "360px")
      )
    )
  )
}

mod_restoration_summary_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    status_colors <- c("Newly Planted" = "#f4a300", "Active Monitoring" = "#2c6e49", "Established" = "#1f77b4")

    dispatched_by_batch <- data$nursery_dispatches |>
      dplyr::group_by(batch_id) |>
      dplyr::summarise(dispatched = sum(quantity), .groups = "drop")

    batches <- data$nursery_batches |>
      dplyr::left_join(dispatched_by_batch, by = "batch_id") |>
      dplyr::mutate(
        dispatched = tidyr::replace_na(dispatched, 0),
        stock = pmax(0, ready_count - dispatched)
      )

    output$kpi_sites_active <- renderText({
      scales::comma(sum(data$restoration_sites$status != "Established"))
    })
    output$kpi_sites_active_sub <- renderText({
      sprintf("of %s total", scales::comma(nrow(data$restoration_sites)))
    })
    output$kpi_area <- renderText(scales::comma(round(sum(data$restoration_sites$area_ha), 1)))
    output$kpi_batches_active <- renderText({
      scales::comma(sum(batches$status %in% c("Germinating", "Growing", "Ready for Planting")))
    })
    output$kpi_ready <- renderText(scales::comma(sum(batches$stock)))

    output$plot_pipeline <- renderPlotly({
      stages <- tibble::tibble(
        stage = factor(c("Seeds Sown", "Germinated", "Ready for Planting", "Planted in Field"),
                        levels = c("Planted in Field", "Ready for Planting", "Germinated", "Seeds Sown")),
        count = c(
          sum(data$nursery_batches$quantity_sown),
          sum(data$nursery_batches$germinated_count),
          sum(data$nursery_batches$ready_count),
          sum(data$planting_events$seedlings_planted)
        )
      )

      p <- ggplot(stages, aes(x = count, y = stage)) +
        geom_col(fill = "#2c6e49") +
        labs(x = "Seedlings", y = NULL) +
        theme_minimal(base_size = 12)
      ggplotly(p, tooltip = c("x", "y"))
    })

    output$map <- renderLeaflet({
      sites <- data$restoration_sites
      pal <- leaflet::colorFactor(status_colors[names(status_colors) %in% sites$status],
                                    domain = names(status_colors)[names(status_colors) %in% sites$status])

      leaflet() |>
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
        addProviderTiles(providers$OpenStreetMap, group = "Streets") |>
        addCircleMarkers(
          data = sites, lng = ~lon, lat = ~lat,
          radius = ~pmax(6, sqrt(area_ha) * 4), color = ~pal(status), fillOpacity = 0.8, stroke = FALSE,
          group = "Restoration Sites",
          popup = ~sprintf("<b>%s</b><br>Area: %.1f ha<br>Status: %s", site_name, area_ha, status)
        ) |>
        addAwesomeMarkers(
          data = data$nursery_facility, lng = ~lon, lat = ~lat,
          icon = leaflet::awesomeIcons(icon = "leaf", markerColor = "green", library = "fa"),
          group = "Nursery",
          popup = ~facility_name
        ) |>
        add_kphp_boundary(data$kphp_boundary) |>
        addLayersControl(
          baseGroups = c("Satellite", "Streets"),
          overlayGroups = c("Restoration Sites", "Nursery", "KPHP VII Boundary"),
          options = layersControlOptions(collapsed = FALSE)
        ) |>
        addLegend(position = "bottomright", pal = pal, values = sites$status,
                   title = "Site status", opacity = 0.9)
    })

    unsuspend_all(output, c(
      "kpi_sites_active", "kpi_sites_active_sub", "kpi_area",
      "kpi_batches_active", "kpi_ready", "plot_pipeline", "map"
    ))
  })
}
