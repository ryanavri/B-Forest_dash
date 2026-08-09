# ---- Restoration Effort detail tab -------------------------------------------

mod_restoration_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      title = "Filters",
      width = 280,
      dateRangeInput(ns("date_range"), "Date range", start = NULL, end = NULL),
      selectizeInput(ns("sites"), "Restoration sites", choices = NULL, multiple = TRUE,
                      options = list(placeholder = "All sites")),
      selectizeInput(ns("species"), "Species planted", choices = NULL, multiple = TRUE,
                      options = list(placeholder = "All species"))
    ),
    layout_columns(
      class = "mb-4",
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Active Restoration Sites", value = textOutput(ns("kpi_sites")),
                 showcase = icon("tree"), theme = "success"),
      value_box(title = "Area Restored (ha)", value = textOutput(ns("kpi_area")),
                 showcase = icon("map"), theme = "primary"),
      value_box(title = "Seedlings Planted", value = textOutput(ns("kpi_planted")),
                 showcase = icon("seedling"), theme = "secondary"),
      value_box(title = "Avg. Survival Rate", value = textOutput(ns("kpi_survival")),
                 showcase = icon("heart-pulse"), theme = "info")
    ),
    layout_columns(
      class = "mb-4",
      col_widths = c(7, 5),
      card(
        full_screen = TRUE,
        card_header("Seedlings Planted Over Time (monthly)"),
        plotlyOutput(ns("plot_trend"), height = "300px")
      ),
      card(
        full_screen = TRUE,
        card_header("Top Species Planted"),
        plotlyOutput(ns("plot_species"), height = "300px")
      )
    ),
    layout_columns(
      class = "mb-4",
      col_widths = c(5, 7),
      card(
        full_screen = TRUE,
        card_header("Survival Rate Trend"),
        plotlyOutput(ns("plot_survival"), height = "340px")
      ),
      card(
        full_screen = TRUE,
        card_header("Restoration Sites Map"),
        leafletOutput(ns("map"), height = "340px")
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Planting Events"),
      DTOutput(ns("table"), fill = FALSE)
    )
  )
}

mod_restoration_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    status_colors <- c("Newly Planted" = "#f4a300", "Active Monitoring" = "#2c6e49",
                        "Established" = "#1f77b4")

    observe({
      updateDateRangeInput(session, "date_range",
        start = data$study_start, end = data$today,
        min = data$study_start, max = data$today)
      updateSelectizeInput(session, "sites",
        choices = stats::setNames(data$restoration_sites$site_id, data$restoration_sites$site_name),
        server = TRUE)
      updateSelectizeInput(session, "species",
        choices = stats::setNames(data$tree_species$species_id, data$tree_species$common_name),
        server = TRUE)
    })

    sites_f <- reactive({
      ids <- if (length(input$sites) > 0) input$sites else data$restoration_sites$site_id
      dplyr::filter(data$restoration_sites, site_id %in% ids)
    })

    planting_f <- reactive({
      req(input$date_range)
      df <- dplyr::filter(data$planting_events,
                            date >= input$date_range[1], date <= input$date_range[2],
                            site_id %in% sites_f()$site_id)
      if (length(input$species) > 0) df <- dplyr::filter(df, species_id %in% input$species)
      df |> dplyr::left_join(data$tree_species, by = "species_id")
    })

    survival_f <- reactive({
      req(input$date_range)
      df <- dplyr::filter(data$survival_monitoring,
                            date >= input$date_range[1], date <= input$date_range[2],
                            site_id %in% sites_f()$site_id)
      if (length(input$species) > 0) df <- dplyr::filter(df, species_id %in% input$species)
      df
    })

    output$kpi_sites <- renderText(scales::comma(sum(sites_f()$status != "Established")))
    output$kpi_area <- renderText(scales::comma(sum(sites_f()$area_ha), accuracy = 0.1))
    output$kpi_planted <- renderText(scales::comma(sum(planting_f()$seedlings_planted)))
    output$kpi_survival <- renderText({
      req(nrow(survival_f()) > 0)
      scales::percent(mean(survival_f()$survival_rate), accuracy = 0.1)
    })

    output$plot_trend <- renderPlotly({
      df <- planting_f()
      req(nrow(df) > 0)
      monthly <- df |>
        dplyr::mutate(month = lubridate::floor_date(date, "month")) |>
        dplyr::group_by(month) |>
        dplyr::summarise(seedlings = sum(seedlings_planted), .groups = "drop")

      p <- ggplot(monthly, aes(x = month, y = seedlings)) +
        geom_area(fill = "#2c6e49", alpha = 0.25) +
        geom_line(color = "#2c6e49", linewidth = 0.9) +
        labs(x = NULL, y = "Seedlings planted") +
        theme_minimal(base_size = 12)
      ggplotly(p)
    })

    output$plot_species <- renderPlotly({
      df <- planting_f()
      req(nrow(df) > 0)
      top <- df |>
        dplyr::group_by(common_name) |>
        dplyr::summarise(seedlings = sum(seedlings_planted), .groups = "drop") |>
        dplyr::slice_max(seedlings, n = 10) |>
        dplyr::arrange(seedlings)

      p <- ggplot(top, aes(x = seedlings, y = reorder(common_name, seedlings))) +
        geom_col(fill = "#8c564b") +
        labs(x = "Seedlings planted", y = NULL) +
        theme_minimal(base_size = 12)
      ggplotly(p, tooltip = c("x", "y"))
    })

    output$plot_survival <- renderPlotly({
      df <- survival_f()
      req(nrow(df) > 0)
      monthly <- df |>
        dplyr::mutate(month = lubridate::floor_date(date, "month")) |>
        dplyr::group_by(month) |>
        dplyr::summarise(survival_rate = mean(survival_rate), .groups = "drop")

      p <- ggplot(monthly, aes(x = month, y = survival_rate)) +
        geom_line(color = "#d62728", linewidth = 0.9) +
        geom_point(color = "#d62728") +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        labs(x = NULL, y = "Avg. survival rate") +
        theme_minimal(base_size = 12)
      ggplotly(p)
    })

    output$map <- renderLeaflet({
      sites <- sites_f()
      pal <- leaflet::colorFactor(status_colors[names(status_colors) %in% sites$status],
                                    domain = names(status_colors)[names(status_colors) %in% sites$status])

      leaflet(sites) |>
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
        addProviderTiles(providers$OpenStreetMap, group = "Streets") |>
        addCircleMarkers(
          lng = ~lon, lat = ~lat,
          radius = ~pmax(6, sqrt(area_ha) * 5),
          color = ~pal(status), fillOpacity = 0.8, stroke = FALSE,
          popup = ~sprintf("<b>%s</b><br>Habitat target: %s<br>Area: %.1f ha<br>Status: %s",
                            site_name, habitat_target, area_ha, status)
        ) |>
        add_kphp_boundary(data$kphp_boundary) |>
        addLayersControl(baseGroups = c("Satellite", "Streets"),
                           overlayGroups = c("KPHP VII Boundary"),
                           options = layersControlOptions(collapsed = FALSE)) |>
        addLegend(position = "bottomright", pal = pal, values = sites$status,
                   title = "Site status", opacity = 0.9)
    })

    output$table <- renderDT({
      df <- planting_f() |>
        dplyr::left_join(dplyr::select(data$restoration_sites, site_id, site_name), by = "site_id") |>
        dplyr::arrange(dplyr::desc(date)) |>
        dplyr::select(Date = date, Site = site_name, Species = common_name,
                       Team = team, `Seedlings Planted` = seedlings_planted)
      render_download_dt(df, filter = "top", order = list(list(0, "desc")))
    })

    unsuspend_all(output, c(
      "kpi_sites", "kpi_area", "kpi_planted", "kpi_survival",
      "plot_trend", "plot_species", "plot_survival", "map", "table"
    ))
  })
}
