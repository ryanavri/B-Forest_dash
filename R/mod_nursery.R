# ---- Nursery detail tab -------------------------------------------------------

mod_nursery_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      title = "Filters",
      width = 280,
      dateRangeInput(ns("date_range"), "Sow date range", start = NULL, end = NULL),
      selectizeInput(ns("species"), "Species", choices = NULL, multiple = TRUE,
                      options = list(placeholder = "All species")),
      selectizeInput(ns("status"), "Batch status", choices = NULL, multiple = TRUE,
                      options = list(placeholder = "All statuses"))
    ),
    layout_columns(
      class = "mb-4",
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Seedlings in Stock", value = textOutput(ns("kpi_stock")),
                 showcase = icon("seedling"), theme = "success"),
      value_box(title = "Germination Rate", value = textOutput(ns("kpi_germ")),
                 showcase = icon("droplet"), theme = "info"),
      value_box(title = "Batches Ready for Planting", value = textOutput(ns("kpi_ready")),
                 showcase = icon("box-open"), theme = "primary"),
      value_box(title = "Active Batches", value = textOutput(ns("kpi_active")),
                 showcase = icon("layer-group"), theme = "secondary")
    ),
    layout_columns(
      class = "mb-4",
      col_widths = c(7, 5),
      card(
        full_screen = TRUE,
        card_header("Sown vs. Germinated (monthly)"),
        plotlyOutput(ns("plot_trend"), height = "300px")
      ),
      card(
        full_screen = TRUE,
        card_header("Batches by Status"),
        plotlyOutput(ns("plot_status"), height = "300px")
      )
    ),
    layout_columns(
      class = "mb-4",
      col_widths = c(5, 7),
      card(
        full_screen = TRUE,
        card_header("Current Stock by Species"),
        plotlyOutput(ns("plot_species"), height = "340px")
      ),
      card(
        full_screen = TRUE,
        card_header("Nursery to Restoration Site Dispatch"),
        leafletOutput(ns("map"), height = "340px")
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Batch Inventory"),
      DTOutput(ns("table"), fill = FALSE)
    )
  )
}

mod_nursery_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    status_colors <- c("Germinating" = "#f4a300", "Growing" = "#2c6e49",
                        "Ready for Planting" = "#1f77b4", "Dispatched" = "#7f7f7f")

    dispatched_by_batch <- data$nursery_dispatches |>
      dplyr::group_by(batch_id) |>
      dplyr::summarise(dispatched = sum(quantity), .groups = "drop")

    observe({
      updateDateRangeInput(session, "date_range",
        start = data$study_start, end = data$today,
        min = data$study_start, max = data$today)
      updateSelectizeInput(session, "species",
        choices = stats::setNames(data$tree_species$species_id, data$tree_species$common_name),
        server = TRUE)
      updateSelectizeInput(session, "status",
        choices = sort(unique(data$nursery_batches$status)), server = TRUE)
    })

    batches_f <- reactive({
      req(input$date_range)
      df <- data$nursery_batches |>
        dplyr::filter(sow_date >= input$date_range[1], sow_date <= input$date_range[2]) |>
        dplyr::left_join(dispatched_by_batch, by = "batch_id") |>
        dplyr::mutate(
          dispatched = tidyr::replace_na(dispatched, 0),
          stock = pmax(0, ready_count - dispatched)
        ) |>
        dplyr::left_join(data$tree_species, by = "species_id")
      if (length(input$species) > 0) df <- dplyr::filter(df, species_id %in% input$species)
      if (length(input$status) > 0) df <- dplyr::filter(df, status %in% input$status)
      df
    })

    dispatches_f <- reactive({
      dplyr::filter(data$nursery_dispatches, batch_id %in% batches_f()$batch_id)
    })

    output$kpi_stock <- renderText(scales::comma(sum(batches_f()$stock)))
    output$kpi_germ <- renderText({
      df <- batches_f()
      req(nrow(df) > 0)
      scales::percent(sum(df$germinated_count) / sum(df$quantity_sown), accuracy = 0.1)
    })
    output$kpi_ready <- renderText(scales::comma(sum(batches_f()$status == "Ready for Planting")))
    output$kpi_active <- renderText({
      scales::comma(sum(batches_f()$status %in% c("Germinating", "Growing", "Ready for Planting")))
    })

    output$plot_trend <- renderPlotly({
      df <- batches_f()
      req(nrow(df) > 0)
      monthly <- df |>
        dplyr::mutate(month = lubridate::floor_date(sow_date, "month")) |>
        dplyr::group_by(month) |>
        dplyr::summarise(Sown = sum(quantity_sown), Germinated = sum(germinated_count), .groups = "drop") |>
        tidyr::pivot_longer(c(Sown, Germinated), names_to = "metric", values_to = "count")

      p <- ggplot(monthly, aes(x = month, y = count, color = metric)) +
        geom_line(linewidth = 0.9) +
        geom_point() +
        scale_color_manual(values = c(Sown = "#8c564b", Germinated = "#2c6e49"), name = NULL) +
        labs(x = NULL, y = "Seedlings") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "top")
      ggplotly(p)
    })

    output$plot_status <- renderPlotly({
      df <- batches_f()
      req(nrow(df) > 0)
      counts <- df |>
        dplyr::count(status, name = "n") |>
        dplyr::arrange(n)

      p <- ggplot(counts, aes(x = n, y = reorder(status, n), fill = status)) +
        geom_col(show.legend = FALSE) +
        scale_fill_manual(values = status_colors) +
        labs(x = "Batches", y = NULL) +
        theme_minimal(base_size = 12)
      ggplotly(p, tooltip = c("x", "y"))
    })

    output$plot_species <- renderPlotly({
      df <- batches_f()
      req(nrow(df) > 0)
      top <- df |>
        dplyr::group_by(common_name) |>
        dplyr::summarise(stock = sum(stock), .groups = "drop") |>
        dplyr::slice_max(stock, n = 10) |>
        dplyr::arrange(stock)

      p <- ggplot(top, aes(x = stock, y = reorder(common_name, stock))) +
        geom_col(fill = "#2c6e49") +
        labs(x = "Seedlings in stock", y = NULL) +
        theme_minimal(base_size = 12)
      ggplotly(p, tooltip = c("x", "y"))
    })

    output$map <- renderLeaflet({
      site_totals <- dispatches_f() |>
        dplyr::group_by(site_id) |>
        dplyr::summarise(dispatched = sum(quantity), .groups = "drop") |>
        dplyr::left_join(data$restoration_sites, by = "site_id") |>
        dplyr::filter(!is.na(lat))

      m <- leaflet() |>
        addProviderTiles(providers$OpenStreetMap) |>
        addAwesomeMarkers(
          data = data$nursery_facility, lng = ~lon, lat = ~lat,
          icon = leaflet::awesomeIcons(icon = "leaf", markerColor = "green", library = "fa"),
          popup = ~facility_name
        )

      if (nrow(site_totals) > 0) {
        for (i in seq_len(nrow(site_totals))) {
          m <- m |>
            addPolylines(
              lng = c(data$nursery_facility$lon, site_totals$lon[i]),
              lat = c(data$nursery_facility$lat, site_totals$lat[i]),
              color = "#2c6e49", weight = 1.5, opacity = 0.5, dashArray = "4 4"
            )
        }
        m <- m |>
          addCircleMarkers(
            data = site_totals, lng = ~lon, lat = ~lat,
            radius = ~pmax(6, sqrt(dispatched) / 3),
            color = "#1f77b4", fillOpacity = 0.8, stroke = FALSE,
            popup = ~sprintf("<b>%s</b><br>Seedlings received: %s", site_name, scales::comma(dispatched))
          )
      }
      m |> add_kphp_boundary(data$kphp_boundary)
    })

    output$table <- renderDT({
      df <- batches_f() |>
        dplyr::arrange(dplyr::desc(sow_date)) |>
        dplyr::select(Batch = batch_id, Species = common_name, Source = seed_source,
                       `Sow Date` = sow_date, Sown = quantity_sown,
                       `Germination Rate` = germination_rate, `Ready` = ready_count,
                       `In Stock` = stock, Status = status)
      render_download_dt(df, filter = "top", order = list(list(3, "desc"))) |>
        formatPercentage("Germination Rate", 1)
    })

    unsuspend_all(output, c(
      "kpi_stock", "kpi_germ", "kpi_ready", "kpi_active",
      "plot_trend", "plot_status", "plot_species", "map", "table"
    ))
  })
}
