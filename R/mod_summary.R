# ---- Biodiversity Summary tab ---------------------------------------------
# Park-wide overview combining camera trap + bioacoustic + SMART patrol data.

mod_summary_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      class = "mb-4",
      col_widths = 12,
      dateRangeInput(ns("date_range"), "Date range",
                      start = NULL, end = NULL, width = "320px")
    ),
    layout_columns(
      class = "mb-4",
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Species Recorded", value = textOutput(ns("kpi_species")),
                 showcase = icon("paw"), theme = "success"),
      value_box(title = "Camera Trap Detections", value = textOutput(ns("kpi_camera")),
                 showcase = icon("camera"), theme = "primary"),
      value_box(title = "Bioacoustic Detections", value = textOutput(ns("kpi_acoustic")),
                 showcase = icon("volume-up"), theme = "info"),
      value_box(title = "Patrols Conducted", value = textOutput(ns("kpi_patrol")),
                 showcase = icon("route"), theme = "warning")
    ),
    layout_columns(
      class = "mb-4",
      col_widths = c(6, 6),
      card(
        full_screen = TRUE,
        card_header("Species Richness by Taxonomic Group"),
        plotlyOutput(ns("plot_taxon"), height = "320px")
      ),
      card(
        full_screen = TRUE,
        card_header("Cumulative Unique Species Recorded Over Time"),
        plotlyOutput(ns("plot_accum"), height = "320px")
      )
    ),
    layout_columns(
      class = "mb-4",
      col_widths = c(7, 5),
      card(
        full_screen = TRUE,
        card_header("Monitoring & Patrol Coverage Map"),
        leafletOutput(ns("map"), height = "420px")
      ),
      card(
        full_screen = TRUE,
        card_header("Most Frequently Detected Species"),
        DTOutput(ns("top_species"), fill = FALSE)
      )
    )
  )
}

mod_summary_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      updateDateRangeInput(session, "date_range",
        start = data$study_start, end = data$today,
        min = data$study_start, max = data$today)
    })

    dr <- reactive({
      req(input$date_range)
      input$date_range
    })

    cam_f <- reactive({
      dplyr::filter(data$camera_detections,
                     as.Date(datetime) >= dr()[1], as.Date(datetime) <= dr()[2])
    })
    ac_f <- reactive({
      dplyr::filter(data$acoustic_detections,
                     as.Date(datetime) >= dr()[1], as.Date(datetime) <= dr()[2])
    })
    pt_f <- reactive({
      dplyr::filter(data$patrols, start_date >= dr()[1], start_date <= dr()[2])
    })

    output$kpi_species <- renderText({
      n <- length(union(cam_f()$species_id, ac_f()$species_id))
      scales::comma(n)
    })
    output$kpi_camera <- renderText(scales::comma(nrow(cam_f())))
    output$kpi_acoustic <- renderText(scales::comma(nrow(ac_f())))
    output$kpi_patrol <- renderText(scales::comma(nrow(pt_f())))

    output$plot_taxon <- renderPlotly({
      detected_ids <- union(cam_f()$species_id, ac_f()$species_id)
      df <- data$species |>
        dplyr::filter(species_id %in% detected_ids) |>
        dplyr::count(taxon_group, name = "n") |>
        dplyr::arrange(n)

      p <- ggplot(df, aes(x = n, y = reorder(taxon_group, n), fill = taxon_group)) +
        geom_col(show.legend = FALSE) +
        labs(x = "Species richness", y = NULL) +
        theme_minimal(base_size = 12)
      ggplotly(p, tooltip = c("x", "y"))
    })

    output$plot_accum <- renderPlotly({
      cam_dates <- dplyr::transmute(cam_f(), species_id, date = as.Date(datetime))
      ac_dates <- dplyr::transmute(ac_f(), species_id, date = as.Date(datetime))
      combined <- dplyr::bind_rows(cam_dates, ac_dates)
      req(nrow(combined) > 0)

      first_seen <- combined |>
        dplyr::group_by(species_id) |>
        dplyr::summarise(first_date = min(date), .groups = "drop") |>
        dplyr::arrange(first_date) |>
        dplyr::mutate(cumulative_species = dplyr::row_number())

      p <- ggplot(first_seen, aes(x = first_date, y = cumulative_species)) +
        geom_step(color = "#2c6e49", linewidth = 0.9) +
        labs(x = NULL, y = "Cumulative species") +
        theme_minimal(base_size = 12)
      ggplotly(p)
    })

    output$map <- renderLeaflet({
      m <- leaflet() |>
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
        addProviderTiles(providers$OpenStreetMap, group = "Streets") |>
        addCircleMarkers(
          data = data$camera_sites, lng = ~longitude, lat = ~latitude,
          radius = 6, color = "#1f77b4", fillOpacity = 0.85, stroke = FALSE,
          group = "Camera Trap Stations",
          popup = ~sprintf("<b>%s</b><br>Camera trap station<br>Status: %s<br>Trap Nights: %s",
                            station_id, status, scales::comma(trap_nights))
        ) |>
        addCircleMarkers(
          data = data$acoustic_sites, lng = ~lon, lat = ~lat,
          radius = 6, color = "#2ca02c", fillOpacity = 0.85, stroke = FALSE,
          group = "Acoustic Recorders",
          popup = ~sprintf("<b>%s</b><br>Acoustic recorder<br>Habitat: %s<br>Status: %s",
                            recorder_id, habitat, status)
        )

      tracks <- dplyr::filter(data$patrol_tracks, has_track)
      if (nrow(tracks) > 0) {
        for (i in seq_len(nrow(tracks))) {
          r <- tracks[i, ]
          m <- m |> addPolylines(
            data = r, color = "#cc5500", weight = 2, opacity = 0.7,
            group = "Patrol Tracks",
            popup = sprintf("<b>%s</b><br>Distance: %.1f km<br>Date: %s",
                             r$patrol_id, r$distance_km, format(r$start_date, "%d %b %Y"))
          )
        }
      }

      m |>
        add_kphp_boundary(data$kphp_boundary) |>
        addLayersControl(
          baseGroups = c("Satellite", "Streets"),
          overlayGroups = c("Camera Trap Stations", "Acoustic Recorders", "Patrol Tracks", "KPHP VII Boundary"),
          options = layersControlOptions(collapsed = FALSE)
        ) |>
        addLegend(position = "bottomright",
                   colors = c("#1f77b4", "#2ca02c", "#cc5500"),
                   labels = c("Camera trap", "Acoustic recorder", "Patrol track"),
                   opacity = 0.9)
    })

    output$top_species <- renderDT({
      cam_n <- dplyr::count(cam_f(), species_id, name = "camera")
      ac_n <- dplyr::count(ac_f(), species_id, name = "acoustic")

      df <- data$species |>
        dplyr::select(species_id, common_name, taxon_group, conservation_status) |>
        dplyr::left_join(cam_n, by = "species_id") |>
        dplyr::left_join(ac_n, by = "species_id") |>
        dplyr::mutate(
          camera = tidyr::replace_na(camera, 0),
          acoustic = tidyr::replace_na(acoustic, 0),
          total = camera + acoustic
        ) |>
        dplyr::filter(total > 0) |>
        dplyr::arrange(dplyr::desc(total)) |>
        dplyr::select(Species = common_name, Group = taxon_group,
                       Status = conservation_status,
                       `Camera` = camera, `Acoustic` = acoustic, `Total` = total) |>
        head(12)

      render_download_dt(df, page_length = 12, dom = "Btip")
    })

    unsuspend_all(output, c(
      "kpi_species", "kpi_camera", "kpi_acoustic", "kpi_patrol",
      "plot_taxon", "plot_accum", "map", "top_species"
    ))
  })
}
