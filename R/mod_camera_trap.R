# ---- Camera Trap detail tab -------------------------------------------------
# Two sub-tabs, backed by real camera trap data (data/camtrap_data.RData,
# loaded via R/load_camtrap_data.R):
#  - "Overall Effort": park-wide camera trap dashboard (trap nights,
#    detections, station coverage, species richness).
#  - "Species Information": single-species profile (ID card, RAI by
#    station, diel activity, detection map/records).
#
# The real deployment log has no habitat/elevation fields and only a single
# survey session, so the old "Active Station" sub-tab, elevation chart, and
# session filter (all mock-data constructs) have been dropped rather than
# faked.

mod_camera_trap_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      title = "Filters",
      width = 280,
      dateRangeInput(ns("date_range"), "Date range", start = NULL, end = NULL),
      selectizeInput(ns("sessions"), "Session", choices = NULL, multiple = TRUE,
                      options = list(placeholder = "All sessions")),
      selectizeInput(ns("stations"), "Stations", choices = NULL, multiple = TRUE,
                      options = list(placeholder = "All stations"))
    ),
    navset_tab(
      id = ns("subtab"),
      nav_panel(
        "Overall Effort",
        icon = icon("chart-column"),
        layout_columns(
          class = "mb-4 mt-3",
          col_widths = c(3, 3, 3, 3),
          value_box(title = "Total Trap Nights", value = textOutput(ns("kpi_nights")),
                     showcase = icon("moon"), theme = "secondary"),
          value_box(title = "Total Detections", value = textOutput(ns("kpi_detections")),
                     showcase = icon("camera"), theme = "success"),
          value_box(title = "Total Unique Stations", value = textOutput(ns("kpi_stations")),
                     showcase = icon("tower-broadcast"), theme = "primary"),
          value_box(title = "Total Species Richness", value = textOutput(ns("kpi_richness")),
                     showcase = icon("paw"), theme = "info")
        ),
        layout_columns(
          class = "mb-4",
          col_widths = c(3, 3, 3, 3),
          value_box(title = "Total Protected Species", value = textOutput(ns("kpi_protected")),
                     showcase = icon("shield-heart"), theme = "success"),
          value_box(title = "Critically Endangered (CR)", value = textOutput(ns("kpi_cr")),
                     showcase = icon("triangle-exclamation"), theme = "danger"),
          value_box(title = "Endangered (EN)", value = textOutput(ns("kpi_en")),
                     showcase = icon("triangle-exclamation"), theme = "warning"),
          value_box(title = "Vulnerable (VU)", value = textOutput(ns("kpi_vu")),
                     showcase = icon("triangle-exclamation"), theme = "secondary")
        ),
        layout_columns(
          class = "mb-4",
          col_widths = c(5, 7),
          card(
            full_screen = TRUE,
            card_header("Top Detected Species (Independent Events)"),
            plotlyOutput(ns("plot_species"), height = "340px")
          ),
          card(
            full_screen = TRUE,
            card_header("Camera Station Map"),
            leafletOutput(ns("map"), height = "340px")
          )
        ),
        card(
          full_screen = TRUE,
          card_header("Detection Records"),
          DTOutput(ns("table"), fill = FALSE)
        )
      ),
      nav_panel(
        "Species Information",
        icon = icon("paw"),
        layout_columns(
          class = "mb-3 mt-3",
          col_widths = 12,
          selectInput(ns("species_profile"), "Select a species", choices = NULL, width = "340px")
        ),
        layout_columns(
          class = "mb-4",
          col_widths = 12,
          uiOutput(ns("species_card"))
        ),
        layout_columns(
          class = "mb-4",
          col_widths = c(3, 3, 3, 3),
          value_box(title = "Total Detections", value = textOutput(ns("profile_kpi_detections")),
                     showcase = icon("camera"), theme = "success"),
          value_box(title = "Unique Stations", value = textOutput(ns("profile_kpi_stations")),
                     showcase = icon("tower-broadcast"), theme = "primary"),
          value_box(title = "First Detection", value = textOutput(ns("profile_kpi_first")),
                     showcase = icon("calendar-day"), theme = "secondary"),
          value_box(title = "Peak Activity Hour", value = textOutput(ns("profile_kpi_peak_hour")),
                     showcase = icon("clock"), theme = "info")
        ),
        layout_columns(
          class = "mb-4",
          col_widths = c(6, 6),
          card(
            full_screen = TRUE,
            card_header("RAI by Station (independent events / 100 trap-nights)"),
            plotlyOutput(ns("profile_rai"), height = "320px")
          ),
          card(
            full_screen = TRUE,
            card_header("Diel Activity Pattern"),
            plotlyOutput(ns("profile_diel"), height = "320px")
          )
        ),
        layout_columns(
          class = "mb-4",
          col_widths = 12,
          card(
            full_screen = TRUE,
            card_header("Where This Species Was Detected"),
            leafletOutput(ns("profile_map"), height = "380px")
          )
        ),
        card(
          full_screen = TRUE,
          card_header("Detection Records"),
          DTOutput(ns("profile_table"), fill = FALSE)
        )
      )
    )
  )
}

mod_camera_trap_server <- function(id, data, active_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # data$species's "method" field is a heuristic used only to drive the
    # MOCK bioacoustic/transect generators (e.g. it tags all birds
    # "acoustic") - it says nothing about what the real camera trap actually
    # recorded, so it must NOT be used to filter which species this module
    # knows about (a bird like Argusianus argus genuinely was camera-trapped
    # despite being tagged "acoustic"). The species universe here is simply
    # whatever data$camera_detections actually contains.
    cam_species <- data$species
    cam_species_detected <- dplyr::filter(cam_species, species_id %in% unique(data$camera_detections$species_id))
    # Fallback icon used only while a species has no image_url set in
    # data$species - once a real photo path/URL is added there, it takes over.
    taxon_icon <- function(taxon) {
      switch(taxon, Mammal = "paw", Bird = "dove", Amphibian = "frog", "paw")
    }

    observe({
      det_range <- range(data$camera_detections$date)
      updateDateRangeInput(session, "date_range",
        start = det_range[1], end = det_range[2],
        min = det_range[1], max = det_range[2])
      updateSelectizeInput(session, "sessions",
        choices = sort(unique(as.character(data$camera_detections$session))), server = TRUE)
      updateSelectizeInput(session, "stations", choices = sort(data$camera_sites$station_id), server = TRUE)
      updateSelectInput(session, "species_profile",
        choices = stats::setNames(cam_species_detected$species_id, cam_species_detected$common_name))
    })

    # Stations in scope per the sidebar "Stations" picker (ignores deployment
    # status - Overall Effort and Species Information count all historical
    # effort, active or retrieved).
    station_pool <- reactive({
      ids <- if (length(input$stations) > 0) input$stations else data$camera_sites$station_id
      dplyr::filter(data$camera_sites, station_id %in% ids)
    })

    # ---- Overall Effort ------------------------------------------------------
    filtered <- reactive({
      req(input$date_range)
      df <- data$camera_detections |>
        dplyr::filter(date >= input$date_range[1], date <= input$date_range[2],
                       station_id %in% station_pool()$station_id)
      if (length(input$sessions) > 0) df <- dplyr::filter(df, as.character(session) %in% input$sessions)
      # cam_species also carries its own "scientific_name" (identical value,
      # since species_id is derived from it either way) - drop it from the
      # join side so the result keeps one unambiguous scientific_name column
      # instead of dplyr suffixing both into scientific_name.x/.y.
      df |> dplyr::left_join(dplyr::select(cam_species, -scientific_name), by = "species_id")
    })

    # Independent camera-trap events (see R/load_camtrap_data.R), same filter
    # scope as filtered() - the correct basis for species-composition and
    # rate-based charts, since a single animal triggering a burst of photos
    # a few seconds apart is one event, not several detections.
    filtered_independent <- reactive({
      req(input$date_range)
      df <- data$camera_independent_events |>
        dplyr::filter(date >= input$date_range[1], date <= input$date_range[2],
                       station_id %in% station_pool()$station_id)
      if (length(input$sessions) > 0) df <- dplyr::filter(df, as.character(session) %in% input$sessions)
      # cam_species also carries its own "scientific_name" (identical value,
      # since species_id is derived from it either way) - drop it from the
      # join side so the result keeps one unambiguous scientific_name column
      # instead of dplyr suffixing both into scientific_name.x/.y.
      df |> dplyr::left_join(dplyr::select(cam_species, -scientific_name), by = "species_id")
    })

    output$kpi_nights <- renderText(scales::comma(sum(station_pool()$trap_nights)))
    output$kpi_detections <- renderText(scales::comma(sum(filtered()$count)))
    # Total monitored stations in scope - same station_pool() the map plots
    # and kpi_nights sums effort over, not just the subset that happened to
    # catch something (n_distinct(filtered()$station_id) undercounts: a
    # station with zero detections still contributed trap-nights and still
    # shows a dot on the map, so it belongs in this count too).
    output$kpi_stations <- renderText(scales::comma(nrow(station_pool())))
    output$kpi_richness <- renderText(scales::comma(dplyr::n_distinct(filtered()$species_id)))

    output$kpi_protected <- renderText({
      scales::comma(dplyr::n_distinct(dplyr::filter(filtered(), conservation_status != "LC")$species_id))
    })
    output$kpi_cr <- renderText({
      scales::comma(dplyr::n_distinct(dplyr::filter(filtered(), conservation_status == "CR")$species_id))
    })
    output$kpi_en <- renderText({
      scales::comma(dplyr::n_distinct(dplyr::filter(filtered(), conservation_status == "EN")$species_id))
    })
    output$kpi_vu <- renderText({
      scales::comma(dplyr::n_distinct(dplyr::filter(filtered(), conservation_status == "VU")$species_id))
    })

    output$plot_species <- renderPlotly({
      df <- filtered_independent()
      req(nrow(df) > 0)
      top <- df |>
        dplyr::count(common_name, name = "events") |>
        dplyr::slice_max(events, n = 10) |>
        dplyr::arrange(events)

      p <- ggplot(top, aes(x = events, y = reorder(common_name, events))) +
        geom_col(fill = "#2c6e49") +
        labs(x = "Independent events", y = NULL) +
        theme_minimal(base_size = 12)
      ggplotly(p, tooltip = c("x", "y"))
    })

    output$map <- renderLeaflet({
      # mapview-based maps (see kphp_grid_base_map() in R/utils.R) have the
      # same hidden-tab race condition as DT tables - their heavier
      # multi-layer init silently never draws if the container was still
      # hidden when the widget was delivered, with no automatic retry.
      # Gating on the navset's own selected-tab input (same fix as DT)
      # guarantees this only (re)renders once its tab is genuinely visible.
      req(active_tab() == "Camera Trap", input$subtab == "Overall Effort")
      counts <- filtered() |> dplyr::count(station_id, name = "detections")
      sites <- station_pool() |>
        dplyr::left_join(counts, by = "station_id") |>
        dplyr::mutate(detections = tidyr::replace_na(detections, 0))

      # Monitoring effort (trap nights), not detections, is what this map
      # encodes - as a heat color (cool = low effort, hot = high effort)
      # rather than point size, so every station stays equally easy to spot
      # regardless of how long it was deployed.
      pal <- leaflet::colorNumeric(palette = "YlOrRd", domain = sites$trap_nights)

      kphp_grid_base_map(data$kphp_boundary, data$monitoring_grid) |>
        addCircleMarkers(
          data = sites, lng = ~longitude, lat = ~latitude,
          radius = 8,
          color = ~pal(trap_nights), fillColor = ~pal(trap_nights), fillOpacity = 0.85, stroke = FALSE,
          popup = ~sprintf("<b>%s</b><br>Status: %s<br>Trap Nights: %s<br>Detections: %d",
                             station_id, status, scales::comma(trap_nights), detections),
          group = "Camera Stations"
        ) |>
        addLegend(
          position = "bottomright", pal = pal, values = sites$trap_nights,
          title = "Trap Nights", opacity = 0.9
        ) |>
        addLayersControl(
          baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
          overlayGroups = c("Camera Stations", "Camera Stations", "KPHP VII"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })

    output$table <- renderDT({
      req(active_tab() == "Camera Trap", input$subtab == "Overall Effort")
      df <- filtered() |>
        dplyr::left_join(dplyr::select(data$camera_sites, station_id, site), by = "station_id") |>
        dplyr::arrange(dplyr::desc(datetime)) |>
        dplyr::mutate(protected_label = ifelse(protected, "Protected", "Not Protected")) |>
        dplyr::select(Site = site, Session = session, Station = station_id,
                       `Scientific Name` = scientific_name, `Common Name` = common_name,
                       DateTime = datetime, `IUCN Status` = conservation_status,
                       Protected = protected_label)
      render_download_dt(df, filter = "top", order = list(list(5, "desc")))
    })

    # ---- Species Information ---------------------------------------------------
    profile_data <- reactive({
      req(input$date_range, input$species_profile)
      df <- data$camera_detections |>
        dplyr::filter(species_id == input$species_profile,
                       date >= input$date_range[1], date <= input$date_range[2],
                       station_id %in% station_pool()$station_id)
      if (length(input$sessions) > 0) df <- dplyr::filter(df, as.character(session) %in% input$sessions)
      df
    })

    # Independent events for the profiled species - basis for RAI (both the
    # bar chart and the map).
    profile_data_independent <- reactive({
      req(input$date_range, input$species_profile)
      df <- data$camera_independent_events |>
        dplyr::filter(species_id == input$species_profile,
                       date >= input$date_range[1], date <= input$date_range[2],
                       station_id %in% station_pool()$station_id)
      if (length(input$sessions) > 0) df <- dplyr::filter(df, as.character(session) %in% input$sessions)
      df
    })

    output$species_card <- renderUI({
      req(input$species_profile)
      status_ref <- dplyr::rename(data$conservation_status_ref,
                                    status_label = label, status_color = color, status_meaning = description)
      sp <- dplyr::filter(cam_species, species_id == input$species_profile) |>
        dplyr::left_join(status_ref, by = c("conservation_status" = "code"))
      req(nrow(sp) == 1)

      has_photo <- !is.na(sp$image_url) && nzchar(sp$image_url)
      has_description <- !is.na(sp$description) && nzchar(sp$description)
      photo_box_style <- paste(
        "width:240px; height:240px; flex-shrink:0; border-radius:10px;",
        "display:flex; flex-direction:column; align-items:center; justify-content:center;",
        "overflow:hidden;"
      )

      div(
        class = "d-flex gap-4 p-3 border rounded-3",
        style = "background: var(--bs-tertiary-bg, #fafafa);",
        if (has_photo) {
          div(
            style = photo_box_style,
            tags$img(src = sp$image_url, alt = sp$common_name,
                       style = "width:100%; height:100%; object-fit:cover;")
          )
        } else {
          div(
            style = paste0(photo_box_style, "border:1px dashed #cbd5e1; color:#9ca3af;",
                            "font-size:13px; text-align:center; padding:8px; gap:10px;"),
            icon(taxon_icon(sp$taxon_group), style = "font-size:64px;"),
            "reference photo", tags$br(), "not available"
          )
        },
        div(
          class = "align-self-center",
          tags$div(sp$common_name, style = "font-size:24px; font-weight:700;"),
          tags$div(sp$scientific_name,
                    style = "font-size:16px; font-style:italic; color:#6b7280; margin-bottom:12px;"),
          tags$div(
            tags$span(sp$taxon_group, class = "badge rounded-pill",
                       style = "background:#eef2ff; color:#4338ca; font-weight:500; margin-right:6px;"),
            tags$span(
              sprintf("%s — %s", sp$conservation_status, sp$status_label),
              class = "badge rounded-pill", title = sp$status_meaning,
              style = sprintf("background:%s22; color:%s; font-weight:600;", sp$status_color, sp$status_color)
            ),
            style = "margin-bottom:8px;"
          ),
          if (has_description) {
            tags$p(sp$description, class = "mb-0", style = "font-size:14px; color:#4b5563; max-width:560px;")
          }
        )
      )
    })

    output$profile_kpi_detections <- renderText(scales::comma(sum(profile_data()$count)))
    output$profile_kpi_stations <- renderText(scales::comma(dplyr::n_distinct(profile_data()$station_id)))
    output$profile_kpi_first <- renderText({
      df <- profile_data()
      req(nrow(df) > 0)
      format(min(df$date), "%d %b %Y")
    })
    output$profile_kpi_peak_hour <- renderText({
      df <- profile_data()
      req(nrow(df) > 0)
      hr <- df |>
        dplyr::mutate(hour = lubridate::hour(datetime)) |>
        dplyr::count(hour, name = "n") |>
        dplyr::slice_max(n, n = 1, with_ties = FALSE) |>
        dplyr::pull(hour)
      sprintf("%02d:00", hr)
    })

    output$profile_rai <- renderPlotly({
      df <- profile_data_independent()
      req(nrow(df) > 0)
      rai <- df |>
        dplyr::group_by(station_id) |>
        dplyr::summarise(events = dplyr::n(), .groups = "drop") |>
        dplyr::left_join(dplyr::select(data$camera_sites, station_id, trap_nights), by = "station_id") |>
        dplyr::mutate(rai = events / trap_nights * 100) |>
        dplyr::arrange(rai)

      p <- ggplot(rai, aes(x = rai, y = reorder(station_id, rai))) +
        geom_col(fill = "#1f77b4") +
        labs(x = "RAI (independent events / 100 trap-nights)", y = NULL) +
        theme_minimal(base_size = 12)
      ggplotly(p, tooltip = c("x", "y"))
    })

    output$profile_diel <- renderPlotly({
      df <- profile_data()
      req(nrow(df) > 0)
      hourly <- df |>
        dplyr::mutate(hour = lubridate::hour(datetime)) |>
        dplyr::count(hour, name = "detections")

      p <- ggplot(hourly, aes(x = hour, y = detections)) +
        geom_col(fill = "#ff7f0e") +
        scale_x_continuous(breaks = seq(0, 23, 4)) +
        labs(x = "Hour of day", y = "Detections") +
        theme_minimal(base_size = 12)
      ggplotly(p)
    })

    output$profile_map <- renderLeaflet({
      req(active_tab() == "Camera Trap", input$subtab == "Species Information")
      counts <- profile_data_independent() |> dplyr::count(station_id, name = "events")
      sites <- station_pool() |>
        dplyr::left_join(counts, by = "station_id") |>
        dplyr::mutate(
          events = tidyr::replace_na(events, 0),
          rai = events / trap_nights * 100
        )

      # RAI (not raw event count) drives color here, same heat-scale
      # convention as the Overall Effort map's trap-nights encoding - every
      # station stays the same size regardless of RAI.
      pal <- leaflet::colorNumeric(palette = "YlOrRd", domain = sites$rai)

      kphp_grid_base_map(data$kphp_boundary, data$monitoring_grid) |>
        addCircleMarkers(
          data = sites, lng = ~longitude, lat = ~latitude, radius = 8,
          color = ~pal(rai), fillColor = ~pal(rai), fillOpacity = 0.85, stroke = FALSE,
          popup = ~sprintf("<b>%s</b><br>RAI: %.1f<br>Independent events: %d", station_id, rai, events),
          group = "Camera Stations"
        ) |>
        addLegend(
          position = "bottomright", pal = pal, values = sites$rai,
          title = "RAI", opacity = 0.9
        ) |>
        addLayersControl(
          baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
          overlayGroups = c("KPHP VII", "Grid", "Camera Stations"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })

    output$profile_table <- renderDT({
      req(active_tab() == "Camera Trap", input$subtab == "Species Information")
      df <- profile_data() |>
        dplyr::arrange(dplyr::desc(datetime)) |>
        dplyr::select(Station = station_id, DateTime = datetime, Count = count)
      render_download_dt(df, filter = "top", order = list(list(1, "desc")))
    })

    unsuspend_all(output, c(
      "kpi_nights", "kpi_detections", "kpi_stations", "kpi_richness",
      "kpi_protected", "kpi_cr", "kpi_en", "kpi_vu",
      "plot_species", "map", "table", "species_card",
      "profile_kpi_detections", "profile_kpi_stations", "profile_kpi_first",
      "profile_kpi_peak_hour", "profile_rai", "profile_diel",
      "profile_map", "profile_table"
    ))
  })
}
