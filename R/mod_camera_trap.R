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
      selectizeInput(ns("stations"), "Stations", choices = NULL, multiple = TRUE,
                      options = list(placeholder = "All stations")),
      selectizeInput(ns("species"), "Species", choices = NULL, multiple = TRUE,
                      options = list(placeholder = "All species (Overall Effort tab)"))
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
            card_header("Top Detected Species"),
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
            card_header("RAI by Station (detections / 100 trap-nights)"),
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

mod_camera_trap_server <- function(id, data) {
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
      updateSelectizeInput(session, "stations", choices = sort(data$camera_sites$station_id), server = TRUE)
      updateSelectizeInput(session, "species",
        choices = stats::setNames(cam_species_detected$species_id, cam_species_detected$common_name), server = TRUE)
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
      if (length(input$species) > 0) df <- dplyr::filter(df, species_id %in% input$species)
      df |> dplyr::left_join(cam_species, by = "species_id")
    })

    output$kpi_nights <- renderText(scales::comma(sum(station_pool()$trap_nights)))
    output$kpi_detections <- renderText(scales::comma(sum(filtered()$count)))
    output$kpi_stations <- renderText(scales::comma(dplyr::n_distinct(filtered()$station_id)))
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
      df <- filtered()
      req(nrow(df) > 0)
      top <- df |>
        dplyr::group_by(common_name) |>
        dplyr::summarise(detections = sum(count), .groups = "drop") |>
        dplyr::slice_max(detections, n = 10) |>
        dplyr::arrange(detections)

      p <- ggplot(top, aes(x = detections, y = reorder(common_name, detections))) +
        geom_col(fill = "#2c6e49") +
        labs(x = "Detections", y = NULL) +
        theme_minimal(base_size = 12)
      ggplotly(p, tooltip = c("x", "y"))
    })

    output$map <- renderLeaflet({
      counts <- filtered() |> dplyr::count(station_id, name = "detections")
      sites <- station_pool() |>
        dplyr::left_join(counts, by = "station_id") |>
        dplyr::mutate(detections = tidyr::replace_na(detections, 0))

      leaflet(sites) |>
        addProviderTiles(providers$OpenStreetMap) |>
        addCircleMarkers(
          lng = ~longitude, lat = ~latitude,
          radius = ~pmax(5, sqrt(detections) * 2),
          color = "#1f77b4", fillOpacity = 0.75, stroke = FALSE,
          popup = ~sprintf("<b>%s</b><br>Status: %s<br>Trap Nights: %s<br>Detections: %d",
                             station_id, status, scales::comma(trap_nights), detections)
        ) |>
        add_kphp_boundary(data$kphp_boundary)
    })

    output$table <- renderDT({
      req(input$subtab == "Overall Effort")
      df <- filtered() |>
        dplyr::arrange(dplyr::desc(datetime)) |>
        dplyr::select(Station = station_id, Species = common_name, Group = taxon_group,
                       Status = conservation_status, DateTime = datetime, Count = count)
      render_download_dt(df, filter = "top", order = list(list(4, "desc")))
    })

    # ---- Species Information ---------------------------------------------------
    profile_data <- reactive({
      req(input$date_range, input$species_profile)
      data$camera_detections |>
        dplyr::filter(species_id == input$species_profile,
                       date >= input$date_range[1], date <= input$date_range[2],
                       station_id %in% station_pool()$station_id)
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
      df <- profile_data()
      req(nrow(df) > 0)
      rai <- df |>
        dplyr::group_by(station_id) |>
        dplyr::summarise(detections = sum(count), .groups = "drop") |>
        dplyr::left_join(dplyr::select(data$camera_sites, station_id, trap_nights), by = "station_id") |>
        dplyr::mutate(rai = detections / trap_nights * 100) |>
        dplyr::arrange(rai)

      p <- ggplot(rai, aes(x = rai, y = reorder(station_id, rai))) +
        geom_col(fill = "#1f77b4") +
        labs(x = "RAI (detections / 100 trap-nights)", y = NULL) +
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
      counts <- profile_data() |> dplyr::count(station_id, name = "detections")
      sites <- station_pool() |>
        dplyr::inner_join(counts, by = "station_id")

      m <- leaflet(station_pool()) |>
        addProviderTiles(providers$OpenStreetMap) |>
        addCircleMarkers(
          lng = ~longitude, lat = ~latitude, radius = 4,
          color = "#c7c7c7", fillOpacity = 0.5, stroke = FALSE
        )

      if (nrow(sites) > 0) {
        m <- m |>
          addCircleMarkers(
            data = sites, lng = ~longitude, lat = ~latitude,
            radius = ~pmax(6, sqrt(detections) * 2),
            color = "#2c6e49", fillOpacity = 0.85, stroke = FALSE,
            popup = ~sprintf("<b>%s</b><br>Detections: %d", station_id, detections)
          )
      }
      m |> add_kphp_boundary(data$kphp_boundary)
    })

    output$profile_table <- renderDT({
      req(input$subtab == "Species Information")
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
