# ---- Bioacoustics detail tab -------------------------------------------------
# Runs on real PAM (passive acoustic monitoring) data (data/pam_data.RData) -
# see R/load_pam_data.R for the pam_detection/pam_effort -> app schema
# translation. Two sub-tabs, mirroring Camera Trap:
#  - "Overall Effort": park-wide bioacoustic dashboard (recording hours,
#    detections, station coverage, species richness).
#  - "Species Information": single-species profile (ID card, detection
#    rate by station, diel calling pattern, map/records). The real recorder
#    deployment log has no elevation data, so (unlike the old mock-data
#    version) there's no "Species Response to Elevation" chart here.

mod_bioacoustics_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      title = "Filters",
      width = 280,
      dateRangeInput(ns("date_range"), "Date range", start = NULL, end = NULL),
      selectizeInput(ns("recorders"), "Recorders", choices = NULL, multiple = TRUE,
                      options = list(placeholder = "All recorders")),
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
          value_box(title = "Total Recording Hours", value = textOutput(ns("kpi_hours")),
                     showcase = icon("clock"), theme = "secondary"),
          value_box(title = "Total Detections", value = textOutput(ns("kpi_detections")),
                     showcase = icon("volume-up"), theme = "success"),
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
            card_header("Acoustic Recorder Map"),
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
                     showcase = icon("volume-up"), theme = "success"),
          value_box(title = "Unique Stations", value = textOutput(ns("profile_kpi_stations")),
                     showcase = icon("tower-broadcast"), theme = "primary"),
          value_box(title = "First Detection", value = textOutput(ns("profile_kpi_first")),
                     showcase = icon("calendar-day"), theme = "secondary"),
          value_box(title = "Peak Calling Hour", value = textOutput(ns("profile_kpi_peak_hour")),
                     showcase = icon("clock"), theme = "info")
        ),
        layout_columns(
          class = "mb-4",
          col_widths = c(6, 6),
          card(
            full_screen = TRUE,
            card_header("Detection Rate by Station (detections / 100 recording-hours)"),
            plotlyOutput(ns("profile_rate"), height = "320px")
          ),
          card(
            full_screen = TRUE,
            card_header("Diel Calling Pattern"),
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

mod_bioacoustics_server <- function(id, data, active_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # data$species's "method" field is a heuristic used only to drive the
    # MOCK generators elsewhere (e.g. it tags all birds "acoustic") - it says
    # nothing about what the real PAM survey actually recorded, so it must
    # NOT be used to filter which species this module knows about. The
    # species universe here is simply whatever data$acoustic_detections
    # actually contains.
    ac_species <- data$species
    ac_species_detected <- dplyr::filter(ac_species, species_id %in% unique(data$acoustic_detections$species_id))
    taxon_icon <- function(taxon) {
      switch(taxon, Mammal = "paw", Bird = "dove", Amphibian = "frog", "paw")
    }

    observe({
      det_range <- range(as.Date(data$acoustic_detections$datetime))
      updateDateRangeInput(session, "date_range",
        start = det_range[1], end = det_range[2],
        min = det_range[1], max = det_range[2])
      updateSelectizeInput(session, "recorders", choices = sort(data$acoustic_sites$recorder_id), server = TRUE)
      updateSelectizeInput(session, "species",
        choices = stats::setNames(ac_species_detected$species_id, ac_species_detected$common_name), server = TRUE)
      updateSelectInput(session, "species_profile",
        choices = stats::setNames(ac_species_detected$species_id, ac_species_detected$common_name))
    })

    # Recorders in scope per the sidebar "Recorders" picker (ignores
    # deployment status - Overall Effort and Species Information count all
    # historical effort, active or retrieved).
    station_pool <- reactive({
      ids <- if (length(input$recorders) > 0) input$recorders else data$acoustic_sites$recorder_id
      dplyr::filter(data$acoustic_sites, recorder_id %in% ids)
    })

    # ---- Overall Effort ------------------------------------------------------
    filtered <- reactive({
      req(input$date_range)
      df <- data$acoustic_detections |>
        dplyr::filter(as.Date(datetime) >= input$date_range[1],
                       as.Date(datetime) <= input$date_range[2],
                       recorder_id %in% station_pool()$recorder_id)
      if (length(input$species) > 0) df <- dplyr::filter(df, species_id %in% input$species)
      # ac_species also carries its own "scientific_name" (identical value,
      # since species_id is derived from it either way) - drop it from the
      # join side so the result keeps one unambiguous scientific_name column
      # instead of dplyr suffixing both into scientific_name.x/.y.
      df |> dplyr::left_join(dplyr::select(ac_species, -scientific_name), by = "species_id")
    })

    output$kpi_hours <- renderText(scales::comma(sum(station_pool()$recording_hours)))
    output$kpi_detections <- renderText(scales::comma(nrow(filtered())))
    # Total monitored recorders in scope - same station_pool() the map plots
    # and kpi_hours sums effort over, not just the subset that happened to
    # record something (same fix as Camera Trap's kpi_stations).
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
      df <- filtered()
      req(nrow(df) > 0)
      top <- df |>
        dplyr::count(common_name, name = "detections") |>
        dplyr::slice_max(detections, n = 10) |>
        dplyr::arrange(detections)

      p <- ggplot(top, aes(x = detections, y = reorder(common_name, detections))) +
        geom_col(fill = "#2c6e49") +
        labs(x = "Detections", y = NULL) +
        theme_minimal(base_size = 12)
      ggplotly(p, tooltip = c("x", "y"))
    })

    output$map <- renderLeaflet({
      # mapview-based maps and DT tables need BOTH the outer top-level nav
      # tab AND this module's own inner subtab to be selected - see the
      # note by unsuspend_all() at the end of this function for why.
      req(active_tab() == "Bioacoustics", input$subtab == "Overall Effort")
      counts <- filtered() |> dplyr::count(recorder_id, name = "detections")
      sites <- station_pool() |>
        dplyr::left_join(counts, by = "recorder_id") |>
        dplyr::mutate(detections = tidyr::replace_na(detections, 0))

      # Monitoring effort (recording hours), not detections, is what this
      # map encodes - as a heat color rather than point size, mirroring
      # Camera Trap's Camera Station Map.
      pal <- leaflet::colorNumeric(palette = "YlOrRd", domain = sites$recording_hours)

      kphp_grid_base_map(data$kphp_boundary, data$monitoring_grid) |>
        addCircleMarkers(
          data = sites, lng = ~lon, lat = ~lat, radius = 8,
          color = ~pal(recording_hours), fillColor = ~pal(recording_hours), fillOpacity = 0.85, stroke = FALSE,
          popup = ~sprintf("<b>%s</b><br>Site: %s<br>Unit: %s<br>Recording Hours: %s<br>Detections: %d",
                             recorder_id, site, unit, scales::comma(recording_hours), detections),
          group = "Acoustic Recorders"
        ) |>
        addLegend(
          position = "bottomright", pal = pal, values = sites$recording_hours,
          title = "Recording Hours", opacity = 0.9
        ) |>
        addLayersControl(
          baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
          overlayGroups = c("KPHP VII", "Grid", "Acoustic Recorders"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })

    output$table <- renderDT({
      req(active_tab() == "Bioacoustics", input$subtab == "Overall Effort")
      df <- filtered() |>
        dplyr::left_join(dplyr::select(data$acoustic_sites, recorder_id, site), by = "recorder_id") |>
        dplyr::arrange(dplyr::desc(datetime)) |>
        dplyr::mutate(protected_label = ifelse(protected, "Protected", "Not Protected")) |>
        dplyr::select(Site = site, Session = session, Station = recorder_id,
                       `Scientific Name` = scientific_name, `Common Name` = common_name,
                       DateTime = datetime, `IUCN Status` = conservation_status,
                       Protected = protected_label)
      render_download_dt(df, filter = "top", order = list(list(5, "desc")))
    })

    # ---- Species Information ---------------------------------------------------
    profile_data <- reactive({
      req(input$date_range, input$species_profile)
      data$acoustic_detections |>
        dplyr::filter(species_id == input$species_profile,
                       as.Date(datetime) >= input$date_range[1],
                       as.Date(datetime) <= input$date_range[2],
                       recorder_id %in% station_pool()$recorder_id)
    })

    output$species_card <- renderUI({
      req(input$species_profile)
      status_ref <- dplyr::rename(data$conservation_status_ref,
                                    status_label = label, status_color = color, status_meaning = description)
      sp <- dplyr::filter(ac_species, species_id == input$species_profile) |>
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

    output$profile_kpi_detections <- renderText(scales::comma(nrow(profile_data())))
    output$profile_kpi_stations <- renderText(scales::comma(dplyr::n_distinct(profile_data()$recorder_id)))
    output$profile_kpi_first <- renderText({
      df <- profile_data()
      req(nrow(df) > 0)
      format(min(as.Date(df$datetime)), "%d %b %Y")
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

    output$profile_rate <- renderPlotly({
      df <- profile_data()
      req(nrow(df) > 0)
      rate <- df |>
        dplyr::count(recorder_id, name = "detections") |>
        dplyr::left_join(dplyr::select(data$acoustic_sites, recorder_id, recording_hours), by = "recorder_id") |>
        dplyr::mutate(rate = detections / recording_hours * 100) |>
        dplyr::arrange(rate)

      p <- ggplot(rate, aes(x = rate, y = reorder(recorder_id, rate))) +
        geom_col(fill = "#2ca02c") +
        labs(x = "Detections / 100 recording-hours", y = NULL) +
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
        geom_col(fill = "#9467bd") +
        scale_x_continuous(breaks = seq(0, 23, 4)) +
        labs(x = "Hour of day", y = "Detections") +
        theme_minimal(base_size = 12)
      ggplotly(p)
    })

    output$profile_map <- renderLeaflet({
      req(active_tab() == "Bioacoustics", input$subtab == "Species Information")
      counts <- profile_data() |> dplyr::count(recorder_id, name = "detections")
      sites <- station_pool() |>
        dplyr::left_join(counts, by = "recorder_id") |>
        dplyr::mutate(
          detections = tidyr::replace_na(detections, 0),
          rate = detections / recording_hours * 100
        )

      # Detection rate (detections / 100 recording-hours), not raw detection
      # count, drives color here - the bioacoustic equivalent of Camera
      # Trap's RAI map. Every recorder stays the same size regardless of rate.
      pal <- leaflet::colorNumeric(palette = "YlOrRd", domain = sites$rate)

      kphp_grid_base_map(data$kphp_boundary, data$monitoring_grid) |>
        addCircleMarkers(
          data = sites, lng = ~lon, lat = ~lat, radius = 8,
          color = ~pal(rate), fillColor = ~pal(rate), fillOpacity = 0.85, stroke = FALSE,
          popup = ~sprintf("<b>%s</b><br>Rate: %.1f<br>Detections: %d", recorder_id, rate, detections),
          group = "Acoustic Recorders"
        ) |>
        addLegend(
          position = "bottomright", pal = pal, values = sites$rate,
          title = "Detections / 100h", opacity = 0.9
        ) |>
        addLayersControl(
          baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
          overlayGroups = c("KPHP VII", "Grid", "Acoustic Recorders"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })

    output$profile_table <- renderDT({
      req(active_tab() == "Bioacoustics", input$subtab == "Species Information")
      df <- profile_data() |>
        dplyr::arrange(dplyr::desc(datetime)) |>
        dplyr::select(Recorder = recorder_id, DateTime = datetime, Confidence = confidence)
      render_download_dt(df, filter = "top", order = list(list(1, "desc")))
    })

    # DT tables and mapview-based maps (output$map/table/profile_map/
    # profile_table) additionally gate on `active_tab()` - the *outer*
    # nav_menu tab (e.g. "Bioacoustics") - on top of `input$subtab` (the
    # *inner* Overall Effort/Species Information selection). Both are
    # needed: with suspendWhenHidden disabled, these heavier widgets get
    # eagerly computed and delivered before the user ever visits their tab,
    # and if the OUTER tab was still hidden at that moment they silently
    # never draw (no error, no retry) even though the inner subtab
    # "looked" selected the whole time. See [[biodash-shiny-gotchas]] memory.
    unsuspend_all(output, c(
      "kpi_hours", "kpi_detections", "kpi_stations", "kpi_richness",
      "kpi_protected", "kpi_cr", "kpi_en", "kpi_vu",
      "plot_species", "map", "table", "species_card",
      "profile_kpi_detections", "profile_kpi_stations", "profile_kpi_first",
      "profile_kpi_peak_hour", "profile_rate", "profile_diel",
      "profile_map", "profile_table"
    ))
  })
}
