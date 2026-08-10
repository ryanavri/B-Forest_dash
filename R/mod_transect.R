# ---- Transect-based Survey detail tab ----------------------------------------
# Three sub-tabs, mirroring Camera Trap/Bioacoustics:
#  - "Overall Effort": park-wide transect survey dashboard (distance
#    surveyed, detections, route coverage, species richness).
#  - "Species Information": single-species profile (ID card, encounter
#    rate by transect, detections by time of day, elevation response,
#    map/records).
#  - "Active Transect": survey-route rotation status (active vs.
#    discontinued transects, roster, status map).
#
# Transects are walked repeatedly across survey sessions rather than
# continuously deployed like a camera/recorder, so effort is measured in
# distance surveyed (km) per walk rather than trap-nights/recording-hours,
# and the natural effort-normalized metric is encounter rate (detections
# per km) rather than RAI/detection-rate-per-hour.

mod_transect_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      title = "Filters",
      width = 280,
      dateRangeInput(ns("date_range"), "Date range", start = NULL, end = NULL),
      selectizeInput(ns("sessions"), "Session", choices = NULL, multiple = TRUE,
                      options = list(placeholder = "All sessions")),
      selectizeInput(ns("transects"), "Transects", choices = NULL, multiple = TRUE,
                      options = list(placeholder = "All transects")),
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
          value_box(title = "Total Distance Surveyed", value = textOutput(ns("kpi_distance")),
                     p("km", class = "small text-muted mb-0"),
                     showcase = icon("route"), theme = "secondary"),
          value_box(title = "Total Detections", value = textOutput(ns("kpi_detections")),
                     showcase = icon("binoculars"), theme = "success"),
          value_box(title = "Total Unique Transects", value = textOutput(ns("kpi_transects")),
                     showcase = icon("road"), theme = "primary"),
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
            card_header("Transect Route Map"),
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
                     showcase = icon("binoculars"), theme = "success"),
          value_box(title = "Unique Transects", value = textOutput(ns("profile_kpi_transects")),
                     showcase = icon("road"), theme = "primary"),
          value_box(title = "First Detection", value = textOutput(ns("profile_kpi_first")),
                     showcase = icon("calendar-day"), theme = "secondary"),
          value_box(title = "Peak Detection Hour", value = textOutput(ns("profile_kpi_peak_hour")),
                     showcase = icon("clock"), theme = "info")
        ),
        layout_columns(
          class = "mb-4",
          col_widths = c(6, 6),
          card(
            full_screen = TRUE,
            card_header("Encounter Rate by Transect (detections / km)"),
            plotlyOutput(ns("profile_rate"), height = "320px")
          ),
          card(
            full_screen = TRUE,
            card_header("Detections by Time of Day"),
            plotlyOutput(ns("profile_diel"), height = "320px")
          )
        ),
        layout_columns(
          class = "mb-4",
          col_widths = 12,
          card(
            full_screen = TRUE,
            card_header("Species Response to Elevation"),
            plotlyOutput(ns("profile_elevation"), height = "300px")
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
      ),
      nav_panel(
        "Active Transect",
        icon = icon("road"),
        layout_columns(
          class = "mb-4 mt-3",
          col_widths = 12,
          checkboxGroupInput(ns("route_status"), "Survey rotation status",
                              choices = c("Active", "Retrieved"),
                              selected = c("Active", "Retrieved"), inline = TRUE)
        ),
        layout_columns(
          class = "mb-4",
          col_widths = c(3, 3, 3, 3),
          value_box(
            title = "Active Transects",
            value = textOutput(ns("kpi_transects_active")),
            p(textOutput(ns("kpi_transects_active_sub")), class = "small text-muted mb-0"),
            showcase = icon("road"), theme = "success"
          ),
          value_box(title = "Discontinued Transects", value = textOutput(ns("kpi_transects_retrieved")),
                     showcase = icon("box-archive"), theme = "secondary"),
          value_box(title = "Avg. Length / Transect", value = textOutput(ns("kpi_avg_length")),
                     p("km", class = "small text-muted mb-0"),
                     showcase = icon("ruler"), theme = "primary"),
          value_box(title = "Newest Route Established", value = textOutput(ns("kpi_newest_established")),
                     showcase = icon("calendar-plus"), theme = "info")
        ),
        layout_columns(
          class = "mb-4",
          col_widths = 12,
          card(
            full_screen = TRUE,
            card_header("Survey Rotation Status Map"),
            leafletOutput(ns("status_map"), height = "420px")
          )
        ),
        card(
          full_screen = TRUE,
          card_header("Transect Roster"),
          DTOutput(ns("roster"), fill = FALSE)
        )
      )
    )
  )
}

mod_transect_server <- function(id, data, active_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tx_species <- dplyr::filter(data$species, method %in% c("camera", "both"))
    status_colors <- c("Active" = "#2ca02c", "Retrieved" = "#7f7f7f")
    taxon_icon <- function(taxon) {
      switch(taxon, Mammal = "paw", Bird = "dove", Amphibian = "frog", "paw")
    }

    observe({
      updateDateRangeInput(session, "date_range",
        start = data$study_start, end = data$today,
        min = data$study_start, max = data$today)
      updateSelectizeInput(session, "sessions",
        choices = stats::setNames(data$sessions$session_id, data$sessions$label), server = TRUE)
      updateSelectizeInput(session, "transects",
        choices = stats::setNames(data$transect_routes$transect_id, data$transect_routes$transect_name),
        server = TRUE)
      updateSelectizeInput(session, "species",
        choices = stats::setNames(tx_species$species_id, tx_species$common_name), server = TRUE)
      updateSelectInput(session, "species_profile",
        choices = stats::setNames(tx_species$species_id, tx_species$common_name))
    })

    # Transects in scope per the sidebar "Transects" picker (ignores rotation
    # status - Overall Effort and Species Information count all historical
    # effort, active or discontinued).
    transect_pool <- reactive({
      ids <- if (length(input$transects) > 0) input$transects else data$transect_routes$transect_id
      dplyr::filter(data$transect_routes, transect_id %in% ids)
    })

    # ---- Overall Effort ------------------------------------------------------
    surveys_f <- reactive({
      req(input$date_range)
      df <- data$transect_surveys |>
        dplyr::filter(survey_date >= input$date_range[1], survey_date <= input$date_range[2],
                       transect_id %in% transect_pool()$transect_id)
      if (length(input$sessions) > 0) df <- dplyr::filter(df, session_id %in% input$sessions)
      df
    })

    filtered <- reactive({
      req(input$date_range)
      df <- data$transect_observations |>
        dplyr::filter(as.Date(datetime) >= input$date_range[1],
                       as.Date(datetime) <= input$date_range[2],
                       transect_id %in% transect_pool()$transect_id)
      if (length(input$sessions) > 0) df <- dplyr::filter(df, session_id %in% input$sessions)
      if (length(input$species) > 0) df <- dplyr::filter(df, species_id %in% input$species)
      df |> dplyr::left_join(tx_species, by = "species_id")
    })

    output$kpi_distance <- renderText(scales::comma(round(sum(surveys_f()$effort_km), 1)))
    output$kpi_detections <- renderText(scales::comma(sum(filtered()$count)))
    output$kpi_transects <- renderText(scales::comma(dplyr::n_distinct(filtered()$transect_id)))
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
      counts <- filtered() |> dplyr::count(transect_id, name = "detections")
      routes <- transect_pool() |>
        dplyr::left_join(counts, by = "transect_id") |>
        dplyr::mutate(detections = tidyr::replace_na(detections, 0))

      m <- leaflet() |> addProviderTiles(providers$OpenStreetMap)
      if (nrow(routes) > 0) {
        pal <- leaflet::colorNumeric("Blues", domain = c(0, max(routes$detections, 1)))
        for (i in seq_len(nrow(routes))) {
          r <- routes[i, ]
          m <- m |> addPolylines(
            lng = c(r$lon, r$end_lon), lat = c(r$lat, r$end_lat),
            color = pal(r$detections), weight = 4, opacity = 0.85,
            popup = sprintf("<b>%s</b><br>Habitat: %s<br>Length: %.1f km<br>Detections: %d",
                             r$transect_name, r$habitat, r$length_km, r$detections)
          )
        }
        m <- m |> addLegend(position = "bottomright", pal = pal, values = routes$detections,
                              title = "Detections")
      }
      m |> add_kphp_boundary(data$kphp_boundary)
    })

    output$table <- renderDT({
      req(active_tab() == "Transect-based Survey", input$subtab == "Overall Effort")
      df <- filtered() |>
        dplyr::left_join(dplyr::select(data$transect_routes, transect_id, transect_name), by = "transect_id") |>
        dplyr::arrange(dplyr::desc(datetime)) |>
        dplyr::select(Transect = transect_name, Species = common_name, Group = taxon_group,
                       Status = conservation_status, DateTime = datetime, Count = count,
                       `Perp. Distance (m)` = perpendicular_distance_m, Session = session_id)
      render_download_dt(df, filter = "top", order = list(list(4, "desc")))
    })

    # ---- Species Information ---------------------------------------------------
    profile_data <- reactive({
      req(input$date_range, input$species_profile)
      df <- data$transect_observations |>
        dplyr::filter(species_id == input$species_profile,
                       as.Date(datetime) >= input$date_range[1],
                       as.Date(datetime) <= input$date_range[2],
                       transect_id %in% transect_pool()$transect_id)
      if (length(input$sessions) > 0) df <- dplyr::filter(df, session_id %in% input$sessions)
      df
    })

    output$species_card <- renderUI({
      req(input$species_profile)
      status_ref <- dplyr::rename(data$conservation_status_ref,
                                    status_label = label, status_color = color, status_meaning = description)
      sp <- dplyr::filter(tx_species, species_id == input$species_profile) |>
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
    output$profile_kpi_transects <- renderText(scales::comma(dplyr::n_distinct(profile_data()$transect_id)))
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
      dist_by_transect <- surveys_f() |>
        dplyr::group_by(transect_id) |>
        dplyr::summarise(km = sum(effort_km), .groups = "drop")

      rate <- df |>
        dplyr::count(transect_id, name = "detections") |>
        dplyr::left_join(dist_by_transect, by = "transect_id") |>
        dplyr::mutate(rate = detections / km) |>
        dplyr::filter(!is.na(rate), is.finite(rate)) |>
        dplyr::arrange(rate)
      req(nrow(rate) > 0)

      p <- ggplot(rate, aes(x = rate, y = reorder(transect_id, rate))) +
        geom_col(fill = "#2c6e49") +
        labs(x = "Detections / km", y = NULL) +
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
        scale_x_continuous(limits = c(5, 18), breaks = seq(6, 17, 2)) +
        labs(x = "Hour of day", y = "Detections") +
        theme_minimal(base_size = 12)
      ggplotly(p)
    })

    output$profile_elevation <- renderPlotly({
      df <- profile_data()
      req(nrow(df) > 0)
      by_transect <- df |>
        dplyr::count(transect_id, name = "detections") |>
        dplyr::left_join(dplyr::select(data$transect_routes, transect_id, elevation_m), by = "transect_id")
      req(nrow(by_transect) > 0)

      p <- ggplot(by_transect, aes(x = elevation_m, y = detections, text = transect_id)) +
        geom_point(color = "#2c6e49", size = 3, alpha = 0.8)

      if (nrow(by_transect) >= 4) {
        p <- p + geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                              color = "#1f77b4", linewidth = 0.7)
      }

      p <- p +
        labs(x = "Elevation (m)", y = "Detections") +
        theme_minimal(base_size = 12)
      ggplotly(p, tooltip = c("x", "y", "text"))
    })

    output$profile_map <- renderLeaflet({
      counts <- profile_data() |> dplyr::count(transect_id, name = "detections")
      routes <- transect_pool() |> dplyr::left_join(counts, by = "transect_id")

      m <- leaflet() |> addProviderTiles(providers$OpenStreetMap)
      pool <- transect_pool()
      for (i in seq_len(nrow(pool))) {
        r <- pool[i, ]
        m <- m |> addPolylines(lng = c(r$lon, r$end_lon), lat = c(r$lat, r$end_lat),
                                 color = "#c7c7c7", weight = 3, opacity = 0.6)
      }
      detected <- dplyr::filter(routes, !is.na(detections), detections > 0)
      if (nrow(detected) > 0) {
        for (i in seq_len(nrow(detected))) {
          r <- detected[i, ]
          m <- m |> addPolylines(
            lng = c(r$lon, r$end_lon), lat = c(r$lat, r$end_lat),
            color = "#2c6e49", weight = 5, opacity = 0.9,
            popup = sprintf("<b>%s</b><br>Detections: %d", r$transect_name, r$detections)
          )
        }
      }
      m |> add_kphp_boundary(data$kphp_boundary)
    })

    output$profile_table <- renderDT({
      req(active_tab() == "Transect-based Survey", input$subtab == "Species Information")
      df <- profile_data() |>
        dplyr::left_join(dplyr::select(data$transect_routes, transect_id, transect_name), by = "transect_id") |>
        dplyr::arrange(dplyr::desc(datetime)) |>
        dplyr::select(Transect = transect_name, DateTime = datetime, Count = count,
                       `Perp. Distance (m)` = perpendicular_distance_m, Session = session_id)
      render_download_dt(df, filter = "top", order = list(list(1, "desc")))
    })

    # ---- Active Transect ---------------------------------------------------------
    active_view <- reactive({
      req(length(input$route_status) > 0)
      dplyr::filter(transect_pool(), status %in% input$route_status)
    })

    output$kpi_transects_active <- renderText(scales::comma(sum(transect_pool()$status == "Active")))
    output$kpi_transects_active_sub <- renderText({
      sprintf("of %s total", scales::comma(nrow(transect_pool())))
    })
    output$kpi_transects_retrieved <- renderText(scales::comma(sum(transect_pool()$status == "Retrieved")))
    output$kpi_avg_length <- renderText({
      df <- active_view()
      req(nrow(df) > 0)
      scales::comma(round(mean(df$length_km), 1))
    })
    output$kpi_newest_established <- renderText({
      df <- transect_pool()
      req(nrow(df) > 0)
      format(max(df$established_date), "%d %b %Y")
    })

    output$status_map <- renderLeaflet({
      sites <- active_view()
      m <- leaflet() |> addProviderTiles(providers$OpenStreetMap)

      if (nrow(sites) > 0) {
        for (i in seq_len(nrow(sites))) {
          r <- sites[i, ]
          m <- m |> addPolylines(
            lng = c(r$lon, r$end_lon), lat = c(r$lat, r$end_lat),
            color = status_colors[[r$status]], weight = 5, opacity = 0.9,
            popup = sprintf("<b>%s</b><br>Habitat: %s<br>Length: %.1f km<br>Status: %s<br>Last surveyed: %s",
                             r$transect_name, r$habitat, r$length_km, r$status,
                             format(r$last_survey_date, "%d %b %Y"))
          )
        }
        pal <- leaflet::colorFactor(status_colors[names(status_colors) %in% sites$status],
                                      domain = names(status_colors)[names(status_colors) %in% sites$status])
        m <- m |> addLegend(position = "bottomright", pal = pal, values = sites$status,
                              title = "Rotation status", opacity = 0.9)
      }
      m |> add_kphp_boundary(data$kphp_boundary)
    })

    output$roster <- renderDT({
      req(active_tab() == "Transect-based Survey", input$subtab == "Active Transect")
      df <- active_view() |>
        dplyr::mutate(
          Established = format(established_date, "%d %b %Y"),
          `Last Surveyed` = format(last_survey_date, "%d %b %Y")
        ) |>
        dplyr::arrange(dplyr::desc(status), transect_id) |>
        dplyr::select(Transect = transect_name, Habitat = habitat, `Length (km)` = length_km,
                       Established, `Last Surveyed`, Status = status)
      render_download_dt(df, dom = "Btip") |>
        formatStyle("Status",
          backgroundColor = styleEqual(c("Active", "Retrieved"), c("#e6f4ea", "#ececec")),
          fontWeight = "bold")
    })

    unsuspend_all(output, c(
      "kpi_distance", "kpi_detections", "kpi_transects", "kpi_richness",
      "kpi_protected", "kpi_cr", "kpi_en", "kpi_vu",
      "plot_species", "map", "table", "species_card",
      "profile_kpi_detections", "profile_kpi_transects", "profile_kpi_first",
      "profile_kpi_peak_hour", "profile_rate", "profile_diel", "profile_elevation",
      "profile_map", "profile_table",
      "kpi_transects_active", "kpi_transects_active_sub", "kpi_transects_retrieved",
      "kpi_avg_length", "kpi_newest_established", "status_map", "roster"
    ))
  })
}
