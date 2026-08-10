# ---- SMART Patrol detail tab -------------------------------------------------
# Runs on real SMART export data (Kerinci-Seblat landscape, Jambi/Sumatra) -
# see R/load_smart_data.R for how data/smart_data.RData's CRP (patrol GPS
# tracks) and CAP (field observations) are translated into patrol_tracks/
# patrols/patrol_observations. Three sub-tabs:
#  - "Overview": patrol effort (patrols, distance, patrol-days), the full
#    observation-category breakdown, real GPS patrol tracks on the map, and
#    the patrol log.
#  - "Wildlife": just the Wildlife-category observations - species-level
#    encounters/signs recorded incidentally during patrols, with curated
#    IUCN status.
#  - "Human Activities": Human Activity-category findings (illegal logging,
#    encroachment, mining, poaching, etc.).

mod_smart_patrol_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      title = "Filters",
      width = 280,
      dateRangeInput(ns("date_range"), "Date range", start = NULL, end = NULL),
      selectizeInput(ns("teams"), "Patrol teams", choices = NULL, multiple = TRUE,
                      options = list(placeholder = "All teams")),
      selectizeInput(ns("stations"), "Stations", choices = NULL, multiple = TRUE,
                      options = list(placeholder = "All stations")),
      selectizeInput(ns("categories"), "Observation category", choices = NULL, multiple = TRUE,
                      options = list(placeholder = "All categories (Overview tab)")),
      tags$hr(),
      uiOutput(ns("data_caption"))
    ),
    navset_tab(
      id = ns("subtab"),
      nav_panel(
        "Overview",
        icon = icon("chart-column"),
        layout_columns(
          class = "mb-4 mt-3",
          col_widths = c(3, 3, 3, 3),
          value_box(title = "Patrols Conducted", value = textOutput(ns("kpi_patrols")),
                     showcase = icon("route"), theme = "primary"),
          value_box(title = "Distance Patrolled (km)", value = textOutput(ns("kpi_distance")),
                     showcase = icon("shoe-prints"), theme = "secondary"),
          value_box(title = "Patrol-Days", value = textOutput(ns("kpi_days")),
                     showcase = icon("calendar-day"), theme = "info"),
          value_box(title = "Observations Logged", value = textOutput(ns("kpi_observations")),
                     showcase = icon("clipboard-list"), theme = "success")
        ),
        layout_columns(
          class = "mb-4",
          col_widths = 12,
          card(
            full_screen = TRUE,
            card_header("Patrol Effort Over Time (monthly distance)"),
            plotlyOutput(ns("plot_effort"), height = "300px")
          )
        ),
        layout_columns(
          class = "mb-4",
          col_widths = 12,
          card(
            full_screen = TRUE,
            card_header("Patrol Tracks & Field Observations Map"),
            leafletOutput(ns("map"), height = "640px")
          )
        ),
        card(
          full_screen = TRUE,
          card_header("Patrol Log"),
          DTOutput(ns("patrol_table"), fill = FALSE)
        )
      ),
      nav_panel(
        "Wildlife",
        icon = icon("paw"),
        layout_columns(
          class = "mb-4 mt-3",
          col_widths = c(3, 3, 3, 3),
          value_box(title = "Wildlife Observations", value = textOutput(ns("kpi_sightings")),
                     showcase = icon("binoculars"), theme = "success"),
          value_box(title = "Species", value = textOutput(ns("kpi_species")),
                     showcase = icon("paw"), theme = "primary"),
          value_box(title = "IUCN Threatened", value = textOutput(ns("kpi_threatened")),
                     showcase = icon("shield-heart"), theme = "info"),
          value_box(title = "Nationally Protected Species", value = textOutput(ns("kpi_protected")),
                     showcase = icon("landmark-flag"), theme = "warning")
        ),
        layout_columns(
          class = "mb-4",
          col_widths = 12,
          card(
            full_screen = TRUE,
            card_header("Top 10 Species by CPUE (sightings / 10 km)"),
            plotlyOutput(ns("plot_wildlife_species"), height = "420px")
          )
        ),
        layout_columns(
          class = "mb-4",
          col_widths = 12,
          card(
            full_screen = TRUE,
            card_header("Wildlife Composition (Class › Order › Family › Species)"),
            plotlyOutput(ns("plot_wildlife_treemap"), height = "460px")
          )
        ),
        layout_columns(
          class = "mb-4",
          col_widths = 12,
          card(
            full_screen = TRUE,
            card_header("Wildlife Observation Locations"),
            leafletOutput(ns("wildlife_map"), height = "420px")
          )
        ),
        card(
          full_screen = TRUE,
          card_header("Wildlife Records"),
          DTOutput(ns("wildlife_table"), fill = FALSE)
        )
      ),
      nav_panel(
        "Human Activities",
        icon = icon("triangle-exclamation"),
        layout_columns(
          class = "mb-4 mt-3",
          col_widths = c(4, 4, 4),
          value_box(title = "Total Threats Recorded", value = textOutput(ns("kpi_threats")),
                     showcase = icon("triangle-exclamation"), theme = "danger"),
          value_box(title = "Illegal Logging & Encroachment", value = textOutput(ns("kpi_logging")),
                     showcase = icon("tree-city"), theme = "secondary"),
          value_box(title = "Poaching Incidents", value = textOutput(ns("kpi_poaching")),
                     showcase = icon("crosshairs"), theme = "danger")
        ),
        layout_columns(
          class = "mb-4",
          col_widths = 12,
          card(
            full_screen = TRUE,
            card_header("Threats by Type — CPUE (incidents / 10 km)"),
            plotlyOutput(ns("plot_threat_type"), height = "420px")
          )
        ),
        layout_columns(
          class = "mb-4",
          col_widths = 12,
          card(
            full_screen = TRUE,
            card_header("Threat Locations"),
            leafletOutput(ns("threat_map"), height = "420px")
          )
        ),
        card(
          full_screen = TRUE,
          card_header("Human Activity Records"),
          DTOutput(ns("threat_table"), fill = FALSE)
        )
      )
    )
  )
}

mod_smart_patrol_server <- function(id, data, active_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    cat_colors <- c(
      "Human Activity" = "#d62728", "Wildlife" = "#2ca02c", "Flora" = "#6a994e",
      "Natural Feature" = "#1f77b4", "Natural Disaster" = "#7f7f7f",
      "Biodiversity Survey" = "#9467bd", "Position/Waypoint" = "#c7c7c7"
    )
    finding_colors <- c(
      "Illegal Logging" = "#8c564b", "Encroachment" = "#e377c2", "Mining & Drilling" = "#d62728",
      "Poaching" = "#b91c1c", "NTFP Extraction" = "#f4a300", "Road Access Construction" = "#7f7f7f",
      "Forest & Land Fire" = "#ff7f0e", "Pollution" = "#17becf", "Infrastructure/Facilities" = "#1f77b4",
      "Camera Trapping" = "#6a51a3", "Natural Feature" = "#1f77b4",
      "Wildlife Sign" = "#2ca02c", "Wildlife Encounter" = "#1a7f37", "Dead Wildlife" = "#7f2704"
    )
    team_colors <- c("TM-GAB" = "#1f77b4", "TM-KPH" = "#2c6e49", "TM-UNK" = "#7f7f7f")

    # The four Kategori_observasi values that represent an actual finding
    # worth counting as an "observation" - excludes bare position pings
    # ("Posisi"/blank) and incidental Fitur/Tumbuhan notes.
    meaningful_categories <- c("Human Activity", "Wildlife", "Biodiversity Survey", "Natural Disaster")

    date_bounds <- reactive({
      obs_dates <- as.Date(data$patrol_observations$datetime)
      rng <- range(c(data$patrols$start_date, data$patrols$end_date, obs_dates), na.rm = TRUE)
      rng
    })

    observe({
      rng <- date_bounds()
      updateDateRangeInput(session, "date_range", start = rng[1], end = rng[2], min = rng[1], max = rng[2])
      updateSelectizeInput(session, "teams",
        choices = stats::setNames(data$patrol_teams$team_id, data$patrol_teams$team_name), server = TRUE)
      updateSelectizeInput(session, "stations",
        choices = sort(unique(stats::na.omit(data$patrol_observations$station))), server = TRUE)
      updateSelectizeInput(session, "categories",
        choices = sort(intersect(unique(stats::na.omit(data$patrol_observations$category)),
                                   meaningful_categories)),
        server = TRUE)
    })

    output$data_caption <- renderUI({
      m <- data$smart_meta
      tags$p(
        class = "small text-muted",
        sprintf("%s landscape · %s – %s · %s patrol-days on record · PIC: %s",
                m$landscape, format(m$start_date, "%b %Y"), format(m$end_date, "%b %Y"),
                scales::comma(m$patrol_days), m$pic)
      )
    })

    patrols_f <- reactive({
      req(input$date_range)
      df <- dplyr::filter(data$patrols, start_date >= input$date_range[1], start_date <= input$date_range[2])
      if (length(input$teams) > 0) df <- dplyr::filter(df, team_id %in% input$teams)
      if (length(input$stations) > 0) df <- dplyr::filter(df, station %in% input$stations)
      df
    })

    # Base observation set (date range + team + station only) - Wildlife/
    # Human Activities tabs scope off of this directly so the sidebar's
    # "Observation category" filter (an Overview-only control) doesn't
    # accidentally empty them out.
    obs_base <- reactive({
      req(input$date_range)
      df <- data$patrol_observations |>
        dplyr::filter(as.Date(datetime) >= input$date_range[1], as.Date(datetime) <= input$date_range[2])
      if (length(input$teams) > 0) df <- dplyr::filter(df, team_id %in% input$teams)
      if (length(input$stations) > 0) df <- dplyr::filter(df, station %in% input$stations)
      df |> dplyr::left_join(dplyr::select(data$patrol_teams, team_id, team_name), by = "team_id")
    })

    obs_f <- reactive({
      df <- dplyr::filter(obs_base(), category %in% meaningful_categories)
      if (length(input$categories) > 0) df <- dplyr::filter(df, category %in% input$categories)
      df
    })

    wildlife_f <- reactive({
      dplyr::filter(obs_base(), category == "Wildlife")
    })

    threats_f <- reactive({
      dplyr::filter(obs_base(), category == "Human Activity")
    })

    # Total patrol effort (km) currently in scope (date/team/station filters)
    # - the denominator for effort-normalized sighting/finding rates. Uses
    # datEff (via data$patrol_effort), joined on Patrol_ID, deduped to one
    # effort figure per patrol so effort isn't double-counted across the many
    # observation rows a single patrol can have.
    total_effort_km <- reactive({
      ids <- unique(patrols_f()$patrol_id)
      eff <- dplyr::filter(data$patrol_effort, patrol_id %in% ids)
      sum(eff$effort_km, na.rm = TRUE)
    })

    # ---- Overview ------------------------------------------------------------
    output$kpi_patrols <- renderText(scales::comma(nrow(patrols_f())))
    output$kpi_distance <- renderText(scales::comma(round(sum(patrols_f()$distance_km, na.rm = TRUE))))
    output$kpi_days <- renderText(scales::comma(round(sum(patrols_f()$duration_days, na.rm = TRUE))))
    output$kpi_observations <- renderText(scales::comma(nrow(obs_f())))

    output$plot_effort <- renderPlotly({
      df <- patrols_f()
      req(nrow(df) > 0)
      monthly <- df |>
        dplyr::mutate(month = lubridate::floor_date(start_date, "month")) |>
        dplyr::group_by(month) |>
        dplyr::summarise(distance_km = sum(distance_km, na.rm = TRUE), .groups = "drop")

      p <- ggplot(monthly, aes(x = month, y = distance_km)) +
        geom_col(fill = "#cc5500") +
        labs(x = NULL, y = "Distance patrolled (km)") +
        theme_minimal(base_size = 12)
      ggplotly(p)
    })

    output$map <- renderLeaflet({
      tracks <- dplyr::filter(data$patrol_tracks, patrol_id %in% patrols_f()$patrol_id, has_track)
      obs <- obs_f()

      m <- leaflet() |>
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
        addProviderTiles(providers$OpenStreetMap, group = "Streets")

      if (nrow(tracks) > 0) {
        pal_team <- leaflet::colorFactor(team_colors[names(team_colors) %in% tracks$team_id],
                                           domain = names(team_colors)[names(team_colors) %in% tracks$team_id])
        for (i in seq_len(nrow(tracks))) {
          r <- tracks[i, ]
          m <- m |> addPolylines(
            data = r, color = pal_team(r$team_id), weight = 3, opacity = 0.8,
            group = "Patrol Tracks",
            popup = sprintf("<b>%s</b><br>Distance: %.1f km<br>Date: %s",
                             r$patrol_id, r$distance_km, format(r$start_date, "%d %b %Y"))
          )
        }
      }
      if (nrow(obs) > 0) {
        pal_cat <- leaflet::colorFactor(cat_colors[names(cat_colors) %in% obs$category],
                                          domain = names(cat_colors)[names(cat_colors) %in% obs$category])
        m <- m |>
          addCircleMarkers(
            data = obs, lng = ~lon, lat = ~lat, radius = 4,
            color = ~pal_cat(category), fillOpacity = 0.85, stroke = FALSE,
            group = "Observations",
            popup = ~sprintf("<b>%s</b>%s<br>Date: %s", category,
                              ifelse(is.na(finding_type), "", paste0("<br>", finding_type)),
                              format(datetime, "%Y-%m-%d"))
          ) |>
          addLegend(position = "bottomright", pal = pal_cat, values = obs$category,
                     title = "Observation category", opacity = 0.9)
      }
      m |>
        add_kphp_boundary(data$kphp_boundary) |>
        addLayersControl(
          baseGroups = c("Satellite", "Streets"),
          overlayGroups = c("Patrol Tracks", "Observations", "KPHP VII Boundary"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })

    output$patrol_table <- renderDT({
      # DT tables inside a sub-tab that isn't visible yet never draw
      # correctly even after suspendWhenHidden is disabled (their JS-side
      # width calc needs the container to actually be on screen at render
      # time) - gating on the navset's own selected-tab input guarantees
      # this only (re)renders exactly when its tab is genuinely showing.
      req(active_tab() == "SMART Patrol", input$subtab == "Overview")
      df <- patrols_f() |>
        dplyr::arrange(dplyr::desc(start_date)) |>
        dplyr::select(
          Patrol_ID = patrol_id,
          `Tanggal Mulai` = start_date,
          `Tanggal Selesai` = end_date,
          Duration = duration_days,
          Station = station,
          Team = team_raw,
          Mandate = mandate,
          Leader = leader,
          `Jarak (km)` = distance_km
        )
      render_download_dt(df, filter = "top", order = list(list(1, "desc")))
    })

    # ---- Wildlife --------------------------------------------------------------
    output$kpi_sightings <- renderText(scales::comma(nrow(wildlife_f())))
    output$kpi_species <- renderText(scales::comma(dplyr::n_distinct(stats::na.omit(wildlife_f()$scientific_name))))
    output$kpi_threatened <- renderText({
      df <- dplyr::filter(wildlife_f(), !conservation_status %in% c(NA, "LC", "DD"))
      scales::comma(dplyr::n_distinct(df$scientific_name))
    })
    output$kpi_protected <- renderText({
      df <- dplyr::filter(wildlife_f(), protected)
      scales::comma(dplyr::n_distinct(df$scientific_name))
    })

    output$plot_wildlife_species <- renderPlotly({
      df <- dplyr::filter(wildlife_f(), !is.na(scientific_name))
      req(nrow(df) > 0)

      cpue <- compute_cpue_se(df, "scientific_name", patrols_f()$patrol_id, data$patrol_effort)
      req(nrow(cpue) > 0)
      top <- cpue |>
        dplyr::slice_max(mean_cpue, n = 10) |>
        dplyr::arrange(mean_cpue) |>
        dplyr::mutate(grp = factor(grp, levels = grp))

      p <- ggplot(top, aes(x = mean_cpue, y = grp, text = sprintf("%d sightings across %d patrols", total_n, n_patrols))) +
        geom_col(fill = "#4f46e5") +
        geom_text(aes(label = sprintf("%.2f", mean_cpue)), hjust = -0.2, size = 3.3) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(x = "Sightings per 10 km surveyed", y = NULL) +
        theme_minimal(base_size = 12) +
        theme(axis.text.y = element_text(face = "italic"))
      ggplotly(p, tooltip = c("x", "y", "text"))
    })

    output$plot_wildlife_treemap <- renderPlotly({
      df <- dplyr::filter(wildlife_f(), !is.na(class), !is.na(order), !is.na(family), !is.na(scientific_name))
      req(nrow(df) > 0)

      species_n <- df |> dplyr::count(class, order, family, scientific_name, name = "n")

      class_df <- species_n |>
        dplyr::group_by(class) |>
        dplyr::summarise(n = sum(n), .groups = "drop") |>
        dplyr::transmute(ids = class, labels = class, parents = "", values = n)
      order_df <- species_n |>
        dplyr::group_by(class, order) |>
        dplyr::summarise(n = sum(n), .groups = "drop") |>
        dplyr::transmute(ids = paste(class, order, sep = "/"), labels = order, parents = class, values = n)
      family_df <- species_n |>
        dplyr::group_by(class, order, family) |>
        dplyr::summarise(n = sum(n), .groups = "drop") |>
        dplyr::transmute(ids = paste(class, order, family, sep = "/"), labels = family,
                           parents = paste(class, order, sep = "/"), values = n)
      species_df <- species_n |>
        dplyr::transmute(ids = paste(class, order, family, scientific_name, sep = "/"), labels = scientific_name,
                           parents = paste(class, order, family, sep = "/"), values = n)

      treemap_df <- dplyr::bind_rows(class_df, order_df, family_df, species_df)

      plot_ly(
        data = treemap_df, type = "treemap",
        ids = ~ids, labels = ~labels, parents = ~parents, values = ~values,
        branchvalues = "total",
        textinfo = "label+value+percent parent"
      )
    })

    output$wildlife_map <- renderLeaflet({
      obs <- wildlife_f()
      status_ref <- data$conservation_status_ref
      obs_status <- ifelse(is.na(obs$conservation_status), "Unassessed", obs$conservation_status)
      pal_levels <- unique(c(status_ref$code, "Unassessed"))
      pal_colors <- c(status_ref$color, "Unassessed" = "#c7c7c7")
      pal <- leaflet::colorFactor(pal_colors[pal_levels %in% obs_status], domain = pal_levels[pal_levels %in% obs_status])

      m <- leaflet() |> addProviderTiles(providers$OpenStreetMap)
      if (nrow(obs) > 0) {
        m <- m |>
          addCircleMarkers(
            data = obs, lng = ~lon, lat = ~lat, radius = 5,
            color = ~pal(obs_status), fillOpacity = 0.85, stroke = FALSE,
            popup = ~sprintf("<b>%s</b><br>%s<br>Status: %s<br>Team: %s<br>Date: %s",
                              dplyr::coalesce(common_name_en, species_common, "Unidentified"),
                              dplyr::coalesce(finding_type, ""),
                              dplyr::coalesce(conservation_status, "Unassessed"),
                              team_name, format(datetime, "%Y-%m-%d"))
          ) |>
          addLegend(position = "bottomright", pal = pal, values = obs_status,
                     title = "Conservation status", opacity = 0.9)
      }
      m |> add_kphp_boundary(data$kphp_boundary)
    })

    output$wildlife_table <- renderDT({
      req(active_tab() == "SMART Patrol", input$subtab == "Wildlife")
      df <- wildlife_f() |>
        dplyr::mutate(Species = dplyr::coalesce(common_name_en, species_common)) |>
        dplyr::arrange(dplyr::desc(datetime)) |>
        dplyr::select(DateTime = datetime, Team = team_name, Species,
                       `Scientific Name` = scientific_name, Finding = finding_type,
                       Count = count, Status = conservation_status, Station = station)
      render_download_dt(df, filter = "top", order = list(list(0, "desc")))
    })

    # ---- Human Activities ---------------------------------------------------------
    output$kpi_threats <- renderText(scales::comma(nrow(threats_f())))
    output$kpi_logging <- renderText({
      scales::comma(sum(threats_f()$finding_type %in% c("Illegal Logging", "Encroachment"), na.rm = TRUE))
    })
    output$kpi_poaching <- renderText(scales::comma(sum(threats_f()$finding_type == "Poaching", na.rm = TRUE)))

    output$plot_threat_type <- renderPlotly({
      df <- dplyr::filter(threats_f(), !is.na(finding_type))
      req(nrow(df) > 0)

      cpue <- compute_cpue_se(df, "finding_type", patrols_f()$patrol_id, data$patrol_effort)
      req(nrow(cpue) > 0)
      top <- cpue |>
        dplyr::arrange(mean_cpue) |>
        dplyr::mutate(grp = factor(grp, levels = grp))

      p <- ggplot(top, aes(x = mean_cpue, y = grp, fill = grp,
                             text = sprintf("%d incidents across %d patrols", total_n, n_patrols))) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = sprintf("%.2f", mean_cpue)), hjust = -0.2, size = 3.3) +
        scale_fill_manual(values = finding_colors) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(x = "Incidents per 10 km surveyed", y = NULL) +
        theme_minimal(base_size = 12)
      ggplotly(p, tooltip = c("x", "y", "text"))
    })

    output$threat_map <- renderLeaflet({
      obs <- dplyr::filter(threats_f(), !is.na(finding_type))
      pal <- leaflet::colorFactor(finding_colors[names(finding_colors) %in% obs$finding_type],
                                    domain = names(finding_colors)[names(finding_colors) %in% obs$finding_type])

      m <- leaflet() |> addProviderTiles(providers$Esri.WorldImagery)
      if (nrow(obs) > 0) {
        m <- m |>
          addCircleMarkers(
            data = obs, lng = ~lon, lat = ~lat, radius = 6,
            color = ~pal(finding_type), fillOpacity = 0.85, stroke = FALSE,
            popup = ~sprintf("<b>%s</b><br>Team: %s<br>Date: %s%s",
                              finding_type, team_name, format(datetime, "%Y-%m-%d"),
                              ifelse(is.na(violation), "", paste0("<br>Violation: ", violation)))
          ) |>
          addLegend(position = "bottomright", pal = pal, values = obs$finding_type,
                     title = "Threat type", opacity = 0.9)
      }
      m |> add_kphp_boundary(data$kphp_boundary)
    })

    output$threat_table <- renderDT({
      req(active_tab() == "SMART Patrol", input$subtab == "Human Activities")
      df <- threats_f() |>
        dplyr::arrange(dplyr::desc(datetime)) |>
        dplyr::select(DateTime = datetime, Team = team_name, `Finding Type` = finding_type,
                       Detail = finding_detail, Violation = violation,
                       `Follow-up` = follow_up_status, Station = station)
      render_download_dt(df, filter = "top", order = list(list(0, "desc")))
    })

    unsuspend_all(output, c(
      "data_caption", "kpi_patrols", "kpi_distance", "kpi_days", "kpi_observations",
      "plot_effort", "map", "patrol_table",
      "kpi_sightings", "kpi_species", "kpi_threatened", "kpi_protected",
      "plot_wildlife_species", "plot_wildlife_treemap", "wildlife_map", "wildlife_table",
      "kpi_threats", "kpi_logging", "kpi_poaching", "plot_threat_type",
      "threat_map", "threat_table"
    ))
  })
}
