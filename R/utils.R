# ---- Shared helpers ------------------------------------------------------

# This app's top nav (nav_menu dropdowns wrapping nested navset_tab sub-tabs)
# never fires the DOM event Shiny's default suspendWhenHidden relies on to
# resume a tab's outputs once it's shown - only the tab active at initial
# page load ever renders, everything else stays stuck at "" forever. Call
# this at the end of every moduleServer body with every output id it
# registers, so all of them compute regardless of which tab is visible.
unsuspend_all <- function(output, ids) {
  for (nm in ids) outputOptions(output, nm, suspendWhenHidden = FALSE)
}

# Deterministic species_id from a scientific name (e.g. "Panthera tigris" ->
# "Panthera_tigris") - used everywhere a species needs a stable join key,
# so the mock species master list (R/mock_data.R) and real detection
# loaders (R/load_camtrap_data.R) always agree on the same id for the same
# species without needing to look one another up at load time.
slugify_species <- function(x) gsub("[^A-Za-z0-9]+", "_", trimws(x))

# DT::datatable() wrapper that adds CSV/Excel download buttons consistently
# across every table in the app. Returns a normal datatable object, so
# formatStyle()/formatPercentage() etc. can still be chained on the result.
render_download_dt <- function(df, filter = "none", page_length = 10,
                                 order = NULL, dom = "Blfrtip") {
  opts <- list(
    dom = dom,
    pageLength = page_length,
    buttons = list(
      list(extend = "csv", text = "Download CSV"),
      list(extend = "excel", text = "Download Excel")
    )
  )
  if (!is.null(order)) opts$order <- order

  DT::datatable(
    df,
    rownames = FALSE,
    filter = filter,
    extensions = "Buttons",
    options = opts
  )
}

# Adds the KPHP VII forest-management-unit boundary (data/KPHP_VII.shp,
# loaded once in app.R as an sf object) to a leaflet map as a light,
# non-obscuring context layer - low fill opacity so it never hides markers
# underneath, appended last in every map so call order elsewhere doesn't
# matter. Silently no-ops if the boundary failed to load.
add_kphp_boundary <- function(map, kphp_boundary) {
  if (is.null(kphp_boundary)) return(map)
  zone_colors <- c(Reference = "#4a90d9", Intervence = "#e8590c")
  map |>
    leaflet::addPolygons(
      data = kphp_boundary,
      fillColor = ~zone_colors[Type], fillOpacity = 0.06,
      color = ~zone_colors[Type], weight = 2, dashArray = "4 4", opacity = 0.8,
      group = "KPHP VII Boundary",
      label = ~sprintf("%s (%s)", Name, Type),
      popup = ~sprintf("<b>%s</b><br>Zone: %s<br>Area: %s ha", Name, Type, scales::comma(Hectares))
    )
}

# Shared base map for Camera Trap and Bioacoustics: KPHP VII boundary +
# monitoring grid (data/Grid_hexa.shp) rendered via mapview instead of plain
# leaflet::addPolygons, per explicit request for this styling. mapview()
# objects wrap a real leaflet map in their @map slot, so the result can still
# be piped straight into ordinary addCircleMarkers()/addLegend() calls for
# the station markers layered on top.
kphp_grid_base_map <- function(kphp_boundary, monitoring_grid) {
  m <- (
    mapview::mapview(
      kphp_boundary, layer.name = "KPHP VII", color = "#D7301F", col.regions = "#FC8D59",
      lwd = 1, map.types = c("OpenStreetMap", "Esri.WorldImagery"), alpha.regions = 0
    ) +
      mapview::mapview(
        monitoring_grid, layer.name = "Grid", color = "#2C3E50", col.regions = "#74A9CF",
        lwd = 1, alpha.regions = 0.05
      )
  )@map
  # mapview auto-adds its own layers control for the KPHP/Grid layers, but
  # callers always add a station-markers layer on top that control doesn't
  # know about - strip it so the caller can add a single, complete control
  # (base tiles + KPHP + Grid + its own marker group) instead of stacking two.
  leaflet::removeLayersControl(m)
}

# CPUE (catch/sighting-per-unit-effort) with standard error, computed the
# proper way for a ratio estimator: per-PATROL rate (including patrols with
# zero sightings of a given group, not just the patrols where it showed up),
# then mean +/- SE of that per-patrol rate across patrols - rather than a
# single pooled count/effort ratio, which has no meaningful variance.
#
#   obs         - observation rows to tally (already filtered to the
#                 category/tab of interest, e.g. Wildlife or Human Activity)
#   group_col   - name of the column in `obs` to group by (e.g.
#                 "scientific_name" or "finding_type")
#   patrol_ids  - patrol_id values in the current filter scope (the
#                 denominator population - effort spent regardless of
#                 whether anything was seen)
#   effort_df   - data.frame with patrol_id, effort_km (one row per patrol)
#   per_x_km    - normalize to "per this many km" (10 -> sightings/10km)
#
# Returns one row per group: grp, mean_cpue, se_cpue, total_n, n_patrols.
compute_cpue_se <- function(obs, group_col, patrol_ids, effort_df, per_x_km = 10) {
  eff <- effort_df |>
    dplyr::filter(patrol_id %in% patrol_ids, !is.na(effort_km), effort_km > 0) |>
    dplyr::distinct(patrol_id, .keep_all = TRUE)
  if (nrow(eff) == 0) return(tibble::tibble())

  obs_grp <- obs |>
    dplyr::rename(grp = dplyr::all_of(group_col)) |>
    dplyr::filter(!is.na(grp), patrol_id %in% eff$patrol_id)
  groups <- unique(obs_grp$grp)
  if (length(groups) == 0) return(tibble::tibble())

  counts <- dplyr::count(obs_grp, patrol_id, grp, name = "n")

  tidyr::expand_grid(patrol_id = eff$patrol_id, grp = groups) |>
    dplyr::left_join(counts, by = c("patrol_id", "grp")) |>
    dplyr::mutate(n = tidyr::replace_na(n, 0)) |>
    dplyr::left_join(eff, by = "patrol_id") |>
    dplyr::mutate(cpue = n / effort_km * per_x_km) |>
    dplyr::group_by(grp) |>
    dplyr::summarise(
      mean_cpue = mean(cpue),
      se_cpue = stats::sd(cpue) / sqrt(dplyr::n()),
      total_n = sum(n),
      n_patrols = dplyr::n(),
      .groups = "drop"
    )
}
