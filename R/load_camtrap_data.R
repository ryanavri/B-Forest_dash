# ---- Real camera trap data loader ------------------------------------------
# Loads data/camtrap_data.RData (detection = individual photo/species records,
# effort = camera deployment/retrieval log) from the Kerinci-Seblat landscape
# and reshapes it into camera_sites (one row per station) and
# camera_detections (one row per detection record) for mod_camera_trap.R and
# the cross-tab species KPIs in mod_summary.R.
#
# `effort` has one row per deployment EVENT, not per station - a station gets
# a second row when it's re-baited/redeployed (e.g. MP02). Deployment-level
# detail isn't used anywhere downstream (the Active Station tab that would
# need it has been removed), so deployments are aggregated to one row per
# station: trap_nights summed across events (each event's own malfunction
# window already subtracted), Setup_date = earliest, Retrieval_date = latest
# (NA - i.e. "Active" - if any event at that station is still deployed).

load_camtrap_data <- function(path = "data/camtrap_data.RData",
                                taxon_path = "data/taxon_reference.csv") {
  e <- new.env()
  load(path, envir = e)
  detection <- e$detection
  effort <- e$effort
  taxon_ref <- load_taxon_reference(taxon_path)

  parse_dmy <- function(x) as.Date(x, format = "%d-%b-%y")

  # ---- Effort / sites -------------------------------------------------------
  # Nights a deployment was actually operating = (retrieval - setup), minus
  # any malfunction window (Problem1_from/to) logged for that event. Cameras
  # with no retrieval date yet are still deployed - "as of" today.
  eff <- effort |>
    dplyr::mutate(
      setup_date = parse_dmy(Setup_date),
      retrieval_date = parse_dmy(Retrieval_date),
      problem_from = parse_dmy(Problem1_from),
      problem_to = parse_dmy(Problem1_to),
      end_date = dplyr::coalesce(retrieval_date, Sys.Date()),
      problem_days = dplyr::if_else(
        !is.na(problem_from) & !is.na(problem_to),
        as.numeric(problem_to - problem_from), 0
      ),
      event_nights = pmax(0, as.numeric(end_date - setup_date) - problem_days),
      is_active = is.na(retrieval_date)
    )

  camera_sites <- eff |>
    dplyr::group_by(station_id = Station) |>
    dplyr::summarise(
      landscape = dplyr::first(Landscape),
      region = dplyr::first(Region),
      site = dplyr::first(Site),
      latitude = dplyr::first(Latitude),
      longitude = dplyr::first(Longitude),
      setup_date = min(setup_date, na.rm = TRUE),
      retrieval_date = dplyr::if_else(
        any(is_active), as.Date(NA),
        suppressWarnings(max(retrieval_date, na.rm = TRUE))
      ),
      status = dplyr::if_else(any(is_active), "Active", "Retrieved"),
      trap_nights = sum(event_nights),
      n_deployments = dplyr::n(),
      .groups = "drop"
    )

  # ---- Detections -------------------------------------------------------------
  # Taxonomy/status fields are NOT kept here - the app-wide species master
  # list (data$species, R/mock_data.R, itself built from taxon_reference.csv)
  # is the join target downstream, so this only uses the reference to decide
  # which rows to exclude.
  camera_detections <- detection |>
    dplyr::mutate(
      detection_id = sprintf("CT-%05d", dplyr::row_number()),
      station_id = Station,
      date = parse_dmy(Date),
      datetime = as.POSIXct(paste(Date, Time), format = "%d-%b-%y %H:%M", tz = "UTC"),
      scientific_name = trimws(Scientific.Name),
      species_id = slugify_species(scientific_name),
      count = suppressWarnings(as.numeric(Count)),
      count = dplyr::coalesce(count, 1),
      session = Session,
      match_key = tolower(scientific_name)
    ) |>
    dplyr::left_join(taxon_ref, by = "match_key") |>
    # Same house rule as SMART: a record naming a species with no
    # taxon_reference match is excluded, not guessed at.
    dplyr::filter(!is.na(class)) |>
    dplyr::select(detection_id, station_id, session, date, datetime,
                   scientific_name, species_id, count)

  list(
    camera_sites = camera_sites,
    camera_detections = camera_detections
  )
}
