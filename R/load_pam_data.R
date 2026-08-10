# ---- Real bioacoustic (PAM) data loader ------------------------------------
# Loads data/pam_data.RData (pam_detection = individual acoustic ID events,
# pam_effort = recorder deployment/retrieval log) from the Kerinci-Seblat
# landscape and reshapes it into acoustic_sites (one row per recorder) and
# acoustic_detections (one row per detection) for mod_bioacoustics.R.
#
# Unlike the camera trap effort log, every pam_effort row already has both a
# setup AND a retrieval date/time (no still-deployed recorders) and there is
# exactly one row per Station.ID (no redeployments), so no aggregation is
# needed - just a straight column reshape.

load_pam_data <- function(path = "data/pam_data.RData",
                            taxon_path = "data/taxon_reference.csv") {
  e <- new.env()
  load(path, envir = e)
  pam_detection <- e$pam_detection
  pam_effort <- e$pam_effort
  taxon_ref <- load_taxon_reference(taxon_path)

  # Setup_date/Retrieval_date carry the real calendar date (time = midnight);
  # Setup_time/Retrieval_time carry the real time-of-day (date = a dummy
  # 1899-12-31 baked in by however this was exported) - stitch each pair
  # into one real datetime.
  combine_datetime <- function(date_col, time_col) {
    as.POSIXct(paste(as.Date(date_col), format(time_col, "%H:%M:%S")), tz = "UTC")
  }

  acoustic_sites <- pam_effort |>
    dplyr::transmute(
      recorder_id = Station.ID,
      landscape = Landscape,
      region = Region,
      site = Site,
      lat = Latitude,
      lon = Longitude,
      unit = Unit,
      setup_datetime = combine_datetime(Setup_date, Setup_time),
      retrieval_datetime = combine_datetime(Retrieval_date, Retrieval_time),
      recording_hours = as.numeric(difftime(retrieval_datetime, setup_datetime, units = "hours"))
    )

  # As with camera trap, taxonomy/status fields are NOT kept here - data$species
  # (built from taxon_reference.csv) is the join target downstream, so this
  # only uses the reference to decide which rows to exclude.
  acoustic_detections <- pam_detection |>
    dplyr::mutate(
      detection_id = sprintf("PAM-%05d", dplyr::row_number()),
      recorder_id = Station.ID,
      session = Session,
      datetime = as.POSIXct(recording_datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      scientific_name = trimws(Scientific.Name),
      species_id = slugify_species(scientific_name),
      match_key = tolower(scientific_name)
    ) |>
    dplyr::filter(!is.na(datetime)) |>
    dplyr::left_join(taxon_ref, by = "match_key") |>
    # Same house rule as SMART/camera trap: a record naming a species with
    # no taxon_reference match is excluded, not guessed at.
    dplyr::filter(!is.na(class)) |>
    dplyr::select(detection_id, recorder_id, session, datetime, scientific_name, species_id, confidence)

  list(
    acoustic_sites = acoustic_sites,
    acoustic_detections = acoustic_detections
  )
}
