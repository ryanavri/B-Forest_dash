# Generates a self-consistent set of mock biodiversity datasets so the
# dashboard runs end-to-end without any external data source. Swap the
# body of generate_biodash_data() for real data loaders (read_csv, DB
# queries, API pulls) when connecting to a live database - the rest of
# the app only depends on the column names/types produced here.

# Chapman-Richards sigmoidal growth model (Richards 1959; Chapman 1961) -
# the standard functional form for cumulative forest biomass/carbon
# accumulation over stand age: slow start, fast-growing middle years,
# plateauing as the stand approaches ecological maturity.
chapman_richards <- function(t, asymptote, k, p) {
  asymptote * (1 - exp(-k * t))^p
}

generate_biodash_data <- function(seed = 42, taxon_path = "data/taxon_reference.csv") {
  set.seed(seed)

  today <- Sys.Date()
  study_start <- today - 545 # ~18 months of history

  # ---- Species master list (derived from taxon_reference.csv) ----------------
  # Built directly from the authoritative taxon reference rather than a
  # hand-curated list - every species anywhere in the app (camera trap,
  # bioacoustic, transect, SMART patrol) is one that appears in
  # data/taxon_reference.csv, nothing fabricated or guessed. method/
  # activity_type are heuristics derived from Class/Order (used only to
  # drive mock bioacoustic/transect sampling - real detections, like camera
  # trap's, use their own actual timestamps and need no such guess).
  taxon_csv <- utils::read.csv(taxon_path, stringsAsFactors = FALSE)
  species <- taxon_csv |>
    dplyr::transmute(
      species_id = slugify_species(Species),
      scientific_name = trimws(Species),
      common_name = dplyr::na_if(trimws(Common.name), ""),
      common_name = dplyr::coalesce(common_name, scientific_name),
      taxon_group = dplyr::case_when(
        Class == "Mammalia" ~ "Mammal",
        Class == "Aves" ~ "Bird",
        Class == "Amphibia" ~ "Amphibian",
        Class == "Reptilia" ~ "Reptile",
        TRUE ~ Class
      ),
      class = Class, order = Order, family = Family,
      conservation_status = dplyr::na_if(trimws(Status), "Not found"),
      conservation_status = dplyr::na_if(conservation_status, ""),
      protected = dplyr::coalesce(trimws(Protected) == "Y", FALSE),
      method = dplyr::case_when(
        Order == "Primates" ~ "both",
        Class %in% c("Aves", "Amphibia") ~ "acoustic",
        TRUE ~ "camera"
      ),
      activity_type = dplyr::case_when(
        Class == "Aves" ~ "diurnal",
        Class == "Amphibia" ~ "nocturnal",
        Class == "Reptilia" ~ "cathemeral",
        Order == "Primates" ~ "diurnal",
        Order == "Carnivora" ~ "nocturnal",
        Order %in% c("Artiodactyla", "Perrisodactyla") ~ "crepuscular",
        Class == "Mammalia" ~ "nocturnal",
        TRUE ~ "cathemeral"
      ),
      # No curated prose exists for a ~500-species authoritative list - left
      # blank rather than fabricated; species_card UIs hide the paragraph
      # when NA. image_url is likewise NA except a live example so the
      # "photo swaps in automatically once set" mechanic stays demonstrable.
      description = NA_character_,
      image_url = dplyr::case_when(
        scientific_name == "Panthera tigris" ~ "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcS74rSCa9dpCdoZBRiwXEiskm-CZD6c74CohgnUCGdZew&s=10",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(scientific_name != "") |>
    dplyr::distinct(species_id, .keep_all = TRUE)

  # IUCN Red List category reference - shared lookup so every tab renders the
  # same label/color/meaning for a given conservation_status code instead of
  # each module hardcoding its own copy.
  conservation_status_ref <- tibble::tribble(
    ~code, ~label,                  ~color,     ~description,
    "LC",  "Least Concern",         "#1a7f37",  "Widespread and abundant; not currently at significant risk of extinction.",
    "NT",  "Near Threatened",       "#9a6700",  "Close to qualifying for a threatened category in the near future.",
    "VU",  "Vulnerable",            "#b45309",  "Facing a high risk of extinction in the wild.",
    "EN",  "Endangered",            "#c2410c",  "Facing a very high risk of extinction in the wild.",
    "CR",  "Critically Endangered", "#b91c1c",  "Facing an extremely high risk of extinction in the wild.",
    "DD",  "Data Deficient",        "#6b7280",  "Not enough data available to assess extinction risk."
  )

  # Lookup by species_id for the mock generators below (bioacoustic/transect
  # sampling) - the activity_type column computed above, keyed for fast
  # `activity_type[[species_id]]` access.
  activity_type <- stats::setNames(species$activity_type, species$species_id)

  sample_hours <- function(n, type) {
    weights <- switch(type,
      nocturnal   = ifelse(0:23 %in% c(19:23, 0:5), 3, 0.2),
      diurnal     = ifelse(0:23 %in% 6:18, 3, 0.2),
      crepuscular = ifelse(0:23 %in% c(5, 6, 7, 17, 18, 19), 4, 0.5),
      cathemeral  = rep(1, 24)
    )
    sample(0:23, n, replace = TRUE, prob = weights)
  }

  habitats <- c(
    "Lowland Dipterocarp Forest", "Riparian Forest", "Peat Swamp Forest",
    "Montane Forest", "Secondary Forest", "Forest Edge"
  )

  # Park bounding box (fictional "Wanariung National Park"), centered on
  # Jambi, Sumatra (-2.58387, 102.22876).
  park_centroid <- c(lat = -2.58387, lon = 102.22876)
  lat_range <- park_centroid["lat"] + c(-0.5, 0.5)
  lon_range <- park_centroid["lon"] + c(-0.4, 0.4)

  jitter_latlon <- function(n) {
    tibble::tibble(
      lat = runif(n, lat_range[1], lat_range[2]),
      lon = runif(n, lon_range[1], lon_range[2])
    )
  }

  # ---- Survey sessions --------------------------------------------------------
  # Camera trap, bioacoustic, and transect fieldwork are all organized into
  # discrete ~quarterly survey sessions (deployment/servicing rounds) - the
  # unit field teams actually plan and report around, and the basis for the
  # "Session" filter on each of those three tabs.
  n_sessions <- max(2, ceiling(as.numeric(today - study_start) / 90))
  session_starts <- study_start + (seq_len(n_sessions) - 1) * 90
  session_ends <- pmin(session_starts + 89, today)
  sessions <- tibble::tibble(
    session_id = sprintf("S%02d", seq_len(n_sessions)),
    label = sprintf("Session %d (%s–%s)", seq_len(n_sessions),
                     format(session_starts, "%b %Y"), format(session_ends, "%b %Y")),
    start_date = session_starts,
    end_date = session_ends
  )
  assign_session <- function(dates) {
    idx <- findInterval(as.Date(dates), session_starts)
    idx <- pmin(pmax(idx, 1), n_sessions)
    sessions$session_id[idx]
  }

  # Camera-detectable species pool - camera trap itself now runs on real
  # data (R/load_camtrap_data.R), but Transect still mock-generates
  # sightings and reuses this same visually-detectable species set.
  cam_species <- dplyr::filter(species, method %in% c("camera", "both"))

  # ---- Bioacoustic sites & effort ------------------------------------------
  n_ar <- 12
  acoustic_sites <- dplyr::bind_cols(
    tibble::tibble(
      recorder_id = sprintf("AR-%02d", 1:n_ar),
      habitat = sample(habitats, n_ar, replace = TRUE),
      elevation_m = round(runif(n_ar, 40, 950)),
      deploy_date = study_start + sample(0:60, n_ar, replace = TRUE)
    ),
    jitter_latlon(n_ar)
  ) |>
    dplyr::mutate(
      retrieval_date = pmin(deploy_date + sample(180:545, n_ar, replace = TRUE), today),
      recording_days = as.integer(retrieval_date - deploy_date),
      # Duty-cycled recording schedule (hours actually recorded per day),
      # e.g. dawn/dusk-only vs. continuous - varies by recorder programming.
      hours_per_day = sample(c(4, 6, 8, 12, 24), n_ar, replace = TRUE,
                               prob = c(0.15, 0.2, 0.25, 0.2, 0.2)),
      recording_hours = recording_days * hours_per_day,
      status = ifelse(retrieval_date >= today - 14, "Active", "Retrieved")
    )

  # ---- Bioacoustic detections -----------------------------------------------
  ac_species <- dplyr::filter(species, method %in% c("acoustic", "both"))
  n_ac_det <- 1900
  acoustic_detections <- tibble::tibble(
    detection_id = sprintf("ARD-%05d", 1:n_ac_det),
    recorder_id = sample(acoustic_sites$recorder_id, n_ac_det, replace = TRUE),
    species_id = sample(ac_species$species_id, n_ac_det, replace = TRUE,
                         prob = rev(seq_len(nrow(ac_species)))^1.2),
    confidence = round(pmin(1, rbeta(n_ac_det, 6, 2)), 2)
  ) |>
    dplyr::left_join(dplyr::select(acoustic_sites, recorder_id, deploy_date, retrieval_date),
                      by = "recorder_id") |>
    dplyr::rowwise() |>
    dplyr::mutate(
      obs_date = deploy_date + sample(0:as.integer(retrieval_date - deploy_date), 1),
      hour = sample_hours(1, activity_type[[species_id]]),
      datetime = as.POSIXct(obs_date) + lubridate::hours(hour) + lubridate::minutes(sample(0:59, 1))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(session_id = assign_session(datetime)) |>
    dplyr::select(detection_id, recorder_id, species_id, datetime, confidence, session_id)

  # ---- Transect routes ---------------------------------------------------------
  n_tx <- 8
  km_per_deg_lat <- 111
  km_per_deg_lon <- 111 * cos(park_centroid["lat"] * pi / 180)
  tx_length_km <- round(runif(n_tx, 1.5, 5), 1)
  tx_bearing <- runif(n_tx, 0, 2 * pi)
  tx_start <- jitter_latlon(n_tx)

  transect_routes <- tibble::tibble(
    transect_id = sprintf("TX-%02d", 1:n_tx),
    transect_name = paste("Transect", LETTERS[1:n_tx]),
    habitat = sample(habitats, n_tx, replace = TRUE),
    elevation_m = round(runif(n_tx, 40, 950)),
    length_km = tx_length_km,
    lat = tx_start$lat,
    lon = tx_start$lon,
    end_lat = tx_start$lat + (tx_length_km / km_per_deg_lat) * cos(tx_bearing),
    end_lon = tx_start$lon + (tx_length_km / km_per_deg_lon) * sin(tx_bearing),
    established_date = study_start + sample(0:60, n_tx, replace = TRUE)
  )

  # ---- Transect surveys (repeat walks across sessions) ---------------------------
  survey_rows <- purrr::map_dfr(seq_len(n_tx), function(i) {
    walked <- sample(c(TRUE, FALSE), n_sessions, replace = TRUE, prob = c(0.8, 0.2))
    walked_sessions <- which(walked)
    if (length(walked_sessions) == 0) walked_sessions <- n_sessions
    tibble::tibble(
      transect_id = transect_routes$transect_id[i],
      session_id = sessions$session_id[walked_sessions],
      survey_date = pmin(sessions$start_date[walked_sessions] +
                            sample(5:80, length(walked_sessions), replace = TRUE), today)
    )
  })

  n_survey <- nrow(survey_rows)
  transect_surveys <- survey_rows |>
    dplyr::mutate(
      survey_id = sprintf("TXS-%04d", dplyr::row_number()),
      team = sample(c("Transect Team 1", "Transect Team 2"), n_survey, replace = TRUE),
      weather = sample(c("Clear", "Overcast", "Light Rain"), n_survey, replace = TRUE,
                         prob = c(0.55, 0.35, 0.1))
    ) |>
    dplyr::left_join(dplyr::select(transect_routes, transect_id, length_km), by = "transect_id") |>
    dplyr::mutate(effort_km = round(length_km * runif(n_survey, 0.9, 1), 2)) |>
    dplyr::select(survey_id, transect_id, session_id, survey_date, team, weather, effort_km)

  # Still in the active survey rotation vs discontinued, based on how recent
  # its last walk was - the transect equivalent of a camera's Active/Retrieved
  # deployment status.
  last_survey <- transect_surveys |>
    dplyr::group_by(transect_id) |>
    dplyr::summarise(last_survey_date = max(survey_date), .groups = "drop")

  transect_routes <- transect_routes |>
    dplyr::left_join(last_survey, by = "transect_id") |>
    dplyr::mutate(status = ifelse(last_survey_date >= today - 100, "Active", "Retrieved"))

  # ---- Transect observations -----------------------------------------------------
  # Visually/audibly encountered while walking in daylight - reuses the same
  # visually-detectable species pool as camera trap (mammals, primates, some
  # reptiles), sampled within a plausible daytime survey window.
  n_tx_det <- 850
  transect_observations <- tibble::tibble(
    obs_id = sprintf("TXO-%05d", 1:n_tx_det),
    survey_id = sample(transect_surveys$survey_id, n_tx_det, replace = TRUE),
    species_id = sample(cam_species$species_id, n_tx_det, replace = TRUE,
                          prob = rev(seq_len(nrow(cam_species)))^1.1),
    count = sample(1:5, n_tx_det, replace = TRUE, prob = c(0.5, 0.25, 0.13, 0.07, 0.05)),
    perpendicular_distance_m = round(rexp(n_tx_det, rate = 1 / 25)),
    hour = sample(6:17, n_tx_det, replace = TRUE),
    minute = sample(0:59, n_tx_det, replace = TRUE)
  ) |>
    dplyr::left_join(dplyr::select(transect_surveys, survey_id, transect_id, session_id, survey_date),
                      by = "survey_id") |>
    dplyr::mutate(datetime = as.POSIXct(survey_date) + lubridate::hours(hour) + lubridate::minutes(minute)) |>
    dplyr::select(obs_id, survey_id, transect_id, session_id, species_id, datetime,
                   count, perpendicular_distance_m)

  # ---- Restoration & nursery tree species -----------------------------------
  tree_species <- tibble::tribble(
    ~species_id, ~common_name,      ~scientific_name,           ~family,             ~type,         ~priority,
    "TS01", "Meranti Tembaga", "Shorea leprosula",         "Dipterocarpaceae",  "Dipterocarp", "High",
    "TS02", "Meranti Kuning",  "Shorea johorensis",        "Dipterocarpaceae",  "Dipterocarp", "High",
    "TS03", "Keruing",         "Dipterocarpus stellatus",  "Dipterocarpaceae",  "Dipterocarp", "High",
    "TS04", "Kapur",           "Dryobalanops lanceolata",  "Dipterocarpaceae",  "Dipterocarp", "Medium",
    "TS05", "Sangal",          "Hopea sangal",             "Dipterocarpaceae",  "Dipterocarp", "Medium",
    "TS06", "Pulai",           "Alstonia scholaris",       "Apocynaceae",       "Pioneer",     "Low",
    "TS07", "Jelutong",        "Dyera costulata",          "Apocynaceae",       "Pioneer",     "Medium",
    "TS08", "Tualang",         "Koompassia excelsa",       "Fabaceae",          "Emergent",    "High",
    "TS09", "Durian Hutan",    "Durio zibethinus",         "Malvaceae",         "Fruit",       "Low",
    "TS10", "Terap",           "Artocarpus elasticus",     "Moraceae",          "Fruit",       "Low",
    "TS11", "Kempas",          "Koompassia malaccensis",   "Fabaceae",          "Emergent",    "Medium",
    "TS12", "Rengas",          "Gluta renghas",            "Anacardiaceae",     "Pioneer",     "Low"
  )

  # Indicative aboveground+belowground carbon potential at stand maturity,
  # by growth type (emergent/dipterocarp species accumulate far more woody
  # biomass than fast-growing pioneers or fruit trees). Placeholder figures
  # for illustration - replace with species-specific allometric estimates.
  tree_species <- tree_species |>
    dplyr::mutate(
      carbon_potential_tco2_ha = round(dplyr::case_when(
        type == "Emergent"    ~ runif(dplyr::n(), 210, 260),
        type == "Dipterocarp" ~ runif(dplyr::n(), 170, 220),
        type == "Pioneer"     ~ runif(dplyr::n(), 90, 130),
        type == "Fruit"       ~ runif(dplyr::n(), 70, 110),
        TRUE ~ 120
      ))
    )

  # ---- Restoration sites -----------------------------------------------------
  n_rs <- 10
  restoration_sites <- dplyr::bind_cols(
    tibble::tibble(
      site_id = sprintf("RS-%02d", 1:n_rs),
      site_name = paste("Restoration Block", LETTERS[1:n_rs]),
      habitat_target = sample(habitats, n_rs, replace = TRUE),
      area_ha = round(runif(n_rs, 1.5, 12), 1),
      start_date = study_start + sample(0:200, n_rs, replace = TRUE)
    ),
    jitter_latlon(n_rs)
  ) |>
    dplyr::mutate(
      status = dplyr::case_when(
        as.integer(today - start_date) < 120 ~ "Newly Planted",
        as.integer(today - start_date) < 365 ~ "Active Monitoring",
        TRUE ~ "Established"
      )
    )

  # ---- Planting events ---------------------------------------------------------
  n_plant <- 420
  planting_events <- tibble::tibble(
    event_id = sprintf("PE-%05d", 1:n_plant),
    site_id = sample(restoration_sites$site_id, n_plant, replace = TRUE),
    species_id = sample(tree_species$species_id, n_plant, replace = TRUE,
                         prob = c(5, 5, 4, 3, 3, 2, 2, 3, 1, 1, 2, 1)),
    team = sample(c("Restoration Team 1", "Restoration Team 2", "Community Planters"),
                   n_plant, replace = TRUE),
    seedlings_planted = sample(20:180, n_plant, replace = TRUE)
  ) |>
    dplyr::left_join(dplyr::select(restoration_sites, site_id, start_date), by = "site_id") |>
    dplyr::rowwise() |>
    dplyr::mutate(date = start_date + sample(0:as.integer(today - start_date), 1)) |>
    dplyr::ungroup() |>
    dplyr::select(event_id, site_id, species_id, team, date, seedlings_planted)

  # ---- Survival monitoring -----------------------------------------------------
  n_mon <- 320
  survival_monitoring <- tibble::tibble(
    mon_id = sprintf("SM-%05d", 1:n_mon),
    site_id = sample(restoration_sites$site_id, n_mon, replace = TRUE),
    species_id = sample(tree_species$species_id, n_mon, replace = TRUE)
  ) |>
    dplyr::left_join(dplyr::select(restoration_sites, site_id, start_date), by = "site_id") |>
    dplyr::rowwise() |>
    dplyr::mutate(
      date = pmin(start_date + sample(60:600, 1), today),
      seedlings_checked = sample(20:100, 1),
      seedlings_alive = rbinom(1, seedlings_checked, runif(1, 0.55, 0.92))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(survival_rate = round(seedlings_alive / seedlings_checked, 3)) |>
    dplyr::select(mon_id, site_id, species_id, date, seedlings_checked, seedlings_alive, survival_rate)

  # ---- Nursery ------------------------------------------------------------------
  nursery_facility <- tibble::tibble(
    facility_id = "NUR-01",
    facility_name = "Wanariung Central Nursery",
    lat = mean(lat_range) + 0.35,
    lon = mean(lon_range) - 0.45
  )

  n_batch <- 65
  nursery_batches <- tibble::tibble(
    batch_id = sprintf("NB-%04d", 1:n_batch),
    species_id = sample(tree_species$species_id, n_batch, replace = TRUE),
    seed_source = sample(c("Wild Collection", "Mother Tree Garden", "Community Donation"),
                          n_batch, replace = TRUE, prob = c(0.55, 0.3, 0.15)),
    sow_date = study_start + sample(0:as.integer(today - study_start - 30), n_batch, replace = TRUE),
    quantity_sown = sample(300:2200, n_batch, replace = TRUE)
  ) |>
    dplyr::mutate(
      germination_rate = round(pmin(0.97, rbeta(n_batch, 6, 2)), 2),
      germinated_count = round(quantity_sown * germination_rate),
      age_days = as.integer(today - sow_date),
      ready_count = ifelse(age_days > 150, round(germinated_count * runif(n_batch, 0.75, 0.95)), 0L),
      status = dplyr::case_when(
        age_days < 30 ~ "Germinating",
        age_days < 150 ~ "Growing",
        age_days < 240 ~ "Ready for Planting",
        TRUE ~ "Dispatched"
      )
    )

  # ---- Nursery dispatches to restoration sites -----------------------------------
  dispatch_eligible <- dplyr::filter(nursery_batches, status %in% c("Ready for Planting", "Dispatched"))
  n_dispatch <- nrow(dispatch_eligible)
  nursery_dispatches <- dispatch_eligible |>
    dplyr::mutate(
      dispatch_id = sprintf("ND-%04d", dplyr::row_number()),
      site_id = sample(restoration_sites$site_id, n_dispatch, replace = TRUE),
      quantity = pmin(ready_count, round(ready_count * runif(n_dispatch, 0.4, 1)))
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(dispatch_date = pmin(sow_date + sample(150:300, 1), today)) |>
    dplyr::ungroup() |>
    dplyr::filter(quantity > 0) |>
    dplyr::select(dispatch_id, batch_id, species_id, dispatch_date, site_id, quantity)

  nursery_batches <- nursery_batches |>
    dplyr::select(batch_id, species_id, seed_source, sow_date, quantity_sown,
                   germination_rate, germinated_count, ready_count, status)

  # ---- Carbon modeling assumptions -----------------------------------------
  # All placeholder figures for a first-pass, illustrative dashboard - swap
  # for a validated allometric model (ARR) and a registry-approved Reference
  # Emission Level (REDD+) once field/remote-sensing data is available.
  carbon_params <- list(
    arr_asymptote_tco2_ha = 180,      # long-run avg across the planted species mix, tCO2e/ha
    arr_k = 0.09,                     # Chapman-Richards growth-rate constant
    arr_p = 2.2,                      # Chapman-Richards shape parameter
    arr_horizon_years = 30,           # typical ARR crediting/monitoring horizon
    protected_forest_area_ha = 38000, # standing forest under active protection
    forest_carbon_density_tco2_ha = 320, # mature tropical forest carbon stock, tCO2e/ha
    baseline_deforestation_rate = 0.016, # business-as-usual annual loss rate
    actual_deforestation_rate = 0.0025,  # observed rate under active patrol protection
    redd_horizon_years = 10,          # typical REDD+ crediting period
    car_tco2_per_year = 4.6           # avg passenger car emissions, tCO2e/yr (for relatable KPIs)
  )

  # ---- Third-party verification (placeholder MRV cycle) ---------------------
  # Independent verification always lags the live model (last audit covers a
  # past monitoring period) and typically comes in below the raw model
  # estimate once conservativeness/buffer deductions are applied. Both
  # dynamics are simulated here so the dashboard can visibly distinguish
  # "projected" (modeled, continuously updated) from "verified" (audited,
  # point-in-time, lower) figures - replace with real registry audit data
  # once available.
  verification_date <- today - 240
  verification_year <- max(as.numeric(difftime(verification_date, study_start, units = "days")) / 365.25, 0.5)

  arr_modeled_at_verification <- chapman_richards(
    verification_year, carbon_params$arr_asymptote_tco2_ha, carbon_params$arr_k, carbon_params$arr_p
  ) * sum(restoration_sites$area_ha)

  redd_baseline_at_verification <- carbon_params$protected_forest_area_ha *
    (1 - (1 - carbon_params$baseline_deforestation_rate)^verification_year)
  redd_actual_at_verification <- carbon_params$protected_forest_area_ha *
    (1 - (1 - carbon_params$actual_deforestation_rate)^verification_year)
  redd_modeled_at_verification <- (redd_baseline_at_verification - redd_actual_at_verification) *
    carbon_params$forest_carbon_density_tco2_ha

  carbon_verification <- tibble::tribble(
    ~mechanism, ~verification_date, ~verification_year, ~modeled_tco2e_at_verification, ~verified_tco2e, ~verifier,
    "ARR",    verification_date, verification_year, round(arr_modeled_at_verification),
    round(arr_modeled_at_verification * runif(1, 0.65, 0.8)), "Independent third-party verifier",
    "REDD++", verification_date, verification_year, round(redd_modeled_at_verification),
    round(redd_modeled_at_verification * runif(1, 0.65, 0.8)), "Independent third-party verifier"
  )

  list(
    species = species,
    conservation_status_ref = conservation_status_ref,
    sessions = sessions,
    acoustic_sites = acoustic_sites,
    acoustic_detections = acoustic_detections,
    transect_routes = transect_routes,
    transect_surveys = transect_surveys,
    transect_observations = transect_observations,
    tree_species = tree_species,
    restoration_sites = restoration_sites,
    planting_events = planting_events,
    survival_monitoring = survival_monitoring,
    nursery_facility = nursery_facility,
    nursery_batches = nursery_batches,
    nursery_dispatches = nursery_dispatches,
    carbon_params = carbon_params,
    carbon_verification = carbon_verification,
    study_start = study_start,
    today = today
  )
}
