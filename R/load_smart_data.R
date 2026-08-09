# ---- Real SMART patrol data loader -----------------------------------------
# Loads data/smart_data.RData (CRP = patrol tracks, CAP = field observations,
# DAVAIL = data-availability summary) and reshapes it into the tables
# mod_smart_patrol.R (and the small set of downstream consumers - mod_summary
# .R's overview map, mod_carbon_redd.R's threat-interception chart) expect.
#
# The source data is a real SMART export from the Kerinci-Seblat landscape
# (Jambi, Sumatra) with Indonesian-language category fields. Top-level
# taxonomy fields (Kategori_observasi, Kategori_temuan, Pelanggaran, follow-up
# status) are translated to English for UI consistency with the rest of the
# app; fine-grained free-text fields (Tipe.temuan, Keterangan/notes,
# Objective) are left in the original Indonesian since they're detail/context
# fields, not aggregated in charts.

# Top-level observation category (drives which tab a record belongs to).
smart_category_map <- c(
  "Aktivitas Manusia" = "Human Activity",
  "Bencana Alam" = "Natural Disaster",
  "Fitur" = "Natural Feature",
  "Posisi" = "Position/Waypoint",
  "Satwa Liar" = "Wildlife",
  "Survey Kehati" = "Biodiversity Survey",
  "Tumbuhan" = "Flora"
)

# Finding sub-type - the detailed breakdown within Wildlife/Human Activity.
smart_finding_map <- c(
  "Camera Trapping" = "Camera Trapping",
  "Fitur Alami" = "Natural Feature",
  "Infrastruktur/ Sarana dan Prasarana" = "Infrastructure/Facilities",
  "Kebakaran Hutan dan Lahan" = "Forest & Land Fire",
  "Pembalakan" = "Illegal Logging",
  "Pembuatan Akses Jalan" = "Road Access Construction",
  "Penambangan dan Pengeboran" = "Mining & Drilling",
  "Pencemaran" = "Pollution",
  "Pengambilan HHBK" = "NTFP Extraction",
  "Penggunaan Kawasan" = "Encroachment",
  "Perburuan Satwa" = "Poaching",
  "Perjumpaan Satwa" = "Wildlife Encounter",
  "Satwa Mati" = "Dead Wildlife",
  "Tanda Satwa" = "Wildlife Sign"
)

smart_violation_map <- c("Ya" = "Yes", "Bukan" = "No")

smart_followup_map <- c(
  "Masih proses" = "In Progress",
  "Terselesaikan" = "Resolved",
  "Tindak lanjut lainnya" = "Other Follow-up"
)

smart_team_id_map <- c("Gabungan" = "TM-GAB", "KPH" = "TM-KPH")

# Authoritative taxonomy/status reference (data/taxon_reference.csv) - joined
# onto observations by scientific name. Matched on a normalized (trimmed,
# lowercased) key so trivial case/whitespace differences don't cause a miss,
# but otherwise an exact match - species not present in the reference are
# left unassigned (no class/order/family/status/protected), never
# backfilled from a guess.
load_taxon_reference <- function(path = "data/taxon_reference.csv") {
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  df |>
    dplyr::transmute(
      match_key = tolower(trimws(Species)),
      class = Class,
      order = Order,
      family = Family,
      common_name_en = Common.name,
      conservation_status = dplyr::na_if(trimws(Status), "Not found"),
      protected = dplyr::coalesce(trimws(Protected) == "Y", FALSE)
    ) |>
    dplyr::filter(match_key != "") |>
    dplyr::distinct(match_key, .keep_all = TRUE)
}

# `%||%`-style helper: translate via a named lookup, falling back to NA for
# blank/unmapped input rather than leaving the raw (often Indonesian, often
# blank-string) value in place.
translate_field <- function(x, map) {
  x <- trimws(x)
  out <- unname(map[x])
  out[x == "" | is.na(x)] <- NA_character_
  out
}

load_smart_patrol_data <- function(path = "data/smart_data.RData",
                                     taxon_path = "data/taxon_reference.csv") {
  e <- new.env()
  load(path, envir = e)
  CRP <- e$CRP
  CAP <- e$CAP
  DAVAIL <- e$DAVAIL
  datEff <- e$datEff
  taxon_ref <- load_taxon_reference(taxon_path)

  # ---- Patrol effort (for per-km sighting/finding rates) --------------------
  # One row per patrol (no duplicate Patrol_IDs, no NAs, covers every
  # Patrol_ID that appears in CAP) - the authoritative effort figure to join
  # observations against when computing effort-normalized rates, rather than
  # a raw sighting/finding count.
  patrol_effort <- datEff |>
    dplyr::transmute(patrol_id = Patrol_ID, effort_km = as.numeric(effort) / 1000)

  # ---- Teams --------------------------------------------------------------
  patrol_teams <- tibble::tibble(
    team_id = c("TM-GAB", "TM-KPH", "TM-UNK"),
    team_name = c("Gabungan (Joint Team)", "KPH Team", "Unknown Team")
  )

  # ---- Patrol tracks (sf) --------------------------------------------------
  patrol_tracks <- CRP |>
    dplyr::mutate(
      patrol_id = Patrol_ID,
      team_id = dplyr::coalesce(unname(smart_team_id_map[Team]), "TM-UNK"),
      team_raw = dplyr::na_if(trimws(Team), ""),
      start_date = as.Date(Patrol_Sta, format = "%b %d, %Y"),
      end_date = as.Date(Patrol_End, format = "%b %d, %Y"),
      distance_km = round(as.numeric(Jarak) / 1000, 2),
      duration_days = pmax(1, as.numeric(end_date - start_date) + 1),
      transport = Patrol_Tra,
      station = Station,
      landscape = Landscape,
      mandate = dplyr::na_if(trimws(Mandate), ""),
      leader = dplyr::na_if(trimws(Leader), ""),
      has_track = !sf::st_is_empty(geometry)
    ) |>
    dplyr::select(patrol_id, team_id, team_raw, type = Type, start_date, end_date, duration_days,
                   station, landscape, transport, mandate, leader, distance_km, has_track, geometry)

  patrols <- sf::st_drop_geometry(patrol_tracks)

  # ---- Observations ---------------------------------------------------------
  patrol_observations <- CAP |>
    dplyr::mutate(
      obs_id = sprintf("OBS-%05d", dplyr::row_number()),
      patrol_id = Patrol_ID,
      team_id = dplyr::coalesce(unname(smart_team_id_map[Team]), "TM-UNK"),
      datetime = Tanggal,
      lat = Y, lon = X,
      category = translate_field(Kategori_observasi, smart_category_map),
      finding_type = translate_field(Kategori_temuan, smart_finding_map),
      finding_detail = dplyr::na_if(trimws(Tipe.temuan), ""),
      species_common = dplyr::na_if(trimws(Jenis.satwa), ""),
      scientific_name = dplyr::na_if(trimws(Scientific.Name), ""),
      count = Jumlah,
      violation = translate_field(Pelanggaran, smart_violation_map),
      follow_up_status = translate_field(Status.tindak.lanjut, smart_followup_map),
      notes = dplyr::na_if(trimws(Keterangan), ""),
      station = Station,
      match_key = tolower(trimws(dplyr::coalesce(scientific_name, "")))
    ) |>
    dplyr::left_join(taxon_ref, by = "match_key") |>
    # A record that names a species but has no taxon_reference match is
    # excluded entirely (not just left unassigned) - e.g. genus-level IDs
    # like "Muntiacus spp" or species absent from the reference. Records
    # with no species claim at all (most Human Activity/position rows)
    # are untouched.
    dplyr::filter(is.na(scientific_name) | !is.na(class)) |>
    dplyr::select(obs_id, patrol_id, team_id, datetime, lat, lon, category, finding_type,
                   finding_detail, species_common, scientific_name, common_name_en,
                   class, order, family, conservation_status, protected,
                   count, violation, follow_up_status, notes, station)

  meta <- list(
    landscape = DAVAIL$Landscape[1],
    total_patrols = DAVAIL$`Total Patrols`[1],
    start_date = DAVAIL$`Start Date`[1],
    end_date = DAVAIL$`End Date`[1],
    patrol_days = DAVAIL$`Patrol Days`[1],
    pic = DAVAIL$PIC[1]
  )

  list(
    patrol_teams = patrol_teams,
    patrols = patrols,
    patrol_tracks = patrol_tracks,
    patrol_observations = patrol_observations,
    patrol_effort = patrol_effort,
    smart_meta = meta
  )
}
