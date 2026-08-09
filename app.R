# Biodash - Biodiversity Monitoring Dashboard
#
# "Biodiversity" navbar dropdown with five sections: Summary, Camera Trap,
# Bioacoustics, SMART Patrol, Transect-based Survey. "Restoration" navbar
# dropdown with Summary, Restoration Effort, and Nursery. "Carbon Stock"
# navbar dropdown with Summary, ARR Potential, and REDD++ Potential.
# Runs on generated mock data (R/mock_data.R) for most sections; SMART Patrol
# and Camera Trap run on real field data (data/smart_data.RData via
# R/load_smart_data.R, data/camtrap_data.RData via R/load_camtrap_data.R) -
# swap the remaining generate_biodash_data() sections for real data loaders
# the same way as each is migrated. The species master list itself
# (data$species) is built from data/taxon_reference.csv, the authoritative
# taxonomy/status source for the whole app.

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(DT)
library(lubridate)
library(scales)
library(purrr)

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = FALSE)
}

biodash_data <- generate_biodash_data()

# KPHP VII (forest management unit) boundary, shown as a context layer on
# every map. Real GIS data (unlike the rest of biodash_data, which is mock) -
# already in WGS84 so no reprojection is needed for leaflet.
biodash_data$kphp_boundary <- tryCatch(
  sf::st_read("data/KPHP_VII.shp", quiet = TRUE),
  error = function(e) {
    warning("Could not load data/KPHP_VII.shp: ", conditionMessage(e))
    NULL
  }
)

# Real SMART patrol data (Kerinci-Seblat landscape) - replaces the mock
# patrol_teams/patrols/patrol_observations with real field data, and adds
# patrol_tracks (actual GPS routes) + smart_meta (data provenance).
# See R/load_smart_data.R for the CRP/CAP -> app schema translation.
smart <- load_smart_patrol_data("data/smart_data.RData")
biodash_data$patrol_teams <- smart$patrol_teams
biodash_data$patrols <- smart$patrols
biodash_data$patrol_tracks <- smart$patrol_tracks
biodash_data$patrol_observations <- smart$patrol_observations
biodash_data$patrol_effort <- smart$patrol_effort
biodash_data$smart_meta <- smart$smart_meta

# Real camera trap data (Kerinci-Seblat landscape) - replaces the mock
# camera_sites/camera_detections with real deployment/detection records.
# See R/load_camtrap_data.R for the detection/effort -> app schema translation.
camtrap <- load_camtrap_data("data/camtrap_data.RData")
biodash_data$camera_sites <- camtrap$camera_sites
biodash_data$camera_detections <- camtrap$camera_detections

ui <- page_navbar(
  title = "Biodash",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2c6e49"),
  fillable = FALSE,
  nav_menu(
    title = "Biodiversity",
    icon = icon("paw"),
    nav_panel("Summary", mod_summary_ui("summary")),
    nav_panel("Camera Trap", icon = icon("camera"), mod_camera_trap_ui("camera")),
    nav_panel("Bioacoustics", icon = icon("volume-up"), mod_bioacoustics_ui("acoustic")),
    nav_panel("SMART Patrol", icon = icon("route"), mod_smart_patrol_ui("patrol")),
    nav_panel("Transect-based Survey", icon = icon("road"), mod_transect_ui("transect"))
  ),
  nav_menu(
    title = "Restoration",
    icon = icon("tree"),
    nav_panel("Summary", mod_restoration_summary_ui("restoration_summary")),
    nav_panel("Restoration Effort", icon = icon("tree"), mod_restoration_ui("restoration")),
    nav_panel("Nursery", icon = icon("seedling"), mod_nursery_ui("nursery"))
  ),
  nav_menu(
    title = "Carbon Stock",
    icon = icon("cloud"),
    nav_panel("Summary", mod_carbon_summary_ui("carbon_summary")),
    nav_panel("ARR Potential", icon = icon("seedling"), mod_carbon_arr_ui("carbon_arr")),
    nav_panel("REDD++ Potential", icon = icon("shield-halved"), mod_carbon_redd_ui("carbon_redd"))
  ),
  nav_spacer(),
  nav_item(tags$span(class = "navbar-text text-light", "B-Forest"))
)

server <- function(input, output, session) {
  mod_summary_server("summary", data = biodash_data)
  mod_camera_trap_server("camera", data = biodash_data)
  mod_bioacoustics_server("acoustic", data = biodash_data)
  mod_smart_patrol_server("patrol", data = biodash_data)
  mod_transect_server("transect", data = biodash_data)
  mod_restoration_summary_server("restoration_summary", data = biodash_data)
  mod_restoration_server("restoration", data = biodash_data)
  mod_nursery_server("nursery", data = biodash_data)
  mod_carbon_summary_server("carbon_summary", data = biodash_data)
  mod_carbon_arr_server("carbon_arr", data = biodash_data)
  mod_carbon_redd_server("carbon_redd", data = biodash_data)
}

shinyApp(ui, server)
