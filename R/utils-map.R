# Internal map helpers used across multiple Shiny Modules

# Calculate marker radius from current zoom level
calculateMarkerSize <- function(zoom) {
  base_size <- 2
  adjusted_size <- base_size * (1.2^(zoom - 10))
  min(max(adjusted_size, 1), 15)
}

# Add standard base map tile layers to a leaflet map
addBaseMaps <- function(map) {
  map |>
    leaflet::addProviderTiles("CartoDB.Positron", group = "Positron") |>
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
    leaflet::addProviderTiles("OpenStreetMap.HOT", group = "OSM") |>
    leaflet::addLayersControl(
      baseGroups = c("Positron", "Satellite", "OSM"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
}

# Calculate click-proximity threshold (in degrees) based on zoom level
calculateThreshold <- function(zoom) {
  base_threshold <- 0.02
  adjusted_threshold <- base_threshold * (2^(10 - zoom))
  min(max(adjusted_threshold, 0.0001), 0.01)
}
