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

routeLineWeight <- function(route_type) {
  switch(
    as.character(route_type),
    "1" = 5,
    "2" = 4,
    "0" = 3,
    2
  )
}

itineraryDrawOrder <- function(itin, routes) {
  route_type_lookup <- setNames(as.integer(routes$route_type), routes$route_id)
  route_name_lookup <- setNames(routes$route_short_name, routes$route_id)

  itin_route_types <- route_type_lookup[itin$route_id]
  itin_route_names <- route_name_lookup[itin$route_id]

  order(-itin_route_types, itin_route_names)
}
