# Internal map helpers used across multiple Shiny Modules

# Calculate marker radius from current zoom level
calculateMarkerSize <- function(zoom) {
  base_size <- 2
  adjusted_size <- base_size * (1.2^(zoom - 10))
  min(max(adjusted_size, 1), 15)
}

# Add standard base map tile layers to a leaflet map
#
# When a CARTO API key is provided, the Positron basemap is loaded from
# CARTO's raster tile service with the key appended. When no key is
# available, the free Esri World Gray Canvas basemap is used as a
# visual fallback. It requires no API key and provides a similar light
# gray aesthetic suitable as a background for transit data overlays.
#
# Free CARTO API keys (5 M tiles/month) can be requested at
# https://carto.com/basemaps/apikey/
#
# @param map A leaflet map object.
# @param carto_key Character string. A CARTO basemap API key. When
#   empty or NULL, falls back to Esri.WorldGrayCanvas.
# @param position Character string. Position of the layers control
#   widget on the map (default "topright").
# @return The map object with basemap tiles and layer control added.
addBaseMaps <- function(map, carto_key = "", position = "topright") {
  use_carto <- !is.null(carto_key) && nzchar(carto_key)

  if (use_carto) {
    # ------ CARTO Positron with API key ------
    positron_url <- paste0(
      "https://{s}.basemaps.cartocdn.com/light_all/",
      "{z}/{x}/{y}{r}.png",
      "?key=",
      carto_key
    )
    map <- map |>
      leaflet::addTiles(
        urlTemplate = positron_url,
        attribution = paste0(
          '&copy; <a href="https://www.openstreetmap.org/copyright">',
          "OpenStreetMap</a>, ",
          '&copy; <a href="https://carto.com/attributions">CARTO</a>'
        ),
        group = "Positron",
        options = leaflet::tileOptions(subdomains = "abcd", maxZoom = 20)
      )
  } else {
    # ------ Free fallback (no API key required) ------
    map <- map |>
      leaflet::addProviderTiles("Esri.WorldGrayCanvas", group = "Positron")
  }

  map |>
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
    leaflet::addProviderTiles("OpenStreetMap.HOT", group = "OSM") |>
    leaflet::addLayersControl(
      baseGroups = c("Positron", "Satellite", "OSM"),
      position = position,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
}

# Calculate click-proximity threshold (in degrees) based on zoom level.
# Converts a fixed screen-pixel tolerance to geographic degrees so the
# threshold matches visual proximity at every zoom level.
calculateThreshold <- function(zoom, pixels = 10) {
  degrees_per_pixel <- 360 / (256 * 2^zoom)
  pixels * degrees_per_pixel
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
