#Internal helpers for stop generation in the stops module
#(activated in the Import / Export / Generate panel)

generate_stops_from_osm <- function(
  polygon_sf,
  current_stops,
  min_stop_dist = 200,
  batch_id = 1L,
  provider = "openstreetmap_fr"
) {
  polygon_sf <- sf::st_make_valid(polygon_sf)

  # Check: can we find an OSM extract for this polygon?
  oe_url <- tryCatch(
    osmextract::oe_match(polygon_sf, provider = provider, quiet = TRUE),
    error = function(e) NULL
  )

  if (is.null(oe_url) || is.null(oe_url$url)) {
    cli::cli_abort(c(
      "No OpenStreetMap extract found for this area.",
      "i" = "Provider {.val {provider}} does not cover the drawn zone.",
      "i" = "Try a different provider in Settings."
    ))
  }

  # Download and filter OSM network
  osm_network <- tryCatch(
    osmextract::oe_get(
      place = polygon_sf,
      boundary = polygon_sf,
      boundary_type = "clipsrc",
      layer = "lines",
      provider = provider,
      max_file_size = 2e+09,
      #2 GB file limit for download from oe_get
      extra_tags = "highway",
      query = paste0(
        "SELECT * FROM lines WHERE highway IN ",
        "('primary', 'secondary', 'tertiary', ",
        "'busway', 'trunk', 'residential')"
      ),
      quiet = TRUE
    ),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to download OSM data.",
        "x" = e$message
      ))
    }
  )

  if (is.null(osm_network) || nrow(osm_network) == 0) {
    return(NULL)
  }

  # Create intersection nodes from OSM network
  stop_nodes <- create_stop_nodes(osm_network)

  if (is.null(stop_nodes) || nrow(stop_nodes) == 0) {
    return(NULL)
  }

  # Add stops by class priority order
  class_priority <- c("primary", "secondary", "tertiary")
  accumulated_stops <- current_stops
  all_new_stops <- NULL

  for (cls in class_priority) {
    cls_raw <- stop_nodes |> filter(node_class == cls)
    if (nrow(cls_raw) == 0) {
      next
    }

    # Filter out candidates too close to any accumulated stop
    if (nrow(accumulated_stops) > 0) {
      buffer <- accumulated_stops |>
        summarise() |>
        sf::st_buffer(min_stop_dist)
      cls_elig <- cls_raw |>
        sf::st_filter(buffer, .predicate = sf::st_disjoint)
    } else {
      cls_elig <- cls_raw
    }

    # Deduplicate within this class
    cls_elig <- deduplicate_stops(cls_elig, min_stop_dist)
    if (nrow(cls_elig) == 0) {
      next
    }

    # Assign unique stop IDs using batch counter + timestamp
    time_id <- as.character(round(as.numeric(Sys.time()), 0))
    cls_stops <- cls_elig |>
      select(names, geometry) |>
      rename(stop_name = names) |>
      mutate(
        stop_id = str_c(
          "ID_",
          as.character(batch_id),
          "_",
          time_id,
          "_",
          row_number()
        ),
        .before = stop_name
      )

    accumulated_stops <- rbind(accumulated_stops, cls_stops)
    all_new_stops <- if (is.null(all_new_stops)) {
      cls_stops
    } else {
      rbind(all_new_stops, cls_stops)
    }

    batch_id <- batch_id + 1L
  }

  if (is.null(all_new_stops) || nrow(all_new_stops) == 0) {
    return(NULL)
  }

  list(
    new_stops = all_new_stops,
    next_batch_id = batch_id
  )
}

# Create stop nodes from osm network

create_stop_nodes <- function(osm_network) {
  nodes <- osm_network |>
    select(osm_id, name, highway, geometry) |>
    sf::st_cast("LINESTRING") |>
    sf::st_cast("POINT") |>
    mutate(
      coords = sf::st_coordinates(geometry),
      lat = round(coords[, "Y"], 4),
      lon = round(coords[, "X"], 4)
    ) |>
    as.data.frame() |>
    select(name, highway, lat, lon) |>
    group_by(lat, lon) |>
    summarise(
      n = dplyr::n_distinct(name),
      highway_types = list(sort(unique(highway))),
      names = list(sort(unique(name))),
      .groups = "drop"
    ) |>
    ungroup() |>
    filter(n > 1) |>
    mutate(
      node_class = vapply(
        highway_types,
        \(x) {
          primary_types <- c("primary", "secondary", "tertiary", "busway")
          if (all(x %in% primary_types)) {
            "primary"
          } else if (any(x %in% primary_types)) {
            "secondary"
          } else {
            "tertiary"
          }
        },
        character(1)
      )
    ) |>
    mutate(
      names = vapply(
        names,
        \(x) paste(x[!is.na(x)], collapse = " / "),
        character(1)
      )
    ) |>
    select(-c(n, highway_types)) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

  if (nrow(nodes) == 0) {
    return(NULL)
  }
  nodes
}

#Deduplicate stops within a minimum distance

deduplicate_stops <- function(stops_sf, min_dist) {
  if (nrow(stops_sf) <= 1) {
    return(stops_sf)
  }

  neighbors <- sf::st_is_within_distance(stops_sf, dist = min_dist)

  keep <- logical(nrow(stops_sf))
  available <- rep(TRUE, nrow(stops_sf))

  for (i in seq_len(nrow(stops_sf))) {
    if (!available[i]) {
      next
    }
    keep[i] <- TRUE
    too_close <- setdiff(neighbors[[i]], i)
    available[too_close] <- FALSE
  }

  stops_sf[keep, ]
}
