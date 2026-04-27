# --- /Helper functions for schedule module/----

sched_format_time <- function(time_str) {
  clean_str <- gsub("[^0-9:]", "", time_str)
  parts <- strsplit(clean_str, ":")[[1]]
  if (length(parts) == 1) {
    hours <- as.numeric(parts[1])
    mins <- 0
    secs <- 0
  } else if (length(parts) == 2) {
    hours <- as.numeric(parts[1])
    mins <- as.numeric(parts[2])
    secs <- 0
  } else if (length(parts) == 3) {
    hours <- as.numeric(parts[1])
    mins <- as.numeric(parts[2])
    secs <- as.numeric(parts[3])
  } else {
    return(NULL)
  }
  if (
    hours < 0 ||
      hours > 30 ||
      mins < 0 ||
      mins > 59 ||
      secs < 0 ||
      secs > 59
  ) {
    return(NULL)
  }
  sprintf("%02d:%02d:%02d", hours, mins, secs)
}

sched_get_speed_for_itin <- function(itin_id, current_data) {
  itin_row <- current_data$itin[current_data$itin$itin_id == itin_id, ]
  if (nrow(itin_row) == 0) {
    return(20)
  }
  route_id <- itin_row$route_id[1]
  route_row <- current_data$routes[
    current_data$routes$route_id == route_id,
  ]
  if (nrow(route_row) == 0) {
    return(20)
  }
  route_type <- route_row$route_type[1]
  if (!is.na(route_type) && route_type %in% c(1, 2, 12)) 40 else 20
}

sched_parse_time_to_seconds <- function(time_str) {
  parts <- as.numeric(strsplit(time_str, ":")[[1]])
  parts[1] * 3600 + parts[2] * 60 + parts[3]
}

sched_get_hours_for_span <- function(first_dep, last_dep) {
  first_hour <- as.numeric(substr(first_dep, 1, 2))
  last_hour <- as.numeric(substr(last_dep, 1, 2))
  sprintf("%02d:00:00", first_hour:last_hour)
}
