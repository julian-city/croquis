# --- Helper functions for schedule module ----

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

#  normalize speed factors if average drifts
sched_sp_normalize <- function(values, threshold = 0.1) {
  avg <- mean(values)
  if (abs(avg - 1.0) > threshold) {
    normalized <- values / avg
    return(round(normalized, 1))
  }
  round(values, 1)
}

# ---------------------------------------------------------------------------
# Speed recalculator
# Adjusts speeds in the hsh table for a given itinerary, service, and hour
# range by modifying either speed directly or runtime (from which speed
# is re-derived).
# Returns the modified ssfs. Caller is responsible for assigning to the
# reactive value.
# ---------------------------------------------------------------------------
sched_speed_recalculator <- function(
  current_ssfs,
  selected_itin_id,
  selected_service,
  itin_len_km,
  operation = c("increase", "decrease"),
  target = c("speed", "runtime"),
  value,
  unit = c("percent", "raw"),
  start_hour,
  end_hour
) {
  operation <- match.arg(operation)
  target <- match.arg(target)
  unit <- match.arg(unit)

  # --- input validation ---------------------------------------------------
  if (!is.numeric(value) || length(value) != 1 || is.na(value) || value <= 0) {
    stop("value must be a single positive number", call. = FALSE)
  }
  if (unit == "percent" && value >= 100 && operation == "decrease") {
    stop("cannot decrease by 100% or more", call. = FALSE)
  }

  start_hour_num <- as.numeric(substr(start_hour, 1, 2))
  end_hour_num <- as.numeric(substr(end_hour, 1, 2))

  if (start_hour_num > end_hour_num) {
    stop("start hour must not be after end hour", call. = FALSE)
  }

  # --- select affected rows ------------------------------------------------
  match_idx <- which(
    current_ssfs$hsh$itin_id == selected_itin_id &
      current_ssfs$hsh$service_id == selected_service &
      as.numeric(substr(current_ssfs$hsh$hour_dep, 1, 2)) >= start_hour_num &
      as.numeric(substr(current_ssfs$hsh$hour_dep, 1, 2)) <= end_hour_num
  )

  if (length(match_idx) == 0) {
    return(current_ssfs)
  }

  old_speeds <- current_ssfs$hsh$speed[match_idx]

  # --- compute new speeds --------------------------------------------------
  sign <- if (operation == "increase") 1 else -1

  if (target == "speed") {
    if (unit == "raw") {
      new_speeds <- round(old_speeds + sign * value, 1)
    } else {
      new_speeds <- round(old_speeds * (1 + sign * value / 100), 1)
    }
  } else {
    # target == "runtime"
    # runtime (min) = (itin_len_km / speed) * 60
    old_runtimes <- (itin_len_km / old_speeds) * 60

    if (unit == "raw") {
      new_runtimes <- old_runtimes + sign * value
    } else {
      new_runtimes <- old_runtimes * (1 + sign * value / 100)
    }

    # guard against non-positive runtimes
    if (any(new_runtimes <= 0, na.rm = TRUE)) {
      stop("resulting runtime would be zero or negative", call. = FALSE)
    }

    new_speeds <- round((itin_len_km / (new_runtimes / 60)), 1)
  }

  # --- guard against non-positive speeds -----------------------------------
  if (any(new_speeds <= 0, na.rm = TRUE)) {
    stop("resulting speed would be zero or negative", call. = FALSE)
  }

  if (any(new_speeds > 431, na.rm = TRUE)) {
    stop("resulting speed exceeds 431 km/h", call. = FALSE)
  }

  current_ssfs$hsh$speed[match_idx] <- new_speeds
  current_ssfs
}
