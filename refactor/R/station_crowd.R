fetch_crowd_api <- function(naptan_code) {
  # Get current day and time
  current_time <- Sys.time()
  current_time <- as.POSIXct(current_time)
  current_day_abbr <- format(current_time, "%a")
  current_hour <- as.numeric(format(current_time, "%H"))
  current_minute <- as.numeric(format(current_time, "%M"))
  current_time_band_index <- floor((current_hour * 60 + current_minute) / 15)
  message("Current day: ", current_day_abbr, ", Hour: ", current_hour, ", Minute: ", current_minute, ", Time band index: ", current_time_band_index)
  
  # # Call API
  base_url <- paste0("https://api.tfl.gov.uk/crowding/", naptan_code, "/", current_day_abbr)
  
  # Make API request
  response <- httr::GET(base_url, httr::timeout(15))
  
  # Handle errors
  if (httr::status_code(response) == 400) {
    error_content <- httr::content(response, "text", encoding = "UTF-8")
    error_parsed <- tryCatch(
      jsonlite::fromJSON(error_content, flatten = TRUE),
      error = function(e) NULL
    )
    error_msg <- if (!is.null(error_parsed) && !is.null(error_parsed$message)) {
      error_parsed$message
    } else {
      error_content
    }
    stop(paste("Bad Request:", error_msg))
  }
  
  httr::stop_for_status(response)
  
  # Parse JSON response
  json_content <- httr::content(response, "text", encoding = "UTF-8")
  parsed_data <- jsonlite::fromJSON(json_content, flatten = TRUE)
  
  # Extract timeBands
  if (is.null(parsed_data$timeBands) || !is.data.frame(parsed_data$timeBands)) {
    message("No timeBands found in response")
    return(NULL)
  }
  
  time_bands <- parsed_data$timeBands
  
  # Convert each timeBand string (e.g., "14:45-15:00") to an index and find the matching one
  time_band_indices <- sapply(time_bands$timeBand, function(tb) {
    # Parse time range string like "14:45-15:00"
    start_time <- sub("-.*", "", tb)  # Get "14:45"
    time_parts <- strsplit(start_time, ":")[[1]]
    hours <- as.numeric(time_parts[1])
    minutes <- as.numeric(time_parts[2])
    # Convert to 15-min band index
    floor((hours * 60 + minutes) / 15)
  })
  
  # Find which timeBand matches the current time
  matching_idx <- which(time_band_indices == current_time_band_index)
  
  if (length(matching_idx) == 0) {
    message("No exact match found for current time band index ", current_time_band_index)
    return(NULL)
  }
  
  current_time_band <- time_bands$timeBand[matching_idx[1]]
  current_crowding_level <- time_bands$percentageOfBaseLine[matching_idx[1]]
  message("Current time falls into timeBand: ", current_time_band)
  message("Current crowding level: ", current_crowding_level)
  
  # Get next 2 time bands (15 mins and 30 mins after)
  next_time_band_index_4 <- current_time_band_index + 4  # 1 hour after
  next_time_band_index_8 <- current_time_band_index + 8  # 2 hours after
  
  # Find these time bands in the data
  next_idx_4 <- which(time_band_indices == next_time_band_index_4)
  next_idx_8 <- which(time_band_indices == next_time_band_index_8)
  
  # Get crowding levels for next time bands (if available)
  next_crowding_4 <- if (length(next_idx_4) > 0) time_bands$percentageOfBaseLine[next_idx_4[1]] else NA
  next_crowding_8 <- if (length(next_idx_8) > 0) time_bands$percentageOfBaseLine[next_idx_8[1]] else NA
  next_time_band_4 <- if (length(next_idx_4) > 0) time_bands$timeBand[next_idx_4[1]] else NA
  next_time_band_8 <- if (length(next_idx_8) > 0) time_bands$timeBand[next_idx_8[1]] else NA
  
  # Compare crowding levels and generate forecast message
  forecast_message <- NULL
  
  # Check if current is the lowest
  all_levels <- c(current_crowding_level, next_crowding_4, next_crowding_8)
  all_levels <- all_levels[!is.na(all_levels)]
  
  if (current_crowding_level == min(all_levels, na.rm = TRUE)) {
    forecast_message <- "Current crowd level is the lowest for the next 30 minutes."
  } else {
    current_crowding_level = current_crowding_level * 100
    next_crowding_4 = next_crowding_4 * 100
    next_crowding_8 = next_crowding_8 * 100
    
    # Check which future time band has lower crowding (prioritize 15 min if both are lower)
    if (!is.na(next_crowding_4) && next_crowding_4 < current_crowding_level) {
      reduction <- current_crowding_level - next_crowding_4
      forecast_message <- paste0("In the next 1 hour (", next_time_band_4, "), the crowd will reduce by ", 
                                round(reduction, 2), "% (from ", round(current_crowding_level, 2), 
                                "% to ", round(next_crowding_4, 2), "%).")
      # Also mention if 30 min is even better
      if (!is.na(next_crowding_8) && next_crowding_8 < next_crowding_4) {
        additional_reduction <- next_crowding_4 - next_crowding_8
        forecast_message <- paste0(forecast_message, " It will reduce further by ", 
                                  round(additional_reduction, 2), "% in 30 minutes (", next_time_band_8, ").")
      }
    } else if (!is.na(next_crowding_8) && next_crowding_8 < current_crowding_level) {
      reduction <- current_crowding_level - next_crowding_8
      forecast_message <- paste0("In the next 2 hours (", next_time_band_8, "), the crowd will reduce by ", 
                                round(reduction, 2), "% (from ", round(current_crowding_level, 2), 
                                "% to ", round(next_crowding_8, 2), "%).")
    } else {
      # Both future bands have higher or equal crowding
      forecast_message <- "Crowding is expected to stay the same or increase in the next 30 minutes."
    }
  }
  
  message("\n=== CROWDING FORECAST ===")
  message(forecast_message)
  message("========================\n")
  
  return(list(
    current_time_band = current_time_band,
    current_crowding_level = current_crowding_level,
    next_15min_time_band = next_time_band_4,
    next_15min_crowding = next_crowding_4,
    next_30min_time_band = next_time_band_8,
    next_30min_crowding = next_crowding_8,
    forecast_message = forecast_message
  ))
}


