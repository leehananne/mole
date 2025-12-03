library(httr)
library(jsonlite)
library(dplyr)

# ==========================================
# HELPER: FETCH BANDS FOR A SPECIFIC DAY
# ==========================================
fetch_tfl_bands <- function(naptan_code, day_abbr) {
  base_url <- paste0("https://api.tfl.gov.uk/crowding/", naptan_code, "/", day_abbr)
  
  response <- tryCatch(
    httr::GET(base_url, httr::timeout(15)),
    error = function(e) return(NULL)
  )
  
  if (is.null(response) || httr::status_code(response) != 200) {
    return(NULL)
  }
  
  content_text <- httr::content(response, "text", encoding = "UTF-8")
  json_data <- jsonlite::fromJSON(content_text, flatten = TRUE)
  
  if (is.null(json_data$timeBands)) return(NULL)
  
  return(json_data$timeBands)
}

# ==========================================
# MAIN FUNCTION
# ==========================================
get_crowd_forecast <- function(naptan_code, n_hours = 6) {
  
  # 1. TIME CONTEXT
  current_time <- Sys.time()
  current_day_abbr <- format(current_time, "%a") # e.g., "Fri"
  start_hour <- as.numeric(format(current_time, "%H"))
  
  message(paste("Fetching crowding data for:", naptan_code, "| Start:", current_day_abbr, start_hour, ":00"))
  
  # 2. DAY MAPPING LOGIC
  # TfL uses: Mon, Tue, Wed, Thu, Fri, Sat, Sun
  days_map <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  
  # Find index of current day
  curr_idx <- match(current_day_abbr, days_map)
  
  # Calculate index of next day (Wrap Sun(7) -> Mon(1))
  next_idx <- ifelse(curr_idx == 7, 1, curr_idx + 1)
  next_day_abbr <- days_map[next_idx]
  
  # 3. FETCH DATA (CURRENT DAY)
  bands_current <- fetch_tfl_bands(naptan_code, current_day_abbr)
  
  # 4. FETCH DATA (NEXT DAY) - Only if needed
  # We check if our window extends past 24
  bands_next <- NULL
  if ((start_hour + n_hours) > 24) {
    message(paste("Forecast crosses midnight. Fetching next day:", next_day_abbr))
    bands_next <- fetch_tfl_bands(naptan_code, next_day_abbr)
  }
  
  # 5. PROCESS LOOP
  forecast_results <- data.frame(
    Hour = integer(),
    CrowdingScore = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in 0:(n_hours - 1)) {
    
    # Calculate the continuous hour (e.g., 23, 24, 25...)
    abs_target_hour <- start_hour + i
    
    # Determine actual hour of day (0-23) and which dataset to use
    if (abs_target_hour < 24) {
      # Case A: Current Day
      hour_of_day <- abs_target_hour
      source_bands <- bands_current
    } else {
      # Case B: Next Day (e.g., 25 -> 01:00)
      hour_of_day <- abs_target_hour - 24
      source_bands <- bands_next
    }
    
    # Skip if we don't have data for this specific day/hour
    if (is.null(source_bands)) {
      next
    }
    
    # Calculate Indices for 15-min bands (1-based index)
    # Band 1: XX:00, Band 2: XX:15 ...
    start_index <- (hour_of_day * 4) + 1
    end_index   <- start_index + 3
    
    # Safety check for index bounds
    if (end_index > nrow(source_bands)) {
      next
    }
    
    # Slice the 4 bands
    hourly_chunk <- source_bands[start_index:end_index, ]
    
    # Average
    avg_crowd <- mean(hourly_chunk$percentageOfBaseLine, na.rm = TRUE)
    
    # Add to results
    # We keep 'Hour' as the relative index (0, 1, 2...) for easier plotting
    forecast_results <- rbind(forecast_results, data.frame(
      Hour = hour_of_day,
      CrowdingScore = avg_crowd
    ))
  }
  
  return(forecast_results)
}

# Example Usage:
result <- get_crowd_forecast("940GZZLUSKS", n_hours = 8)
print(result)