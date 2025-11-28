library(httr)
library(jsonlite)
library(dplyr)

# Function to get aggregated hourly crowding forecast
get_crowd_forecast <- function(naptan_code, n_hours = 6) {
  
  # ==========================================
  # 1. DETERMINE TIME CONTEXT
  # ==========================================
  current_time <- Sys.time()
  
  # TfL API requires the Day of the Week (e.g., "Mon", "Fri")
  current_day_abbr <- format(current_time, "%a")
  
  # We need the current hour (0-23) to determine the starting block
  # If time is 13:20, current_hour is 13. We start fetching from 13:00.
  start_hour <- as.numeric(format(current_time, "%H"))
  
  message(paste("Fetching crowding data for:", naptan_code, "| Day:", current_day_abbr, "| Starting Hour:", start_hour))
  
  # ==========================================
  # 2. CALL TFL CROWDING API
  # ==========================================
  base_url <- paste0("https://api.tfl.gov.uk/crowding/", naptan_code, "/", current_day_abbr)
  
  response <- tryCatch(
    httr::GET(base_url, httr::timeout(15)),
    error = function(e) return(NULL)
  )
  
  # Basic Error Handling
  if (is.null(response) || httr::status_code(response) != 200) {
    warning("Failed to fetch crowding data. Returning default NULL.")
    return(NULL)
  }
  
  # Parse JSON
  content_text <- httr::content(response, "text", encoding = "UTF-8")
  json_data <- jsonlite::fromJSON(content_text, flatten = TRUE)
  
  # Extract the time bands list
  # The API returns 'timeBands' with 'percentageOfBaseLine'
  raw_bands <- json_data$timeBands
  
  if (is.null(raw_bands) || nrow(raw_bands) == 0) {
    warning("No time band data available in API response.")
    return(NULL)
  }
  
  # ==========================================
  # 3. PROCESS 15-MIN BANDS INTO HOURLY AVG
  # ==========================================
  
  forecast_results <- data.frame(
    Hour = integer(),
    CrowdingScore = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop for the next N hours
  for (i in 0:(n_hours - 1)) {
    
    target_hour <- start_hour + i
    
    # Stop if we exceed the current day (24-hour limit)
    # The API only returns data for the specific Day requested (e.g., "Fri")
    if (target_hour >= 24) {
      break 
    }
    
    # Calculate Indices for 15-min bands
    # Band 1: XX:00, Band 2: XX:15, Band 3: XX:30, Band 4: XX:45
    # R uses 1-based indexing.
    # Hour 0 (00:00) -> Indices 1, 2, 3, 4
    # Hour 1 (01:00) -> Indices 5, 6, 7, 8
    # Formula: (Hour * 4) + 1
    
    start_index <- (target_hour * 4) + 1
    end_index   <- start_index + 3
    
    # Ensure indices don't exceed available data
    if (end_index > nrow(raw_bands)) {
      break
    }
    
    # Slice the 4 bands for this hour
    hourly_chunk <- raw_bands[start_index:end_index, ]
    
    # Calculate Average Crowding (0.0 to 1.0 scale usually, API gives %)
    # API 'percentageOfBaseLine' is typically 0.0 to 1.0 (or sometimes >1 if very busy)
    avg_crowd <- mean(hourly_chunk$percentageOfBaseLine, na.rm = TRUE)
    
    # Add to results
    forecast_results <- rbind(forecast_results, data.frame(
      # Hour = i,
      Hour = target_hour, # if we want hour 
      CrowdingScore = avg_crowd
    ))
  }

  return(forecast_results)
}

# Example Usage (Uncomment to test):
result <- get_crowd_forecast("940GZZLUSKS", n_hours = 7)
print(result)