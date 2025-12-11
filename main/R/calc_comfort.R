# ==============================================================================
# UTILITY FOR COMFORT CALCULATIONS
#   fetch_tfl_bands
#   get_hourly_crowd_forecast
#   get_15min_crowd_forecast
#   get_weather_forecast
#   calculate_comfort_index
# ==============================================================================

# Function: Fetch TfL API
# ------------------------------------------------------------------------------
# Arguments:
#   naptan_code: Naptan code of the station
#   day_abbr: Abbreviation used for days (e.g. mon, tue, wed)

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


# Function: Get Crowd Forecast - Hourly
# ------------------------------------------------------------------------------
# Arguments:
#   naptan_code: Naptan code of the station
#   n_hours: Crowd forecast timeframe

get_hourly_crowd_forecast <- function(naptan_code, n_hours = 4) {
  
  current_time <- Sys.time()
  current_day_abbr <- format(current_time, "%a")
  start_hour <- as.numeric(format(current_time, "%H"))
  
  # Days mapping and wrapping for midnight
  days_map <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  curr_idx <- match(current_day_abbr, days_map)
  next_idx <- ifelse(curr_idx == 7, 1, curr_idx + 1)
  next_day_abbr <- days_map[next_idx]
  
  bands_current <- fetch_tfl_bands(naptan_code, current_day_abbr)
  
  if (length(bands_current)==0) {
    # Generate sequence of hours
    hours_seq <- (start_hour + 0:(n_hours - 1)) %% 24
    
    return(data.frame(
      Hour = 1:n_hours,
      TimeDisplay = sprintf("%02d:00", hours_seq),
      CrowdingScore = rep(0.4, n_hours)
    ))
  }
  
  # Fetch next day data if our window extends past 24
  bands_next <- NULL
  if ((start_hour + n_hours) > 24) {
    message(paste("Forecast crosses midnight. Fetching next day:", next_day_abbr))
    bands_next <- fetch_tfl_bands(naptan_code, next_day_abbr)
  }
  
  forecast_results <- data.frame(Hour = integer(), CrowdingScore = numeric())
  
  for (i in 0:(n_hours - 1)) {
    abs_target_hour <- start_hour + i
    
    if (abs_target_hour < 24) {
      hour_of_day <- abs_target_hour
      source_bands <- bands_current
    } else {
      hour_of_day <- abs_target_hour - 24
      source_bands <- bands_next
    }
    
    # Skip if we don't have data for this specific day/hour
    if (is.null(source_bands)) next
    
    # Calculate Indices for 15-min bands
    start_index <- (hour_of_day * 4) + 1
    end_index   <- start_index + 3
    
    # Safety check for index bounds
    if (end_index > nrow(source_bands)) next
    
    # Slice the 4 bands for hourly grouping
    hourly_chunk <- source_bands[start_index:end_index, ]
    avg_crowd <- mean(hourly_chunk$percentageOfBaseLine, na.rm = TRUE)
    
    # Add to results
    forecast_results <- rbind(forecast_results, data.frame(
      Hour = i + 1, # from 1 to N
      TimeDisplay = paste0(sprintf("%02d", hour_of_day), ":00"),
      CrowdingScore = avg_crowd
    ))
  }
  
  return(forecast_results)
}


# Function: Get Crowd Forecast - 15 Minute
# ------------------------------------------------------------------------------
# Arguments:
#   naptan_code: Naptan code of the station
#   n_hours: Crowd forecast timeframe

get_15min_crowd_forecast <- function(naptan_code, n_hours = 2) {
  
  current_time <- Sys.time()
  current_day_abbr <- format(current_time, "%a")
  current_hour <- as.numeric(format(current_time, "%H"))
  current_minute <- as.numeric(format(current_time, "%M"))
  
  # Start band in 15 minutes time frame
  start_band_index <- (current_hour * 4) + floor(current_minute / 15) + 1
  n_bands_needed <- n_hours * 4 
  
  bands_current <- fetch_tfl_bands(naptan_code, current_day_abbr)
  
  if (length(bands_current)==0) {
    # Generate time labels manually
    dummy_bands <- data.frame(TimeBand = character(), CrowdingScore = numeric())
    
    # Create a base time object aligned to the current 15-min block
    # Logic: Round down to nearest 15 min
    start_time_obj <- as.POSIXct(paste(Sys.Date(), paste0(current_hour, ":", start_band_index*15)), format="%Y-%m-%d %H:%M")
    
    for(i in 0:(n_bands_needed - 1)) {
      t_start <- start_time_obj + (i * 15 * 60) # add 15 mins in seconds
      t_end   <- t_start + (15 * 60)
      
      label <- paste0(format(t_start, "%H:%M"), " - ", format(t_end, "%H:%M"))
      
      dummy_bands <- rbind(dummy_bands, data.frame(
        TimeBand = label,
        CrowdingScore = 0.4
      ))
    }
    return(dummy_bands)
  }
  
  result_bands <- data.frame()
  
  # Loop to gather bands
  # We might need to cross midnight, so we handle indices > 96
  
  # Try to get bands from current day
  if (!is.null(bands_current)) {
    end_band_index_curr <- min(start_band_index + n_bands_needed - 1, 96)
    
    if (start_band_index <= 96) {
      chunk_curr <- bands_current[start_band_index:end_band_index_curr, ]
      # Add timestamp for plotting
      chunk_curr$time_label <- paste(chunk_curr$timeBand) 
      result_bands <- rbind(result_bands, chunk_curr)
    }
  }
  
  # Check if we need next day data
  bands_collected <- nrow(result_bands)
  bands_remaining <- n_bands_needed - bands_collected
  
  if (bands_remaining > 0) {
    # Determine next day name
    days_map <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    curr_idx <- match(current_day_abbr, days_map)
    next_idx <- ifelse(curr_idx == 7, 1, curr_idx + 1)
    next_day_abbr <- days_map[next_idx]
    
    bands_next <- fetch_tfl_bands(naptan_code, next_day_abbr)
    
    if (!is.null(bands_next)) {
      # We need bands 1 to bands_remaining
      chunk_next <- bands_next[1:bands_remaining, ]
      chunk_next$time_label <- paste(chunk_next$timeBand, "(Next Day)")
      result_bands <- rbind(result_bands, chunk_next)
    }
  }
  
  output <- data.frame(
    TimeBand = result_bands$timeBand,
    CrowdingScore = result_bands$percentageOfBaseLine,
    stringsAsFactors = FALSE
  )
  return(output)
}


# Function: Get Weather Forecast - Hourly
# ------------------------------------------------------------------------------
# Arguments:
#   api_key: Valid Google Maps API Key
#   lat, lon: Latitude and Longitude of the station location
#   n_forecast: Crowd forecast timeframe (typically, n-1)

get_weather_forecast <- function(api_key, lat, lon, n_forecast = 3) {
  
  base_url_current <- "https://weather.googleapis.com/v1/currentConditions:lookup"
  
  # Construct Query
  current_resp <- tryCatch(
    httr::GET(
      url = base_url_current,
      query = list(
        key = api_key,
        "location.latitude" = lat,
        "location.longitude" = lon
      ),
      httr::timeout(10)
    ),
    error = function(e) return(NULL)
  )
  
  # Initialize Row 0 (Current) with defaults
  row_current <- data.frame(
    Hour = 0,
    Temp = NA,
    PrecipProb = 0, # Current conditions often don't have probability, default to 0
    ConditionIcon = NA,
    stringsAsFactors = FALSE
  )
  
  # Process Current Response
  if (!is.null(current_resp) && httr::status_code(current_resp) == 200) {
    content_curr <- httr::content(current_resp, "text", encoding = "UTF-8")
    json_curr <- jsonlite::fromJSON(content_curr, flatten = TRUE)
    
    # Extract Data safely
    conds <- json_curr
    
    if (!is.null(conds)) {
      row_current$Temp <- conds$feelsLikeTemperature$degrees
      row_current$ConditionIcon <- conds$weatherCondition$iconBaseUri
      
      # Check if precipitation probability exists in current (rare), else keep 0
      if (!is.null(conds$precipitation$probability$percent)) {
        row_current$PrecipProb <- conds$precipitation$probability$percent
      }
    }
  } else {
    status_msg <- if (is.null(current_resp)) "Network Error/NULL" else httr::status_code(current_resp)
    warning(paste("Failed to fetch Current Conditions. Status:", status_msg))
  }
  
  base_url_forecast <- "https://weather.googleapis.com/v1/forecast/hours:lookup"
  
  forecast_resp <- tryCatch(
    httr::GET(
      url = base_url_forecast,
      query = list(
        key = api_key,
        "location.latitude" = lat,
        "location.longitude" = lon,
        "hours" = n_forecast # Request N hours
      ),
      httr::timeout(10)
    ),
    error = function(e) return(NULL)
  )
  
  forecast_rows <- data.frame()
  
  if (!is.null(forecast_resp) && httr::status_code(forecast_resp) == 200) {
    content_fc <- httr::content(forecast_resp, "text", encoding = "UTF-8")
    json_fc <- jsonlite::fromJSON(content_fc, flatten = TRUE)
    
    # --- FIX: Use 'forecastHours' instead of 'hours' ---
    hours_data <- json_fc$forecastHours
    
    if (!is.null(hours_data) && nrow(hours_data) > 0) {
      
      # Helper function to safely extract column or return NA
      safe_extract <- function(df, col_name) {
        if (col_name %in% names(df)) return(df[[col_name]])
        return(rep(NA, nrow(df)))
      }
      
      # Helper function to safely extract column or return 0
      safe_extract_0 <- function(df, col_name) {
        if (col_name %in% names(df)) return(df[[col_name]])
        return(rep(0, nrow(df)))
      }
      
      # Select and Rename Columns
      # using flatten=TRUE names from jsonlite
      forecast_rows <- data.frame(
        Hour = 1:nrow(hours_data), # Assign relative hours 1..N
        
        # safely extract in case some fields are missing in specific rows
        Temp = safe_extract(hours_data, "feelsLikeTemperature.degrees"),
        PrecipProb = safe_extract_0(hours_data, "precipitation.probability.percent"),
        ConditionIcon = safe_extract(hours_data, "weatherCondition.iconBaseUri"),
        
        stringsAsFactors = FALSE
      )
      
      # Handle potential NAs in Precip if API omits them for 0%
      forecast_rows$PrecipProb[is.na(forecast_rows$PrecipProb)] <- 0
    }
  } else {
    status_msg <- if (is.null(forecast_resp)) "Network Error/NULL" else httr::status_code(forecast_resp)
    warning(paste("Failed to fetch Hourly Forecast. Status:", status_msg))
  }
  
  # Combine Current (Hour 0) + Forecast (Hour 1..N)
  final_df <- rbind(row_current, forecast_rows)
  
  return(final_df)
}


# Function: Get Station Accessibility
# ------------------------------------------------------------------------------
# Arguments:
#   naptan_code: Naptan code of the station
#   station_df: Master station database with accessibility data

get_station_accessibility <- function(naptan_code, disruption=NULL, station_df) {
  
  # Check if disruption dataframe has data and contains status == 1
  if (!is.null(disruption) && nrow(disruption) > 0) {
    # Check if ANY row has status 1 (No Step Free Access)
    if (1 %in% disruption$status) {
      return(0) # Immediate Override: Station is inaccessible
    }
  }
  
  # Filter for the specific station
  station_acc_data <- station_df %>%
    filter(NaptanCode == naptan_code) %>%
    select(Lift, SameLevel, Interchange) %>%
    head(1)
  
  if (nrow(station_acc_data) == 0) return(0) # Default to 0 if not found
  
  # Ensure numeric
  lift_val <- as.numeric(station_acc_data$Lift)
  same_level <- station_acc_data$SameLevel
  interchange <- station_acc_data$Interchange
  
  # Scoring Logic
  score <- case_when(
    # Condition 1: Step-free access exists (Lift or Same Level)
    (lift_val > 0 | isTRUE(same_level) | same_level == "yes") ~ 10,
    
    # Condition 2: No step-free to street, but full interchange available
    (lift_val == 0 & interchange == 2) ~ 6,
    
    # Condition 3: No step-free to street, partial interchange available
    (lift_val == 0 & interchange == 1) ~ 3,
    
    # Default
    TRUE ~ 0
  )
  
  return(score)
}


# Function: Calculate Comfort Index
# ------------------------------------------------------------------------------
# Arguments: 
#   weather_df, crowd_df, access_score: forecast and accessibility data for selected station
#   disruption: status in delay relevant to the station
#   weights: weighting based on user profiles

calculate_comfort_index <- function(weather_df, crowd_df, access_score, disruption=NULL, weights) {
  
  w_t <- weights["w_weather"]
  w_c <- weights["w_crowd"]
  w_a <- weights["w_access"]

  # Ensure lengths match
  n_rows <- min(nrow(weather_df), nrow(crowd_df))
  if (n_rows == 0) return(NULL)
  
  df <- data.frame(
    Hour = 1:n_rows,
    Temp = weather_df$Temp[1:n_rows],
    PrecipProb = weather_df$PrecipProb[1:n_rows],
    ConditionIcon = weather_df$ConditionIcon[1:n_rows],
    Crowd_Ratio = crowd_df$CrowdingScore[1:n_rows],
    Access_Val = access_score
  )
  
  # --- Define Scoring Logic Helpers ---
  
  # 1. Thermal Score (Tc)
  # Ideal: 14-22C. Distance reduces score. Rain reduces score.
  calc_thermal <- function(temp, precip) {
    dist <- pmax(0, 14 - temp) + pmax(0, temp - 22)
    base <- pmax(0, pmin(10, 10 - dist))
    
    precip <- ifelse(is.na(precip), 0, precip)
    penalty <- 1 - (0.5 * (precip / 100))
    return(base * penalty)
  }
  
  # 2. Crowd Score (Cc)
  # Invert: 0.1 crowd -> 9.0 comfort
  calc_crowd <- function(ratio) {
    return((1 - ratio) * 10)
  }
  
  # Disruption Penalty
  # Check if disruption dataframe has data and contains status == 1
  if (!is.null(disruption) && nrow(disruption) > 0) {
    if (2 %in% disruption$status) {
      delay_penalty <- 0.3 # Severe Delay
    } else if (1 %in% disruption$status) {
      delay_penalty <- 0.6 # Minor Delay
    } else {
      delay_penalty <- 1
    }
  } else {
    delay_penalty <- 1 
  }
  
  # Calculate Columns
  df$Tc <- mapply(calc_thermal, df$Temp, df$PrecipProb)
  df$Cc <- calc_crowd(df$Crowd_Ratio)
  df$Ac <- df$Access_Val 
  
  # Final Index
  df$Comfort_Index <- delay_penalty * ((w_t * df$Tc) + (w_c * df$Cc) + (w_a * df$Ac))
  df$Comfort_Index <- round(df$Comfort_Index, 2)
  
  return(df)
}