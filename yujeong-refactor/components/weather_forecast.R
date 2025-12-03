library(httr)
library(jsonlite)
library(dplyr)

# Function to get combined Current + Forecast weather data
get_weather_forecast <- function(api_key, lat, lon, n_forecast = 5) {
  
  # ==========================================
  # 1. FETCH CURRENT CONDITIONS (Hour 0)
  # ==========================================
  
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
    warning(paste("Failed to fetch Current Conditions. Status:", httr::status_code(current_resp)))
  }
  
  # ==========================================
  # 2. FETCH HOURLY FORECAST (Hour 1 to N)
  # ==========================================
  
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
    warning(paste("Failed to fetch Hourly Forecast. Status:", httr::status_code(forecast_resp)))
  }
  
  # ==========================================
  # 3. COMBINE AND RETURN
  # ==========================================
  
  # Bind Current (Hour 0) + Forecast (Hour 1..N)
  final_df <- rbind(row_current, forecast_rows)
  
  return(final_df)
}

# Example Usage:
# api_key <- "AIzaSyCkp9eNSjWSoLJ_s0NX61yg21lcwCAaD8Q"
# result <- get_weather_forecast(api_key, 51.494094, -0.174138, n_forecast = 5)
# print(result)