fetch_weather_data <- function(api_key, latitude, longitude, station_name = NULL) {
  if (is.null(api_key) || api_key == "" || is.na(latitude) || is.na(longitude)) return(NULL)
  base_url <- "https://weather.googleapis.com/v1/currentConditions:lookup"
  full_url <- modify_url(base_url, query = list(
    key = api_key,
    "location.latitude" = latitude,
    "location.longitude" = longitude
  ))
  tryCatch({
    if (!is.null(station_name)) {
      message("Fetching Google Weather for: ", station_name)
    }
    response <- httr::GET(full_url, timeout(10))
    stop_for_status(response)
    json_content <- httr::content(response, "text", encoding = "UTF-8")
    parsed_data <- jsonlite::fromJSON(json_content, flatten = TRUE)
    if (!is.null(station_name)) parsed_data$SelectedStationName <- station_name
    return(parsed_data)
  }, error = function(e) {
    message("!!! Error fetching Google Weather data: ", e$message)
    showNotification(paste("Error fetching weather:", e$message), type = "error")
    return(NULL)
  })
}


