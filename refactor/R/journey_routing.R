fetch_journey_route <- function(origin_naptan, destination_naptan) {
  if (is.null(origin_naptan) || origin_naptan == "" || 
      is.null(destination_naptan) || destination_naptan == "") {
    return(NULL)
  }
  
  # Construct the API URL with all the specified parameters
  base_url <- paste0("https://api.tfl.gov.uk/Journey/JourneyResults/", 
                     origin_naptan, "/to/", destination_naptan)
  
  # TFL API expects boolean values as strings "true"/"false" and some parameters as strings
  query_params <- list(
    journeyPreference = "LeastInterchange",
    mode = "public-tube",  
    accessibilityPreference = "NoRequirements",
    cyclePreference = "None",
    alternativeCycle = "false",
    alternativeWalking = "false",
    useMultiModalCall = "false",
    taxiOnlyTrip = "false",
    routeBetweenEntrances = "true",
    useRealTimeLiveArrivals = "true",
    calcOneDirection = "true",
    includeAlternativeRoutes = "false"
  )
  
  tryCatch({
    response <- httr::GET(base_url, query = query_params, httr::timeout(15))
    
    # Check status and get error details if 400
    status <- httr::status_code(response)
    if (status == 400) {
      error_content <- httr::content(response, "text", encoding = "UTF-8")
      error_parsed <- tryCatch(
        jsonlite::fromJSON(error_content, flatten = TRUE),
        error = function(e) NULL
      )
      if (!is.null(error_parsed) && !is.null(error_parsed$message)) {
        stop(paste("Bad Request:", error_parsed$message))
      } else {
        stop(paste("Bad Request:", error_content))
      }
    }
    
    httr::stop_for_status(response)
    json_content <- httr::content(response, "text", encoding = "UTF-8")
    parsed_data <- jsonlite::fromJSON(json_content, flatten = TRUE)
    return(parsed_data)
  }, error = function(e) {
    status_code <- NA
    if (exists("response") && !is.null(response)) {
      try(status_code <- httr::status_code(response), silent = TRUE)
    }
    # Don't call showNotification here - let the server handle it
    # Just return error details
    err_message <- e$message
    if (!is.na(status_code)) { 
      err_message <- paste("HTTP", status_code, "-", err_message)
    }
    message("Journey routing error: ", err_message)
    return(NULL)
  })
}

