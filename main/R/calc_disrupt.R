# ==============================================================================
# calc_disrupt.R
# Calculation utility to fetch and interpret TfL disruption data
# ==============================================================================

# FETCH DATA
# Fetches raw disruption descriptions for a given Naptan code
# ------------------------------------------------------------------------------
fetch_disruption_data <- function(naptan_code) {
  
  # Validate input
  if (is.null(naptan_code) || naptan_code == "") {
    return(character(0))
  }
  
  # Construct URL
  url <- paste0("https://api.tfl.gov.uk/StopPoint/", naptan_code, "/Disruption")
  
  # Make API call
  response <- tryCatch({
    httr::GET(url, httr::timeout(10))
  }, error = function(e) NULL)
  
  # Check for valid response
  if (is.null(response) || httr::status_code(response) != 200) {
    return(character(0))
  }
  
  # Parse JSON
  content_text <- httr::content(response, "text", encoding = "UTF-8")
  
  # specific check for empty response bodies which can happen
  if (nchar(content_text) == 0) {
    return(character(0))
  }
  
  data <- tryCatch({
    jsonlite::fromJSON(content_text, flatten = TRUE)
  }, error = function(e) NULL)
  
  # Extract descriptions
  # The API returns a list of objects, usually flattened into a dataframe by jsonlite
  if (!is.null(data) && "description" %in% names(data)) {
    return(as.character(data$description))
  } else {
    return(character(0))
  }
}


# INTERPRET DELAY
# Returns dataframe [status, message] for Minor (1) or Severe (2) delays
# ------------------------------------------------------------------------------
interpret_delay <- function(naptan_code) {
  
  # 1. Fetch raw descriptions
  descriptions <- fetch_disruption_data(naptan_code)
  
  # Initialize vectors to store results
  status_list <- c()
  message_list <- c()
  
  # 2. Loop through descriptions and categorize
  if (!is.null(descriptions) && length(descriptions) > 0) {
    for (desc in descriptions) {
      
      # Check for Minor Delays
      if (grepl("Minor Delays", desc, ignore.case = TRUE)) {
        status_list <- c(status_list, 1)
        message_list <- c(message_list, desc)
      }
      
      # Check for Severe Delays
      # Note: using separate if statements to catch cases where both might appear 
      # (though rare in one string, safer to process individually)
      if (grepl("Severe Delays", desc, ignore.case = TRUE)) {
        status_list <- c(status_list, 2)
        message_list <- c(message_list, desc)
      }
    }
  }
  
  # 3. Create Final Dataframe
  if (length(status_list) > 0) {
    result_df <- data.frame(
      status = status_list,
      message = message_list,
      stringsAsFactors = FALSE
    )
  } else {
    # Return empty dataframe structure if no relevant delays found
    result_df <- data.frame(
      status = numeric(0),
      message = character(0),
      stringsAsFactors = FALSE
    )
  }
  
  return(result_df)
}


# INTERPRET ACCESS
# Returns dataframe [status, message] for No Step Free Access (1)
# ------------------------------------------------------------------------------
interpret_access <- function(naptan_code) {
  
  # 1. Fetch raw descriptions
  descriptions <- fetch_disruption_data(naptan_code)
  
  # Initialize vectors
  status_list <- c()
  message_list <- c()
  
  # 2. Process descriptions
  if (!is.null(descriptions) && length(descriptions) > 0) {
    for (desc in descriptions) {
      
      # Check for Step Free Access issues
      if (grepl("No Step Free Access", desc, ignore.case = TRUE)) {
        status_list <- c(status_list, 1)
        message_list <- c(message_list, desc)
      }
    }
  }
  
  # 3. Create Final Dataframe
  if (length(status_list) > 0) {
    result_df <- data.frame(
      status = status_list,
      message = message_list,
      stringsAsFactors = FALSE
    )
  } else {
    result_df <- data.frame(
      status = numeric(0),
      message = character(0),
      stringsAsFactors = FALSE
    )
  }
  
  return(result_df)
}