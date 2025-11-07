fetch_tfl_data <- function(naptan_code, days_list) {
  if (is.null(naptan_code) || naptan_code == "" || length(days_list) == 0) return(data.frame())
  all_days_data <- map_dfr(days_list, function(day) {
    api_url <- paste0("https://api.tfl.gov.uk/crowding/", naptan_code, "/", day)
    tryCatch({
      response <- httr::GET(api_url, timeout(15))
      stop_for_status(response)
      json_content <- httr::content(response, "text", encoding = "UTF-8")
      parsed_data <- jsonlite::fromJSON(json_content, flatten = TRUE)
      if (is.data.frame(parsed_data$timeBands) && nrow(parsed_data$timeBands) > 0) {
        day_dataframe <- parsed_data$timeBands
        day_dataframe$dayOfWeek <- day
        day_dataframe$naptan <- naptan_code
        if (!all(c("timeBand", "percentageOfBaseLine") %in% names(day_dataframe))) return(NULL)
        if (is.character(day_dataframe$timeBand) && all(grepl("^[0-9]+$", day_dataframe$timeBand))) {
          day_dataframe$timeBand <- factor(day_dataframe$timeBand, levels = as.character(0:95))
        } else if (is.numeric(day_dataframe$timeBand)) {
          day_dataframe$timeBand <- factor(day_dataframe$timeBand, levels = sort(unique(as.numeric(day_dataframe$timeBand))))
        }
        return(day_dataframe)
      } else {
        message("No valid timeBands data frame returned for Naptan ", naptan_code, " on ", day)
        if (exists("station_choices", inherits = TRUE)) {
          showNotification(paste("No crowding data available for", names(station_choices[station_choices == naptan_code]), "on", day), type = "warning", duration = 3)
        }
        return(NULL)
      }
    }, error = function(e) {
      status_code <- NA
      if (exists("response") && !is.null(response)) {
        try(status_code <- httr::status_code(response), silent = TRUE)
      }
      err_message <- paste("Error fetching TfL crowding data for", day)
      if (!is.na(status_code)) { err_message <- paste(err_message, "- HTTP Status:", status_code) }
      err_message <- paste(err_message, "-", e$message)
      message(err_message)
      showNotification(err_message, type = "error", duration = 8)
      return(NULL)
    })
  })
  if (is.null(all_days_data) || nrow(all_days_data) == 0) return(data.frame())
  return(all_days_data)
}


