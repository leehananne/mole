library(shiny)
library(plotly)
library(httr)
library(jsonlite)
library(ggplot2)
library(purrr)
library(dplyr)
library(lubridate)
library(DT)

# Load API keys from config file or environment variables
# Priority: config.R (if exists) > environment variables > fallback
if (file.exists("config.R")) {
  source("config.R", local = TRUE)
  message("Loaded API keys from config.R")
} else {
  # Try to load from environment variables
  google_maps_api <- Sys.getenv("GOOGLE_MAPS_API_KEY", unset = NA)
  
  if (is.na(google_maps_api) || google_maps_api == "") {
    # Fallback: use hardcoded key (not recommended for production)
    # TODO: Remove this fallback and require proper configuration
    warning("API key not found in environment or config.R. Using fallback key.")
    google_maps_api <- "AIzaSyAyNRSTGTUmjKXa7CqdmxczCNl4U3HOEYI"
  } else {
    message("Loaded API keys from environment variables")
  }
}

fetch_and_process_tfl_stoppoints <- function() {
  urls <- c(
    tube = "https://api.tfl.gov.uk/StopPoint/Mode/tube",
    elizabeth_overground = "https://api.tfl.gov.uk/StopPoint/Mode/elizabeth-line,overground"
  )
  all_stations_list <- list()
  station_choices <<- list("Loading Error" = "")
  master_tube_locations <<- data.frame(NaptanCode=character(), Latitude=numeric(), Longitude=numeric(), StationName=character())
  tryCatch({
    for (mode_group in names(urls)) {
      url <- urls[[mode_group]]
      message("Fetching data for: ", mode_group)
      response <- httr::GET(url, timeout(60))
      stop_for_status(response, task = paste("fetch data for", mode_group))
      json_content <- httr::content(response, "text", encoding = "UTF-8")
      parsed_data <- jsonlite::fromJSON(json_content, flatten = TRUE, simplifyDataFrame = TRUE)
      stations_df <- parsed_data$stopPoints
      if (is.data.frame(stations_df) && nrow(stations_df) > 0) {
        stations_df$modeGroupFetched <- mode_group
        all_stations_list[[mode_group]] <- stations_df
      } else {
        warning("No 'stopPoints' data frame found or empty for ", mode_group)
      }
    }
    if (length(all_stations_list) == 0) stop("No station data successfully fetched from any endpoint.")
    combined_df <- bind_rows(all_stations_list)
    message("Combined data frame created with ", nrow(combined_df), " total rows before filtering.")
    message("Filtering and cleaning data...")
    available_names <- names(combined_df)
    naptan_col <- if ("naptanId" %in% available_names) { "naptanId" } else if ("stationNaptan" %in% available_names) { "stationNaptan" } else { stop("Required Naptan ID column ('naptanId' or 'stationNaptan') not found.") }
    message("Using column '", naptan_col, "' for NaptanCode.")
    master_tube_df <- combined_df %>%
      select(StationName = commonName, NaptanCode = !!sym(naptan_col), Latitude = lat, Longitude = lon, StopType = stopType) %>%
      filter(grepl("NaptanMetroStation|NaptanRailStation", StopType, ignore.case = TRUE),
             !is.na(StationName) & StationName != "",
             !is.na(NaptanCode) & NaptanCode != "",
             !is.na(Latitude), !is.na(Longitude)) %>%
      distinct(NaptanCode, .keep_all = TRUE) %>%
      arrange(StationName)
    if (nrow(master_tube_df) == 0) stop("No valid Metro/Rail stations found after filtering.")
    station_choices <<- setNames(master_tube_df$NaptanCode, master_tube_df$StationName)
    master_tube_locations <<- master_tube_df %>% select(NaptanCode, Latitude, Longitude, StationName)
    message("Station data processing complete. Found ", nrow(master_tube_locations), " unique stations.")
  }, error = function(e) {
    message("!!! Error fetching/processing TfL Stoppoint data: ", e$message)
  })
}

fetch_and_process_tfl_stoppoints()

default_station_name <- "South Kensington Underground Station"
default_naptan_code <- NULL
if (length(station_choices) > 1 && !(names(station_choices)[1] %in% c("Loading Error"))) {
  message("Successfully loaded ", length(station_choices), " stations for dropdowns.")
  default_naptan_code <- station_choices[names(station_choices) == default_station_name]
  if (length(default_naptan_code) == 0 || is.na(default_naptan_code)) {
    warning("Default station '", default_station_name, "' not found. Using the first station instead.")
    default_naptan_code <- station_choices[[1]]
  } else {
    default_naptan_code <- unname(default_naptan_code)
    message("Default station set to: ", default_station_name, " (", default_naptan_code, ")")
  }
} else {
  warning("Failed to load station data from TfL API. Dropdowns will be empty or show error.")
  default_naptan_code <- ""
}

source("R/tfl_helpers.R")
source("R/weather_helpers.R")
source("R/journey_routing.R")


