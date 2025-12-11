# All data parsed from TfL -> StopPoint/Mode/tube API
# Facilities data from API too
# Lift, Toilet not accurate but still can use wifi info if we want

library(shiny)
library(httr)     # For API
library(jsonlite) # For API
library(stringr)  # For splitting the coordinates string
library(dplyr)    # For data manipulation
library(purrr)    # For parsing nested data
library(DT)       # For displaying an interactive data table

# --- Data Pre-engineering ---

# Helper function to safely extract nested properties
find_prop <- function(prop_list, key_name) {
  # Set default value based on key
  default_val <- if (key_name == "Lifts") "0" else ""
  
  if (!is.data.frame(prop_list) || nrow(prop_list) == 0) {
    return(default_val) # Default if no properties
  }
  
  val <- prop_list$value[prop_list$key == key_name]
  
  if (length(val) > 0 && !is.na(val[1])) {
    return(val[1]) # Return the value (e.g., "Yes")
  } else {
    return(default_val) # Key not found, return default
  }
}

# TfL API to get the locations
fetch_and_process_tfl_stoppoints <- function() {
  urls <- c(
    tube = "https://api.tfl.gov.uk/StopPoint/Mode/tube",
    elizabeth_overground = "https://api.tfl.gov.uk/StopPoint/Mode/elizabeth-line,overground"
  )
  all_stations_list <- list()
  
  # Define an empty data frame to return on error
  empty_df <- data.frame(
    StationName = character(), NaptanCode = character(), HubNaptan = character(),
    Latitude = numeric(), Longitude = numeric(),
    Lift = character(), Toilet = character(), Wifi = character()
  )
  
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
    
    master_tube_df <- combined_df %>%
      # Select all the columns we need, including the nested one
      # Use any_of() to safely select columns that might be missing
      select(
        StationName = commonName,
        NaptanCode = any_of("stationNaptan"), # <-- Use correct field name
        HubNaptan = any_of("hubNaptanCode"),   # <-- Use correct field name
        Latitude = lat,
        Longitude = lon,
        StopType = stopType,
        Properties = additionalProperties
      ) %>%
      filter(
        grepl("NaptanMetroStation|NaptanRailStation", StopType, ignore.case = TRUE),
        !is.na(StationName) & StationName != "",
        # Don't filter on NaptanCode here, as it might be missing
        !is.na(Latitude), !is.na(Longitude)
      ) %>%
      
      # --- UPDATED SECTION: Parse nested data ---
      mutate(
        # Use purrr::map_chr to apply our helper function to each row
        Lift = purrr::map_chr(Properties, ~find_prop(., "Lifts")),
        Toilet = purrr::map_chr(Properties, ~find_prop(., "Toilets")),
        Wifi = purrr::map_chr(Properties, ~find_prop(., "WiFi")),
        
        # Add NaptanCode column if it doesn't exist, and clean it
        NaptanCode = if ("NaptanCode" %in% names(.)) {
          ifelse(is.na(NaptanCode) | NaptanCode == "", "", NaptanCode)
        } else { "" },
        
        # Add HubNaptan column if it doesn't exist, and clean it
        HubNaptan = if ("HubNaptan" %in% names(.)) {
          ifelse(is.na(HubNaptan) | HubNaptan == "", "", HubNaptan)
        } else { "" }
      ) %>%
      
      # Now we can safely filter out stations that still have no NaptanCode
      filter(NaptanCode != "") %>%
      distinct(NaptanCode, .keep_all = TRUE) %>%
      arrange(StationName)
    
    if (nrow(master_tube_df) == 0) stop("No valid Metro/Rail stations found after filtering.")
    
    # --- FINAL STEP: Select and order columns as requested ---
    final_station_data <- master_tube_df %>%
      select(
        StationName,
        NaptanCode,
        HubNaptan,
        Latitude,
        Longitude,
        Lift,
        Toilet,
        Wifi
      )
    
    message("Station data processing complete. Found ", nrow(final_station_data), " unique stations.")
    return(final_station_data) # Return the final, clean data frame
    
  }, error = function(e) {
    message("!!! Error fetching/processing TfL Stoppoint data: ", e$message)
    # Print the full error for debugging
    print(e)
    return(empty_df) # Return the empty data frame on error
  })
}

# --- This section is now cleaner ---
station_table_data <- fetch_and_process_tfl_stoppoints()

# --- UI (User Interface) ---
# (No changes needed)
ui <- fluidPage(
  titlePanel("Station Data Viewer"),
  
  mainPanel(
    width = 12,
    h4("Searchable Station List"),
    p(paste("Displaying", nrow(station_table_data), "stations loaded from TfL API")),
    hr(),
    DT::dataTableOutput("stationTable")
  )
)

# --- Server (Logic) ---
# (No changes needed)
server <- function(input, output, session) {
  
  # --- 1. Render the Station Table ---
  output$stationTable <- DT::renderDataTable({
    # Show notification if data is empty
    if (nrow(station_table_data) == 0) {
      showNotification(
        "Error: No data loaded from API. Please check connection/API status.",
        type = "error",
        duration = NULL
      )
    }
    
    DT::datatable(
      station_table_data,
      options = list(pageLength = 15, searching = TRUE, lengthChange = TRUE),
      rownames = FALSE,
      filter = 'top'
    )
  })
  
}

# Run the application
shinyApp(ui, server)