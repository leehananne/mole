# Station location data parsing - API
# Using TfL API for station data
# Combined with lifts.csv and StepFreeInterchangeInfo.csv
# Outputs name, naptan, hub naptan, lat, long, lift count, and interchange status.

library(shiny)
library(httr)     # For API
library(jsonlite) # For API
library(stringr)  # <-- ADDED BACK for string parsing
library(dplyr)    # For data manipulation
library(DT)       # For displaying an interactive data table

# --- Data Pre-engineering ---

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
    Latitude = numeric(), Longitude = numeric()
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
      select(
        StationName = commonName,
        NaptanCode = any_of("stationNaptan"),
        HubNaptan = any_of("hubNaptanCode"),
        Latitude = lat,
        Longitude = lon,
        StopType = stopType
      ) %>%
      filter(
        grepl("NaptanMetroStation|NaptanRailStation", StopType, ignore.case = TRUE),
        !is.na(StationName) & StationName != "",
        !is.na(Latitude), !is.na(Longitude)
      ) %>%
      mutate(
        NaptanCode = if ("NaptanCode" %in% names(.)) {
          ifelse(is.na(NaptanCode) | NaptanCode == "", "", NaptanCode)
        } else { "" },
        HubNaptan = if ("HubNaptan" %in% names(.)) {
          ifelse(is.na(HubNaptan) | HubNaptan == "", "", HubNaptan)
        } else { "" }
      ) %>%
      filter(NaptanCode != "") %>%
      distinct(NaptanCode, .keep_all = TRUE) %>%
      arrange(StationName)
    
    if (nrow(master_tube_df) == 0) stop("No valid Metro/Rail stations found after filtering.")
    
    final_station_data <- master_tube_df %>%
      select(
        StationName,
        NaptanCode,
        HubNaptan,
        Latitude,
        Longitude
      )
    
    message("API Station data processing complete. Found ", nrow(final_station_data), " unique stations.")
    return(final_station_data)
    
  }, error = function(e) {
    message("!!! Error fetching/processing TfL Stoppoint data: ", e$message)
    print(e)
    return(empty_df)
  })
}

# --- 1. Fetch Station Data from API ---
api_station_data <- fetch_and_process_tfl_stoppoints()

# --- 2. Load and Process Lifts.csv ---
lifts_filename <- "lifts.csv"
if (!file.exists(lifts_filename)) {
  warning(paste(lifts_filename, "not found. Lift data will be unavailable."))
  lift_counts <- data.frame(StationUniqueId = character(), LiftCount = integer())
} else {
  lifts_df <- read.csv(lifts_filename, stringsAsFactors = FALSE)
  
  if (!"StationUniqueId" %in% names(lifts_df)) {
    warning("'StationUniqueId' column not found in lifts.csv. Lift data unavailable.")
    lift_counts <- data.frame(StationUniqueId = character(), LiftCount = integer())
  } else {
    lift_counts <- lifts_df %>%
      group_by(StationUniqueId) %>%
      summarise(LiftCount = n(), .groups = 'drop')
    message("Successfully processed lift data. Found lift information for ", nrow(lift_counts), " stations.")
  }
}

# --- 3. Load and Process StepFreeInterchangeInfo.csv ---
interchange_filename <- "stepfreeinterchange.csv"
if (!file.exists(interchange_filename)) {
  warning(paste(interchange_filename, "not found. Interchange data will be unavailable."))
  interchange_lookup <- data.frame(StationUniqueId = character(), HasInterchange = logical())
} else {
  interchange_df <- read.csv(interchange_filename, stringsAsFactors = FALSE)
  
  if (!"FromPlatformUniqueId" %in% names(interchange_df)) {
    warning("'FromPlatformUniqueId' column not found in StepFreeInterchangeInfo.csv. Data unavailable.")
    interchange_lookup <- data.frame(StationUniqueId = character(), HasInterchange = logical())
  } else {
    # Parse the ID: "HUBLBG-Plat01" -> "HUBLBG"
    parsed_ids <- stringr::str_split_fixed(interchange_df$FromPlatformUniqueId, "-", 2)[, 1]
    
    # Create a lookup table of unique IDs that have an interchange
    interchange_lookup <- data.frame(StationUniqueId = unique(parsed_ids[parsed_ids != ""])) %>%
      mutate(HasInterchange = TRUE)
    
    message("Successfully processed interchange data. Found info for ", nrow(interchange_lookup), " stations.")
  }
}

# --- 4. Join Data and Finalize Table ---
station_table_data <- api_station_data %>%
  
  # --- Join Lift Data (2-stage join) ---
  # 1. Join by NaptanCode
  left_join(lift_counts, by = c("NaptanCode" = "StationUniqueId")) %>%
  # 2. Join by HubNaptan
  left_join(lift_counts, by = c("HubNaptan" = "StationUniqueId"), suffix = c(".naptan", ".hub")) %>%
  
  # --- Join Interchange Data (2-stage join) ---
  # 1. Join by NaptanCode
  left_join(interchange_lookup, by = c("NaptanCode" = "StationUniqueId")) %>%
  # 2. Join by HubNaptan
  left_join(interchange_lookup, by = c("HubNaptan" = "StationUniqueId"), suffix = c(".naptan", ".hub")) %>%
  
  # --- Consolidate and Format ---
  mutate(
    # Find first non-NA lift count (Naptan, then Hub), default to 0
    FinalLiftCount = coalesce(LiftCount.naptan, LiftCount.hub, 0L),
    
    # Convert to character, 0 -> "0"
    Lift = as.character(FinalLiftCount),
    
    # Find first non-NA interchange status (Naptan, then Hub), default to FALSE
    `Interchange step-free` = coalesce(HasInterchange.naptan, HasInterchange.hub, FALSE)
  ) %>%
  
  # --- Select and reorder final columns ---
  select(
    StationName,
    NaptanCode,
    HubNaptan,
    Latitude,
    Longitude,
    Lift,
    `Interchange step-free`
  ) %>%
  arrange(StationName) # Sort alphabetically by name


# --- UI (User Interface) ---
ui <- fluidPage(
  titlePanel("Station Data Viewer"),
  
  mainPanel(
    width = 12,
    h4("Searchable Station List"),
    p(paste("Displaying", nrow(station_table_data), "stations loaded from TfL API, lifts.csv, and StepFreeInterchangeInfo.csv")),
    hr(),
    DT::dataTableOutput("stationTable")
  )
)

# --- Server (Logic) ---
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