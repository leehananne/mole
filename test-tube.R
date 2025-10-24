# --- 0. Load Required Libraries ---
# Make sure these are installed: install.packages(c("httr", "jsonlite", "dplyr", "readr"))
library(httr)      # For making API requests
library(jsonlite)  # For parsing JSON
library(dplyr)     # For data manipulation (bind_rows, select, filter, etc.)
library(readr)     # For writing CSV files efficiently

# --- 1. Function to Fetch and Process TfL StopPoint Data ---
fetch_and_process_tfl_stoppoints <- function() {
  # Define API endpoints
  urls <- c(
    tube = "https://api.tfl.gov.uk/StopPoint/Mode/tube",
    elizabeth_overground = "https://api.tfl.gov.uk/StopPoint/Mode/elizabeth-line,overground"
  )
  
  all_stations_list <- list() # Initialize list to store results
  
  # Loop through URLs to fetch data
  for (mode_group in names(urls)) {
    url <- urls[[mode_group]]
    message("Fetching data for: ", mode_group, " from ", url)
    
    tryCatch({
      response <- httr::GET(url, timeout(60)) # Increased timeout to 60 seconds
      # Raise an error if the HTTP request failed (e.g., 404, 500)
      stop_for_status(response, task = paste("fetch data for", mode_group))
      
      json_content <- httr::content(response, "text", encoding = "UTF-8")
      # Parse JSON, flatten nested lists, simplify into data frames where possible
      parsed_data <- jsonlite::fromJSON(json_content, flatten = TRUE, simplifyDataFrame = TRUE)
      
      # Extract the main data frame of stop points
      stations_df <- parsed_data$stopPoints
      
      # Check if we got a valid data frame with rows
      if (is.data.frame(stations_df) && nrow(stations_df) > 0) {
        message("-> Successfully parsed ", nrow(stations_df), " stop points.")
        stations_df$modeGroupFetched <- mode_group # Add info about which API call it came from
        all_stations_list[[mode_group]] <- stations_df # Add to our list
      } else {
        warning("-> No 'stopPoints' data frame found or it was empty for ", mode_group)
      }
      
    }, error = function(e) {
      # Report error if fetching or parsing fails for a specific URL
      warning("!!! Error processing ", mode_group, ": ", e$message)
    })
  } # End loop
  
  # Check if we collected any data at all
  if (length(all_stations_list) == 0) {
    stop("Failed to fetch any valid data from the TfL API endpoints. Cannot proceed.")
  }
  
  # Combine the data frames from all successful API calls
  message("Combining fetched data...")
  combined_df <- bind_rows(all_stations_list)
  message("Combined data frame created with ", nrow(combined_df), " total rows before filtering.")
  
  # --- Create the final master_tube data frame ---
  message("Filtering and cleaning data to create master_tube...")
  
  # Identify the correct Naptan column (usually naptanId for stations)
  # Check available names just in case API structure changes
  available_names <- names(combined_df)
  naptan_col <- if ("naptanId" %in% available_names) {
    "naptanId"
  } else if ("stationNaptan" %in% available_names) {
    warning("Using 'stationNaptan' as NaptanCode, 'naptanId' preferred but not found.")
    "stationNaptan"
  } else {
    stop("Could not find required Naptan ID column ('naptanId' or 'stationNaptan').")
  }
  message("Using column '", naptan_col, "' for NaptanCode.")
  
  master_tube <- combined_df %>%
    # Select and rename the essential columns
    select(
      StationName = commonName,
      NaptanCode = !!sym(naptan_col), # Use the identified Naptan column
      Latitude = lat,
      Longitude = lon,
      StopType = stopType,
      Modes = modes # Keep modes if needed for extra info (might be a list column)
    ) %>%
    # Filter for valid station entries
    filter(
      # Keep only actual Metro/Rail stations based on StopType
      grepl("NaptanMetroStation|NaptanRailStation", StopType, ignore.case = TRUE),
      # Ensure essential fields are not missing
      !is.na(StationName) & StationName != "",
      !is.na(NaptanCode) & NaptanCode != "",
      !is.na(Latitude),
      !is.na(Longitude)
    ) %>%
    # Ensure each NaptanCode is unique
    distinct(NaptanCode, .keep_all = TRUE) %>%
    # Arrange alphabetically by name
    arrange(StationName)
  
  # Check if the final data frame has data
  if (nrow(master_tube) == 0) {
    stop("No valid Metro or Rail stations remained after filtering. Check API data or filter logic.")
  }
  
  message("master_tube data frame created successfully with ", nrow(master_tube), " unique stations.")
  return(master_tube)
}

# --- 2. Execute the Function to Create master_tube ---
master_tube_df <- fetch_and_process_tfl_stoppoints()

# --- 3. Explore the Final Data Frame (Optional) ---
if (exists("master_tube_df") && is.data.frame(master_tube_df) && nrow(master_tube_df) > 0) {
  message("\n--- Structure of final master_tube_df ---")
  glimpse(master_tube_df)
  
  message("\n--- First 6 Rows of master_tube_df ---")
  print(head(master_tube_df))
  
  # --- 4. Save the Data Frame to CSV ---
  output_filename <- "master_tube_stations.csv"
  message("\nSaving master_tube_df to '", output_filename, "' in working directory: ", getwd())
  tryCatch({
    readr::write_csv(master_tube_df, output_filename)
    message("File saved successfully.")
  }, error = function(e) {
    warning("!!! Error saving file: ", e$message)
  })
  
} else {
  warning("master_tube_df was not created successfully or is empty. File not saved.")
}

message("\n--- End of script ---")