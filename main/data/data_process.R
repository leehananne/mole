# Process and save the master station list as csv
# Station: name, naptan, lat, long, lift, samelevel, interchange, farezone, wifi

library(httr)
library(jsonlite)
library(stringr)
library(dplyr)

lift_df <- read.csv("data/Lifts.csv", stringsAsFactors = FALSE)
stationpoint_df <- read.csv("data/StationPoints.csv", stringsAsFactors = FALSE)
interchange_df <- read.csv("data/StepFreeInterchange.csv", stringsAsFactors = FALSE)
platform_df <- read.csv("data/Platforms.csv", stringsAsFactors = FALSE)
station_df <- read.csv("data/Stations.csv", stringsAsFactors = FALSE)


# ==============================================
# 1. TfL API Data Call
# ==============================================

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

# --- Load API Station Data (Name, Naptan, Lat, Long) ---  
api_station_data <- fetch_and_process_tfl_stoppoints()


# ==============================================
# 2. Station Topology CSV Data
# ==============================================

# A. Lift Data: Count lifts per station
lift_df <- lift_df %>%
  group_by(StationUniqueId) %>%
  summarise(LiftCount = n(), .groups = "drop")

# B. Same Level Data: Identify stations on same level
stationpoint_df <- stationpoint_df %>%
  group_by(StationUniqueId) %>%
  summarise(Distinct_Levels_Count = n_distinct(Level), .groups = "drop") %>%
  filter(Distinct_Levels_Count == 1) %>%
  mutate(SameLevel = TRUE) %>%
  select(StationUniqueId, SameLevel)

# C. Interchange Data
# C1. Direct Interchange Data (from StepFreeInterchange.csv)
parsed_ids <- stringr::str_split_fixed(interchange_df$FromPlatformUniqueId, "-", 2)[, 1]
interchange_df <- data.frame(StationUniqueId = unique(parsed_ids[parsed_ids != ""])) %>%
  mutate(HasInterchange = TRUE)

# C2. Platform Data (from Platforms.csv)
platform_df <- platform_df %>%
  group_by(StationUniqueId) %>%
  summarise(
    PlatformScore = case_when(
      all(HasServiceInterchange) ~ 2,
      any(HasServiceInterchange) ~ 1,
      TRUE ~ 0
    ),
    .groups = "drop"
  )

# C3. Combine for Final Interchange Score
interchange_combined <- full_join(interchange_df, platform_df, by = "StationUniqueId") %>%
  mutate(
    # Replace NAs with defaults before calculation
    HasInterchange = coalesce(HasInterchange, FALSE),
    PlatformScore = coalesce(PlatformScore, 0),
    
    # 2 for Full, 1 for Partial, 0 for None
    InterchangeVal = case_when(
      HasInterchange == TRUE | PlatformScore == 2 ~ 2,
      PlatformScore == 1 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  select(StationUniqueId, InterchangeVal)

# D. Station Features (Fare Zones, Wifi) 
station_df <- station_df %>%
  select(UniqueId, FareZones, Wifi)


# ==============================================
# 3. Data Combine
# ==============================================

station_table_data <- api_station_data %>%
  left_join(lift_df, by = c("NaptanCode" = "StationUniqueId")) %>%
  left_join(lift_df, by = c("HubNaptan" = "StationUniqueId"), suffix = c(".naptan", ".hub")) %>%
  
  left_join(stationpoint_df, by = c("NaptanCode" = "StationUniqueId")) %>%
  left_join(stationpoint_df, by = c("HubNaptan" = "StationUniqueId"), suffix = c(".naptan", ".hub")) %>%
  
  left_join(interchange_combined, by = c("NaptanCode" = "StationUniqueId")) %>%
  left_join(interchange_combined, by = c("HubNaptan" = "StationUniqueId"), suffix = c(".naptan", ".hub")) %>%
  
  left_join(station_df, by = c("NaptanCode" = "UniqueId")) %>%
  left_join(station_df, by = c("HubNaptan" = "UniqueId"), suffix = c(".naptan", ".hub")) %>%
  
mutate(
  # Lift: Coalesce Naptan/Hub, default to 0
  Lift = coalesce(LiftCount.naptan, LiftCount.hub, 0L),
  
  # SameLevel: Coalesce Naptan/Hub, default to FALSE
  SameLevel = coalesce(SameLevel.naptan, SameLevel.hub, FALSE),
  
  # Interchange: Coalesce Naptan/Hub, default to 0
  Interchange = coalesce(InterchangeVal.naptan, InterchangeVal.hub, 0),
  
  # FareZones: Coalesce Naptan/Hub, keep NA if missing (or set default "")
  FareZones = coalesce(FareZones.naptan, FareZones.hub),
  
  # Wifi: Coalesce Naptan/Hub, keep NA if missing (or set default "no")
  Wifi = coalesce(Wifi.naptan, Wifi.hub)
) %>%
  
  select(
    StationName,
    NaptanCode,
    HubNaptan,
    Latitude,
    Longitude,
    Lift,
    SameLevel,
    Interchange,
    FareZones,
    Wifi
  ) %>%
  arrange(StationName)

# Save to CSV for future usage
write.csv(station_table_data, "data_master.csv", row.names = FALSE)