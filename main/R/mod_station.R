# ==============================================================================
# STATION MODULE
#   Fetch service lines and toilet services
#   UI component for station card
# ==============================================================================


# Function: Fetch Line Servies from API
# ------------------------------------------------------------------------------
fetch_line_services <- function(naptan_id) {
  if (is.null(naptan_id) || naptan_id == "") return(character(0))
  
  url <- paste0("https://api.tfl.gov.uk/StopPoint/ServiceTypes?id=", naptan_id)
  
  tryCatch({
    resp <- httr::GET(url, timeout(10))
    stop_for_status(resp)
    content <- jsonlite::fromJSON(httr::content(resp, "text"), flatten = TRUE)
    
    if (length(content) > 0 && "lineName" %in% names(content)) {
      # Return unique, sorted line names
      return(sort(unique(content$lineName)))
    } else {
      return(character(0))
    }
  }, error = function(e) {
    warning(paste("Error fetching lines for", naptan_id, ":", e$message))
    return(character(0))
  })
}


# Function: Fetch Toilet Data from CSV
# ------------------------------------------------------------------------------
fetch_station_toilets <- function(naptan_id, hub_naptan, toilet_df) {
  if (is.null(toilet_df) || nrow(toilet_df) == 0) return(NULL)
  
  # 1. Determine ID to search (Try Naptan, fallback to Hub if Naptan fails match?)
  # The requirement: "Compare NaptanCode (if not HubNaptan) to toilet_data$StationUniqueId"
  # We will try Naptan first.
  
  matched_toilets <- toilet_df %>% filter(StationUniqueId == naptan_id)
  
  # If no match and we have a Hub ID, try that (optional, based on interpretation)
  if (nrow(matched_toilets) == 0 && !is.null(hub_naptan) && hub_naptan != "") {
    matched_toilets <- toilet_df %>% filter(StationUniqueId == hub_naptan)
  }
  
  return(matched_toilets)
}


# UI: Main Card UI Component
# ------------------------------------------------------------------------------
station_card_ui <- function(station, station_df, toilet_df) {
  
  # 1. EMPTY STATE CHECK
  if (length(station) == 0 || is.na(station) || station == "") {
    return(wellPanel(
      style = "text-align: center; color: #777; padding: 20px; border: 1px dashed #ddd; background: white;",
      icon("subway", class = "fa-2x"),
      p("Select a station above to view details.")
    ))
  }
  
  # 2. RESOLVE NAPTAN ID
  match_row <- station_df %>% filter(StationName == station) %>% head(1)
  
  if (nrow(match_row) > 0) {
    naptan_id <- match_row$NaptanCode
    hub_naptan <- if("HubNaptan" %in% names(match_row)) match_row$HubNaptan else NULL
    station_data <- match_row
  } else {
    # Try filtering by NaptanCode directly
    match_row_code <- station_df %>% filter(NaptanCode == station) %>% head(1)
    if (nrow(match_row_code) > 0) {
      naptan_id <- station
      hub_naptan <- if("HubNaptan" %in% names(match_row_code)) match_row_code$HubNaptan else NULL
      station_data <- match_row_code
    } else {
      return(div(class = "alert alert-warning", paste("Station data not found for:", station)))
    }
  }
  
  # 3. Data Extraction
  fare_zone <- if("FareZones" %in% names(station_data)) station_data$FareZones else "N/A"
  has_wifi  <- if("Wifi" %in% names(station_data)) tolower(as.character(station_data$Wifi)) %in% c("yes", "true", "1") else FALSE
  lift_info <- if("Lift" %in% names(station_data)) station_data$Lift else "Unknown"
  
  # 4. Fetch Services (API)
  lines_served <- fetch_line_services(naptan_id)
  
  lines_ui <- if (length(lines_served) > 0) {
    lapply(lines_served, function(line) {
      bg_color <- if (line %in% names(tube_colors)) tube_colors[[line]] else "#666666"
      
      span(class = "service-badge", 
           style = paste0("background-color: ", bg_color, " !important;"), 
           tools::toTitleCase(line))
    })
  } else {
    span(class = "text-muted", "Service info unavailable")
  }
  
  # 5. Fetch Toilet Data
  # Pass the dataframe (toilet_df) loaded in global.R
  station_toilets <- fetch_station_toilets(naptan_id, hub_naptan, toilet_df)
  toilet_count <- if(is.null(station_toilets)) 0 else nrow(station_toilets)
  
  # Build Toilet Rows
  toilet_rows_ui <- if (toilet_count > 0) {
    lapply(1:nrow(station_toilets), function(i) {
      row <- station_toilets[i, ]
      
      # Helper for icons
      # Ensure boolean is handled correctly (sometimes CSVs have "True"/"False" strings)
      is_accessible <- isTRUE(row$IsAccessible) || tolower(as.character(row$IsAccessible)) == "true"
      has_baby <- isTRUE(row$HasBabyChanging) || tolower(as.character(row$HasBabyChanging)) == "true"
      
      div(class = "toilet-row",
          span(class = "toilet-type", row$Type), # Left Aligned
          div(class = "toilet-icons", # Right Aligned
              # Accessible Icon
              span(class = paste("facility-icon", if(is_accessible) "icon-active" else "icon-inactive"),
                   icon("wheelchair"), title = if(is_accessible) "Accessible" else "Not Accessible"),
              
              # Baby Changing Icon
              span(class = paste("facility-icon", if(has_baby) "icon-active" else "icon-inactive"),
                   icon("baby"), title = if(has_baby) "Baby Changing Available" else "No Baby Changing")
          )
      )
    })
  } else {
    NULL
  }
  
  # 6. Build the UI Card
  div(class = "station-card",
      
      # Header
      h3(station_data$StationName),
      
      # Services Section
      div(style = "margin-bottom: 15px;",
          div(lines_ui)
      ),
      
      # Facilities Grid
      div(style = "background: #f9f9f9; padding: 10px; border-radius: 4px; font-size: 0.95rem;",
          
          # Fare Zone
          div(class = "facility-row",
              span(class = "facility-label", icon("ticket-alt"), " Fare Zone:"),
              span(fare_zone)
          ),
          
          # Lifts
          div(class = "facility-row",
              span(class = "facility-label", icon("elevator"), " Lifts:"),
              span(lift_info)
          ),
          
          # Wifi
          div(class = "facility-row",
              span(class = "facility-label", icon("wifi"), " Wifi:"),
              span(style = if(has_wifi) "color: #28a745; font-weight: bold;" else "color: #999;",
                   if(has_wifi) icon("check") else icon("times"),
                   if(has_wifi) " Available" else " No"
              )
          ),
          
          # Toilets (Redesigned)
          div(class = "toilet-container",
              tags$details(
                open = FALSE, # Closed by default
                # Logic to disable if count is 0
                onclick = if(toilet_count == 0) "return false;" else NULL, 
                
                tags$summary(
                  class = paste("toilet-summary", if(toilet_count == 0) "disabled" else ""),
                  span(class = "facility-label", icon("restroom"), " Toilets:"),
                  span(
                    paste0(toilet_count, " Found "),
                    if(toilet_count > 0) icon("caret-down", style="color: #007bff;") else NULL
                  )
                ),
                
                # Dropdown Content (Only renders if count > 0)
                if (toilet_count > 0) {
                  div(class = "toilet-list", toilet_rows_ui)
                } else {
                  NULL
                }
              )
          )
      )
  )
}