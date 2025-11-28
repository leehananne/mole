# Journey Router Component
# This component displays journey routing information in a visual format
# Usage: Call journey_router_ui(id) in your UI, then use renderUI with generate_journey_html() in server

# --- 1. CONSTANTS (Colors) ---
tube_colors <- c(
  "Victoria" = "#0098D4",
  "District" = "#00782A",
  "Central" = "#E32017",
  "Piccadilly" = "#003688",
  "Northern" = "#000000",
  "Jubilee" = "#A0A5A9",
  "Bakerloo" = "#B36305",
  "Metropolitan" = "#9B0056",
  "Circle" = "#FFD300",
  "Hammersmith-City" = "#F3A9BB",
  "Waterloo-City" = "#95CDBA",
  "Elizabeth" = "#6950a1",
  "Overground" = "#ef7b10",
  "DLR" = "#00AFAD",
  "Tram" = "#87c403"
)

# --- 1.5. HELPER FUNCTION ---
# Normalizes line names by replacing " & " with "-" for color lookup
# Arguments:
#   line_name: Character string with the line name (e.g., "Hammersmith & City")
# Returns: Normalized line name (e.g., "Hammersmith-City")
normalize_line_name <- function(line_name) {
  if (is.na(line_name) || line_name == "") {
    return("Walking")
  }
  # Replace " & " with "-" for color lookup
  normalized <- gsub(" & ", "-", line_name, fixed = TRUE)
  return(normalized)
}

# --- 2. UI FUNCTION ---
# Arguments:
#   id: The outputID for the shiny element (e.g., "journey_route_output")
journey_router_ui <- function(id) {
  tagList(
    # Add the CSS specifically for this component
    tags$head(
      tags$style(HTML("
        .journey-container { max-width: 100%; font-family: 'Arial', sans-serif; margin-top: 10px; }
        .journey-step { display: flex; min-height: 60px; }
        .graphic-col { display: flex; flex-direction: column; align-items: center; margin-right: 10px; min-width: 25px; }
        .station-dot { width: 16px; height: 16px; border-radius: 50%; background: white; border: 3px solid #333; z-index: 2; }
        .connector-line { width: 4px; flex-grow: 1; background-color: #ccc; margin-top: -2px; margin-bottom: -2px; z-index: 1; }
        .info-col { padding-top: 0px; }
        .station-name { font-weight: bold; font-size: 14px; }
        .line-info { font-size: 12px; color: #666; }
        .duration-badge { background: #eee; padding: 1px 5px; border-radius: 3px; font-size: 10px; margin-left: 5px; }
      "))
    ),
    # The placeholder where the router will appear
    uiOutput(id)
  )
}

# --- 3. TRANSFORMATION FUNCTION ---
# Converts journey_details from extract_journey_details() to format expected by generate_journey_html()
# Arguments:
#   journey_details: Data frame from extract_journey_details() with columns: route_name, departure_name, arrival_name, duration
# Returns: Data frame with columns: Line, StartStation, EndStation, Duration
transform_journey_details <- function(journey_details) {
  if (is.null(journey_details) || !is.data.frame(journey_details) || nrow(journey_details) == 0) {
    return(data.frame(Line = character(), 
    StartStation = character(), 
    EndStation = character(), 
    Duration = integer(), 
    DepartureTime = character(),
    ArrivalTime = character(),
    stringsAsFactors = FALSE))
  }
  
  # Validate required columns exist
  required_cols <- c("route_name", "departure_name", "arrival_name", "duration", "departure_time", "arrival_time")
  missing_cols <- setdiff(required_cols, names(journey_details))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns in journey_details:", paste(missing_cols, collapse = ", ")))
  }
  
  # Extract line name from route_name (remove " Line" suffix if present)
  # Use safe extraction with length checks
  route_name_vec <- journey_details$route_name
  if (is.null(route_name_vec) || length(route_name_vec) == 0) {
    route_name_vec <- rep(NA_character_, nrow(journey_details))
  }
  
  line_names <- ifelse(
    is.na(route_name_vec) | route_name_vec == "",
    "Walking",
    gsub("\\s+Line$", "", route_name_vec, ignore.case = TRUE)
  )
  
  # Safely extract other columns with length checks
  departure_name_vec <- if (length(journey_details$departure_name) > 0) journey_details$departure_name else rep("", nrow(journey_details))
  arrival_name_vec <- if (length(journey_details$arrival_name) > 0) journey_details$arrival_name else rep("", nrow(journey_details))
  duration_vec <- if (length(journey_details$duration) > 0) journey_details$duration else rep(0, nrow(journey_details))
  departure_time_vec <- if (length(journey_details$departure_time) > 0) journey_details$departure_time else rep("", nrow(journey_details))
  arrival_time_vec <- if (length(journey_details$arrival_time) > 0) journey_details$arrival_time else rep("", nrow(journey_details))
  
  # Create transformed data frame
  result <- data.frame(
    Line = line_names,
    StartStation = ifelse(is.na(departure_name_vec), "", departure_name_vec),
    EndStation = ifelse(is.na(arrival_name_vec), "", arrival_name_vec),
    Duration = ifelse(is.na(duration_vec), 0, duration_vec),
    DepartureTime = ifelse(is.na(departure_time_vec), "", departure_time_vec),
    ArrivalTime = ifelse(is.na(arrival_time_vec), "", arrival_time_vec),
    stringsAsFactors = FALSE
  )
  
  return(result)
}

# --- 4. LOGIC FUNCTION ---
# Call this in your server.R inside renderUI
# Arguments:
#   journey_data: Data frame with columns: Line, StartStation, EndStation, Duration
generate_journey_html <- function(journey_data) {
  
  # Clear any previous state - ensure we start fresh
  # Error handling: if no data, return nothing
  if (is.null(journey_data) || !is.data.frame(journey_data) || nrow(journey_data) == 0) {
    return(div(class = "journey-container", 
               p("No journey data available. Select origin and destination stations, then click 'Plan Journey' to find a route.")))
  }
  
  # Validate required columns exist
  required_cols <- c("Line", "StartStation", "EndStation", "Duration", "DepartureTime", "ArrivalTime")
  missing_cols <- setdiff(required_cols, names(journey_data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Initialize fresh list for steps - clear any previous data
  steps_html <- list()
  
  # Generate HTML for each leg
  for (i in seq_len(nrow(journey_data))) {
    # Extract scalar values from the row to avoid vector indexing issues
    # Add safety checks for each extraction - ensure we get scalars, not vectors
    line_val <- if (i <= length(journey_data$Line) && i > 0) {
      val <- journey_data$Line[i]
      # Ensure scalar - take first element if vector
      if (length(val) > 1) val[1] else val
    } else {
      NA_character_
    }
    
    start_station <- if (i <= length(journey_data$StartStation) && i > 0) {
      val <- journey_data$StartStation[i]
      if (length(val) > 1) val[1] else val
    } else {
      ""
    }
    
    end_station <- if (i <= length(journey_data$EndStation) && i > 0) {
      val <- journey_data$EndStation[i]
      if (length(val) > 1) val[1] else val
    } else {
      ""
    }
    
    duration_val <- if (i <= length(journey_data$Duration) && i > 0) {
      val <- journey_data$Duration[i]
      if (length(val) > 1) val[1] else val
    } else {
      0
    }
    
    departure_time <- if (i <= length(journey_data$DepartureTime) && i > 0) {
      val <- journey_data$DepartureTime[i]
      if (length(val) > 1) val[1] else val
    } else {
      ""
    }
    
    # Ensure line_name is a scalar character
    if (is.na(line_val) || length(line_val) == 0 || (length(line_val) == 1 && line_val == "")) {
      line_name <- "Walking"
    } else {
      line_name <- as.character(line_val[1])  # Force scalar
    }
    
    # Normalize line name for color lookup (e.g., "Hammersmith & City" -> "Hammersmith-City")
    normalized_line_name <- normalize_line_name(line_name)
    
    # Safe color lookup - ensure normalized_line_name is a scalar
    if (length(normalized_line_name) > 1) {
      normalized_line_name <- normalized_line_name[1]
    }
    color <- tryCatch({
      tube_colors[[as.character(normalized_line_name)]]
    }, error = function(e) {
      NULL
    })
    if(is.null(color)) color <- "#333" # Fallback color
    
    line_style <- paste0("background-color: ", color, ";")
    if (line_name == "Walking") line_style <- "border-left: 4px dotted #666; background-color: transparent; width: 0;"
    
    # Format line display text
    line_display <- if (line_name == "Walking") {
      "Walk"
    } else {
      paste(line_name, "Line")
    }
    
    step_html <- tags$div(class = "journey-step",
             tags$div(class = "time-info", departure_time),
             tags$div(class = "graphic-col",
                      tags$div(class = "station-dot", style = paste0("border-color: ", color, ";")),
                      tags$div(class = "connector-line", style = line_style)
             ),
             tags$div(class = "info-col",
                      div(class = "station-name", start_station),
                      div(class = "line-info", line_display),
                      span(class = "duration-badge", paste(duration_val, "mins"))
             )
    )
    steps_html[[i]] <- step_html
  }
  
  # Generate Final Destination Dot
  last_row_idx <- nrow(journey_data)
  # Add safety checks for final destination extraction - ensure scalars
  final_line_val <- if (last_row_idx <= length(journey_data$Line) && last_row_idx > 0) {
    val <- journey_data$Line[last_row_idx]
    if (length(val) > 1) val[1] else val
  } else {
    NA_character_
  }
  
  final_end_station <- if (last_row_idx <= length(journey_data$EndStation) && last_row_idx > 0) {
    val <- journey_data$EndStation[last_row_idx]
    if (length(val) > 1) val[1] else val
  } else {
    ""
  }
  
  final_arrival_time <- if (last_row_idx <= length(journey_data$ArrivalTime) && last_row_idx > 0) {
    val <- journey_data$ArrivalTime[last_row_idx]
    if (length(val) > 1) val[1] else val
  } else {
    ""
  }
  
  # Ensure final_line is a scalar character
  if (is.na(final_line_val) || length(final_line_val) == 0 || (length(final_line_val) == 1 && final_line_val == "")) {
    final_line <- "Walking"
  } else {
    final_line <- as.character(final_line_val[1])  # Force scalar
  }
  
  # Normalize line name for color lookup (e.g., "Hammersmith & City" -> "Hammersmith-City")
  normalized_final_line <- normalize_line_name(final_line)
  
  # Safe color lookup - ensure normalized_final_line is a scalar
  if (length(normalized_final_line) > 1) {
    normalized_final_line <- normalized_final_line[1]
  }
  final_color <- tryCatch({
    tube_colors[[as.character(normalized_final_line)]]
  }, error = function(e) {
    NULL
  })
  if(is.null(final_color)) final_color <- "#333"
  
  final_step <- tags$div(class = "journey-step",
                         tags$div(class = "time-info", final_arrival_time),
                         tags$div(class = "graphic-col",
                                  tags$div(class = "station-dot", style = paste0("border-color: ", final_color, ";"))
                         ),
                         tags$div(class = "info-col",
                                  div(class = "station-name", final_end_station),
                                  div(class = "line-info", "Arrive")
                         )
  )
  
  # Return the bundle - use tagList to properly combine all elements
  # This ensures clean rendering without accumulating old data
  div(class = "journey-container", 
      tagList(steps_html, final_step)
  )
}