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
  "Walking" = "dotted"
)

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
  if (is.null(journey_details) || nrow(journey_details) == 0) {
    return(data.frame(Line = character(), StartStation = character(), EndStation = character(), Duration = integer(), stringsAsFactors = FALSE))
  }
  
  # Extract line name from route_name (remove " Line" suffix if present)
  line_names <- ifelse(
    is.na(journey_details$route_name) | journey_details$route_name == "",
    "Walking",
    gsub("\\s+Line$", "", journey_details$route_name, ignore.case = TRUE)
  )
  
  # Create transformed data frame
  result <- data.frame(
    Line = line_names,
    StartStation = ifelse(is.na(journey_details$departure_name), "", journey_details$departure_name),
    EndStation = ifelse(is.na(journey_details$arrival_name), "", journey_details$arrival_name),
    Duration = ifelse(is.na(journey_details$duration), 0, journey_details$duration),
    stringsAsFactors = FALSE
  )
  
  return(result)
}

# --- 4. LOGIC FUNCTION ---
# Call this in your server.R inside renderUI
# Arguments:
#   journey_data: Data frame with columns: Line, StartStation, EndStation, Duration
generate_journey_html <- function(journey_data) {
  
  # Error handling: if no data, return nothing
  if (is.null(journey_data) || nrow(journey_data) == 0) {
    return(div(class = "journey-container", 
               p("No journey data available. Select origin and destination stations, then click 'Plan Journey' to find a route.")))
  }
  
  # Generate HTML for each leg
  steps_html <- lapply(seq_len(nrow(journey_data)), function(i) {
    row <- journey_data[i, ]
    line_name <- ifelse(is.na(row$Line) || row$Line == "", "Walking", row$Line)
    color <- tube_colors[[line_name]]
    if(is.null(color)) color <- "#333" # Fallback color
    
    line_style <- paste0("background-color: ", color, ";")
    if (line_name == "Walking") line_style <- "border-left: 4px dotted #666; background-color: transparent; width: 0;"
    
    # Format line display text
    line_display <- if (line_name == "Walking") {
      "Walk"
    } else {
      paste(line_name, "Line")
    }
    
    tags$div(class = "journey-step",
             tags$div(class = "graphic-col",
                      tags$div(class = "station-dot", style = paste0("border-color: ", color, ";")),
                      tags$div(class = "connector-line", style = line_style)
             ),
             tags$div(class = "info-col",
                      div(class = "station-name", row$StartStation),
                      div(class = "line-info", line_display,
                          span(class = "duration-badge", paste(row$Duration, "mins")))
             )
    )
  })
  
  # Generate Final Destination Dot
  last_leg <- journey_data[nrow(journey_data), ]
  final_line <- ifelse(is.na(last_leg$Line) || last_leg$Line == "", "Walking", last_leg$Line)
  final_color <- tube_colors[[final_line]]
  if(is.null(final_color)) final_color <- "#333"
  
  final_step <- tags$div(class = "journey-step",
                         tags$div(class = "graphic-col",
                                  tags$div(class = "station-dot", style = paste0("border-color: ", final_color, ";"))
                         ),
                         tags$div(class = "info-col",
                                  div(class = "station-name", last_leg$EndStation),
                                  div(class = "line-info", "Arrive")
                         )
  )
  
  # Return the bundle
  div(class = "journey-container", steps_html, final_step)
}