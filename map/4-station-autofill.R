# Function to generate the Search UI
# Arguments:
#   id: The inputID for the shiny element (e.g., "search_box")
#   station_names: A character vector of station names (e.g., final_station_data$StationName)
station_search_ui <- function(id, station_names) {
  
  # We verify we have a valid list, otherwise provide a fallback
  choices_list <- if (is.null(station_names) || length(station_names) == 0) {
    character(0)
  } else {
    sort(unique(station_names))
  }
  
  selectizeInput(
    inputId = id,
    label = "Station Search",
    choices = c("", choices_list), # Add empty string for clean initial state
    selected = "",
    multiple = FALSE,
    width = "100%",
    options = list(
      placeholder = 'Station name',
      maxOptions = 5,      # <--- LIMITS DISPLAY TO TOP 3 MATCHES
      onInitialize = I('function() { this.setValue(""); }') # Ensure it starts empty
    )
  )
}