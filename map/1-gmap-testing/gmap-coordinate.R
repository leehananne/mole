# Simple test of google maps api - 2
# Markers on multiple locations
# Inputs as (Lat, Lon) coordinate, multiple locations separated as comma

library(shiny)
library(stringr) # <-- Added for coordinate parsing

# --- Configuration ---
# 1. API Key (Must have Maps JavaScript API enabled)
YOUR_API_KEY <- "AIzaSyCkp9eNSjWSoLJ_s0NX61yg21lcwCAaD8Q"

# 2. Your new custom Map ID
YOUR_MAP_ID <- "a3091a6195c7c2574ff8364a" 

# 3. Default location (now as a coordinate string)
DEFAULT_LOCATION <- "(51.49399987276369, -0.1739223108838274)"

# 4. Build the Google API script URL
google_api_url <- paste0(
  "https://maps.googleapis.com/maps/api/js?key=",
  YOUR_API_KEY,
  "&callback=initMap"
)

# --- UI (User Interface) ---
ui <- fluidPage(
  titlePanel("Dynamic Map with Coordinates"),
  
  tags$head(
    # CSS to make the map fill the main panel
    tags$style(HTML("
      #map-container {
        height: 600px; /* Set a fixed height */
        width: 100%;
      }
      #map {
        height: 100%;
        width: 100%;
      }
    ")),
    
    # --- JavaScript Logic ---
    tags$script(HTML(paste0("
      let map;
      let markers = []; // Use an array for multiple markers

      // --- 1. Create the Map (Called by Google API script) ---
      function initMap() {
        map = new google.maps.Map(document.getElementById('map'), {
          center: { lat: 51.5072, lng: -0.1276 }, // Default London
          zoom: 12,
          mapId: '", YOUR_MAP_ID, "' // Apply your custom style
        });
        
        // Tell Shiny that the map is ready for the default location
        Shiny.setInputValue('map_ready', true, {priority: 'event'});
      }

      // --- 2. Helper Function to Clear Old Markers ---
      function clearMarkers() {
        for (let i = 0; i < markers.length; i++) {
          markers[i].setMap(null); // Remove marker from map
        }
        markers = []; // Empty the array
      }
      
      // --- 3. Listen for Messages from Shiny ---
      // This handler now expects an ARRAY of {lat: ..., lng: ...} objects
      Shiny.addCustomMessageHandler('plotCoordinates', (coordinates) => {
        
        // 1. Clear any markers from the previous search
        clearMarkers();

        if (!Array.isArray(coordinates) || coordinates.length === 0) {
          return; // Nothing to plot
        }

        const bounds = new google.maps.LatLngBounds();

        // 2. Process all coordinates (NO GEOCODING NEEDED)
        coordinates.forEach(coord => {
          
          // Validate coordinate object
          if (typeof coord.lat !== 'number' || typeof coord.lng !== 'number' || 
              isNaN(coord.lat) || isNaN(coord.lng)) {
            console.warn('Invalid coordinate object received:', coord);
            return; // skip this one
          }

          // Create a LatLng object
          const latLng = new google.maps.LatLng(coord.lat, coord.lng);

          // Create a new marker
          const newMarker = new google.maps.Marker({
            position: latLng,
            map: map
          });
          
          markers.push(newMarker);
          
          // Expand the map bounds to include this new point
          bounds.extend(latLng);
        });

        // 3. Adjust the map view
        if (markers.length === 0) {
          // This case should be rare if we filter above, but good to have
          console.warn('No valid coordinates to plot.');
        
        } else if (markers.length === 1) {
          // If only one, center and zoom in close
          map.setCenter(bounds.getCenter());
          map.setZoom(15);
        
        } else {
          // If multiple, fit all markers on screen
          map.fitBounds(bounds);
        }
        
      }); // end addCustomMessageHandler
      
    "))) # End tags$script
  ), # End tags$head
  
  sidebarLayout(
    sidebarPanel(
      h4("Map Configuration"),
      p("Enter (Lat, Lng) pairs separated by commas."),
      
      textInput("location_text", 
                "Enter Coordinates:", 
                value = DEFAULT_LOCATION),
      
      actionButton("search_btn", "Search")
    ),
    
    mainPanel(
      h3("Styled Google Map"),
      # The HTML container where the map will be drawn
      div(id = "map-container",
          div(id = "map")
      )
    )
  ),
  
  # Load the Google Maps API script at the end of the body
  tags$script(src = google_api_url, async = TRUE, defer = TRUE)
)

# --- Server (Logic) ---
server <- function(input, output, session) {
  
  # --- Helper function to parse coordinate strings ---
  parseCoords <- function(text) {
    # Find all (lat, lng) pairs using regex
    # This captures the two numbers inside parentheses
    matches <- str_match_all(text, "\\(([^,]+),([^)]+)\\)")[[1]]
    
    if (length(matches) == 0 || nrow(matches) == 0) {
      return(list()) # Return empty list if no matches
    }
    
    # Create a list of named lists (will become JS objects)
    coords_list <- lapply(seq_len(nrow(matches)), function(i) {
      # matches[i, 1] is the full string "(lat, lng)"
      # matches[i, 2] is the lat string
      # matches[i, 3] is the lng string
      list(
        lat = as.numeric(trimws(matches[i, 2])),
        lng = as.numeric(trimws(matches[i, 3]))
      )
    })
    
    # Filter out any pairs that failed conversion (resulted in NA)
    coords_list <- Filter(function(x) !is.na(x$lat) && !is.na(x$lng), coords_list)
    
    return(coords_list)
  }
  
  # --- 1. Handle Search Button ---
  observeEvent(input$search_btn, {
    req(input$location_text)
    
    # Parse the text input into a list of coordinates
    coords_list <- parseCoords(input$location_text)
    
    # Send the list of coordinate objects to JavaScript
    # If list is empty, this will clear the markers
    session$sendCustomMessage("plotCoordinates", coords_list)
  })
  
  # --- 2. Handle Default Location ---
  observeEvent(input$map_ready, {
    
    # Parse the default location string
    coords_list <- parseCoords(DEFAULT_LOCATION)
    
    # Send the default coordinates
    session$sendCustomMessage("plotCoordinates", coords_list)
    
  }, once = TRUE) # 'once = TRUE' ensures this only runs one time
  
}

# Run the application
shinyApp(ui, server)