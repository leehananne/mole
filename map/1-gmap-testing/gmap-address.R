# Simple test of google maps api - 1
# Markers on entered location address
# Inputs as address (postcodes, names, etc) -> gmap API to geocode the address

library(shiny)

# --- Configuration ---
# 1. API Key (Must have Maps JavaScript API & Geocoding API enabled)
YOUR_API_KEY <- "AIzaSyCkp9eNSjWSoLJ_s0NX61yg21lcwCAaD8Q"

# 2. Your new custom Map ID
YOUR_MAP_ID <- "a3091a6195c7c2574ff8364a" 

# 3. Default location
DEFAULT_LOCATION <- "SW7 2AZ"

# 4. Build the Google API script URL
#    &callback=initMap tells the API to run our 'initMap' function once loaded
google_api_url <- paste0(
  "https://maps.googleapis.com/maps/api/js?key=",
  YOUR_API_KEY,
  "&callback=initMap"
)

# --- UI (User Interface) ---
ui <- fluidPage(
  titlePanel("Dynamic Map with Custom Style"),
  
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
      let marker;
      let geocoder;

      // --- 1. Create the Map (Called by Google API script) ---
      function initMap() {
        geocoder = new google.maps.Geocoder();
        map = new google.maps.Map(document.getElementById('map'), {
          center: { lat: 51.5072, lng: -0.1276 }, // Default London
          zoom: 12,
          mapId: '", YOUR_MAP_ID, "' // Apply your custom style
        });
        
        // Create a single, reusable marker
        marker = new google.maps.Marker({
          map: map,
          visible: false // Start as invisible
        });
        
        // Tell Shiny that the map is ready for the default location
        Shiny.setInputValue('map_ready', true, {priority: 'event'});
      }

      // --- 2. Geocode Location Function ---
      // This function finds a lat/lon from a text string
      function findAndMoveMap(location) {
        if (!geocoder) return; // Don't run if map isn't ready
        
        geocoder.geocode({ 'address': location }, (results, status) => {
          if (status === 'OK') {
            
            // --- *** UPDATED CODE *** ---
            // Center the map on the result's location
            map.setCenter(results[0].geometry.location);
            // Set a fixed, close-up zoom level
            map.setZoom(15); 
            // We removed map.fitBounds() to force this new centered zoom
            
            // Move the marker
            marker.setPosition(results[0].geometry.location);
            marker.setVisible(true);
            
          } else {
            // If geocode fails, just log an error
            console.warn('Geocode was not successful: ' + status);
          }
        });
      }

      // --- 3. Listen for Messages from Shiny ---
      // This handler will be called by our server logic
      Shiny.addCustomMessageHandler('geocodeLocation', (location) => {
        findAndMoveMap(location);
      });
      
    "))) # End tags$script
  ), # End tags$head
  
  sidebarLayout(
    sidebarPanel(
      h4("Map Configuration"),
      p("Enter a location and click 'Search' to update the map."),
      
      textInput("location_text", 
                "Enter Location:", 
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
  
  # --- 1. Handle Search Button ---
  # This runs when the user clicks 'Search'
  observeEvent(input$search_btn, {
    req(input$location_text)
    # Send the new location to JavaScript
    session$sendCustomMessage("geocodeLocation", input$location_text)
  })
  
  # --- 2. Handle Default Location ---
  # We wait for the 'map_ready' signal from JS (in initMap)
  observeEvent(input$map_ready, {
    # Send the default location to be geocoded
    session$sendCustomMessage("geocodeLocation", DEFAULT_LOCATION)
  }, once = TRUE) # 'once = TRUE' ensures this only runs one time
  
}

# Run the application
shinyApp(ui, server)