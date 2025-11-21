# Google maps combined with API data
# Displays the map with station markers and transit layers

library(shiny)
library(httr)     # <-- ADDED for API
library(jsonlite) # <-- ADDED for API
library(stringr)  # For splitting the coordinates string
library(dplyr)    # For data manipulation
# library(DT) has been removed

source("2-api-lifts.R")

station_map_data <- station_table_data %>%
  select(
    name = StationName,
    lat = Latitude,
    lon = Longitude
  )


# --- Configuration ---
YOUR_API_KEY <- "AIzaSyCkp9eNSjWSoLJ_s0NX61yg21lcwCAaD8Q"
YOUR_MAP_ID <- "a3091a6195c7c2574ff8364a" 
google_api_url <- paste0(
  "https://maps.googleapis.com/maps/api/js?key=",
  YOUR_API_KEY,
  "&callback=initMap"
)

# --- UI (User Interface) ---
ui <- fluidPage(
  titlePanel("London Station Map"), # Title updated
  
  tags$head(
    # CSS
    tags$style(HTML("
      #map-container {
        height: 75vh;
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
      let infowindow;
      let stationsPlotted = false; 
      let markers = []; 
      let transitLayer; 

      // --- 1. Create the Map ---
      function initMap() {
        console.log('initMap called');
        infowindow = new google.maps.InfoWindow();
        
        map = new google.maps.Map(document.getElementById('map'), {
          center: { lat: 51.5072, lng: -0.1276 }, 
          zoom: 12,
          mapId: '", YOUR_MAP_ID, "'
        });
        
        transitLayer = new google.maps.TransitLayer(); 
        
        Shiny.setInputValue('map_ready', true, {priority: 'event'});
      }
      
      // --- 2. Function to plot all stations (unchanged) ---
      Shiny.addCustomMessageHandler('plotStations', function(stationData) {
        if (!map || stationsPlotted) {
          console.log('Map not ready or stations already plotted.');
          return; 
        }
        
        if (typeof stationData !== 'object' || stationData === null || 
            !Array.isArray(stationData.name) || 
            !Array.isArray(stationData.lat) || 
            !Array.isArray(stationData.lon)) {
          console.warn('Invalid station data format received from R.');
          return;
        }
        
        if (stationData.name.length === 0) {
          console.warn('No station data received.');
          return;
        }
        
        const stations = [];
        for (let i = 0; i < stationData.name.length; i++) {
          stations.push({
            name: stationData.name[i],
            lat: stationData.lat[i],
            lon: stationData.lon[i]
          });
        }

        stationsPlotted = true; 
        console.log('Plotting ' + stations.length + ' stations.');
        
        const bounds = new google.maps.LatLngBounds();
        let markersAdded = 0;

        stations.forEach(station => {
          if (typeof station.lat !== 'number' || typeof station.lon !== 'number' || 
              isNaN(station.lat) || isNaN(station.lon)) {
            console.warn('Invalid station object received:', station);
            return; 
          }

          const latLng = new google.maps.LatLng(station.lat, station.lon); 

          const stationMarker = new google.maps.Marker({
            position: latLng,
            map: map,
            title: station.name
          });
          
          stationMarker.addListener('click', () => {
            infowindow.setContent('<strong>' + station.name + '</strong>');
            infowindow.open(map, stationMarker);
          });
          
          markers.push(stationMarker); 
          
          bounds.extend(latLng);
          markersAdded++;
        });
        
        console.log('--- Markers Array ---', markers);

        if (markersAdded === 0) {
          console.warn('No valid stations were plotted.');
        } else if (markersAdded === 1) {
          map.setCenter(bounds.getCenter());
          map.setZoom(14); 
        } else {
          console.log('Fitting map bounds to show all markers.');
          map.fitBounds(bounds);
        }
      });
      
      // --- 3. Handle Tab Click to Resize Map (REMOVED) ---
      
      // --- 4. Handle Transit Layer Toggle (now #3) ---
      Shiny.addCustomMessageHandler('toggleTransitLayer', function(show) {
        if (map && transitLayer) {
          if (show) {
            transitLayer.setMap(map); // Show the layer
          } else {
            transitLayer.setMap(null); // Hide the layer
          }
        }
      });
      
    "))) # End tags$script
  ), # End tags$head
  
  mainPanel(
    width = 12,
    
    checkboxInput("transit_toggle", "Show Transit Layer", value = TRUE),
    hr(),
    
    # The HTML container where the map will be drawn
    div(id = "map-container",
        div(id = "map")
    )
  ),
  
  # Load the Google Maps API script
  tags$script(src = google_api_url, async = TRUE, defer = TRUE)
)

# --- Server (Logic) ---
server <- function(input, output, session) {
  
  observeEvent(input$map_ready, {
    # Send the pre-engineered map data to JS to be plotted
    session$sendCustomMessage("plotStations", station_map_data)
  }, once = TRUE) # Only need to do this once
  
  observeEvent(input$transit_toggle, {
    # Send the TRUE/FALSE value to JavaScript
    session$sendCustomMessage("toggleTransitLayer", input$transit_toggle)
  }, ignoreNULL = FALSE) # 'ignoreNULL = FALSE' sends the initial 'FALSE' on load
  
}

# Run the application
shinyApp(ui, server)