# 3-api-map.R
# This file handles Data Fetching, Map Configuration, and Map Logic.
# It is designed to be sourced by dashboard_structure.R

library(shiny)
library(httr)
library(jsonlite)
library(stringr)
library(dplyr)

# ==============================================================================
# 1. DATA PROCESSING
#    (Consolidated from previous steps to ensure this file is self-contained)
# ==============================================================================

source("2-api-lifts.R")

station_map_data <- station_table_data %>%
  select(
    name = StationName,
    lat = Latitude,
    lon = Longitude
  )

# ==============================================================================
# 2. MAP CONFIGURATION & ASSETS
# ==============================================================================

YOUR_API_KEY <- "AIzaSyCkp9eNSjWSoLJ_s0NX61yg21lcwCAaD8Q"
YOUR_MAP_ID <- "a3091a6195c7c2574ff8364a" 

google_api_script <- tags$script(
  src = paste0("https://maps.googleapis.com/maps/api/js?key=", YOUR_API_KEY, "&callback=initMap"),
  async = TRUE, defer = TRUE
)

# Define the CSS and JS as a UI object we can insert into tags$head()
map_assets <- tagList(
  # CSS
  tags$style(HTML("
      #map-container { height: 100%; width: 100%; }
      #map { height: 100%; width: 100%; }
  ")),
  
  # JavaScript
  tags$script(HTML(paste0("
      let map;
      let markers = []; 
      let transitLayer; 

      function initMap() {
        map = new google.maps.Map(document.getElementById('map'), {
          center: { lat: 51.5072, lng: -0.1276 }, 
          zoom: 12,
          mapId: '", YOUR_MAP_ID, "',
          disableDefaultUI: true, // Cleaner look for dashboard
          zoomControl: true
        });
        
        transitLayer = new google.maps.TransitLayer(); 
        // transitLayer.setMap(map); // Optional: Enable by default if desired
        
        Shiny.setInputValue('map_ready', true, {priority: 'event'});
      }
      
      Shiny.addCustomMessageHandler('plotStations', function(stations) {
        if (!map) return;
        
        // Clear old markers
        markers.forEach(m => m.setMap(null));
        markers = [];

        const bounds = new google.maps.LatLngBounds();
        
        stations.forEach(s => {
          const marker = new google.maps.Marker({
            position: {lat: s.lat, lng: s.lon},
            map: map,
            title: s.name
          });
          markers.push(marker);
          bounds.extend(marker.getPosition());
        });
        
        // Smart Zoom: Don't zoom out too far if showing all London
        if (stations.length > 1) {
           // Optional: map.fitBounds(bounds); 
        }
      });
      
      Shiny.addCustomMessageHandler('toggleTransitLayer', function(show) {
        if (map && transitLayer) {
          show ? transitLayer.setMap(map) : transitLayer.setMap(null);
        }
      });
  ")))
)

# ==============================================================================
# 3. SERVER LOGIC FUNCTION
#    This function is called inside the main server() to activate map logic
# ==============================================================================

map_server_logic <- function(input, output, session) {
  
  # Plot stations when map is ready
  observeEvent(input$map_ready, {
    session$sendCustomMessage("plotStations", station_map_data)
  }, once = TRUE)
  
  # Transit toggle logic (Assumes input$transit_toggle exists in UI)
  observeEvent(input$transit_toggle, {
    session$sendCustomMessage("toggleTransitLayer", input$transit_toggle)
  }, ignoreNULL = FALSE)
}