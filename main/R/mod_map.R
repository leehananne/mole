# ==============================================================================
# MAP MODULE
#   - Calls Google Maps API
#   - Handles Single Station Pinning
#   - Handles Journey (Origin -> Dest) Pinning & Routing
# ==============================================================================

# 1. MAP CONFIGURATION & ASSETS
# ------------------------------------------------------------------------------

map_assets <- tagList(
  # CSS
  tags$style(HTML("
      #map-container { height: 100%; width: 100%; position: relative; }
      #map { height: 100%; width: 100%; min-height: 500px; }
  ")),
  
  # Google Maps Script
  tags$script(src = paste0("https://maps.googleapis.com/maps/api/js?key=", GMAP_API, "&callback=initMap"), async = TRUE, defer = TRUE),
  
  # Custom Map Logic
  tags$script(HTML(paste0("
      let map;
      let selectedMarker = null;  // For single station selection
      let journeyLine = null;     // For the route line
      let journeyMarkers = [];    // For Origin/Dest markers
      let transitLayer = null;

      function initMap() {
        map = new google.maps.Map(document.getElementById('map'), {
          center: { lat: 51.5072, lng: -0.1276 }, // London Center
          zoom: 12,
          mapId: '", MAP_ID, "',
          disableDefaultUI: true,
          zoomControl: true,
          streetViewControl: false,
          mapTypeControl: false
        });
        
        transitLayer = new google.maps.TransitLayer();
        transitLayer.setMap(map);
        
        // Signal to Shiny that map is loaded
        Shiny.setInputValue('map_ready', true, {priority: 'event'});
      }
      
      // --- Helper: Clear Journey Elements ---
      function clearJourney() {
         if (journeyLine) journeyLine.setMap(null);
         journeyMarkers.forEach(m => m.setMap(null));
         journeyMarkers = [];
      }
      
      // --- Handler: Toggle Transit Layer ---
      // Receives true (Show) or false (Hide)
      Shiny.addCustomMessageHandler('toggleTransit', function(show) {
        if (!transitLayer || !map) return;
        transitLayer.setMap(show ? map : null);
      });

      // --- Handler: Highlight a Single Station ---
      Shiny.addCustomMessageHandler('highlightStation', function(data) {
        if (!map || !data) return;
        
        // 1. Clear previous selection & journey
        if (selectedMarker) selectedMarker.setMap(null);
        clearJourney();

        // 2. Create new marker
        const pos = { lat: data.lat, lng: data.lon };
        
        selectedMarker = new google.maps.Marker({
          position: pos,
          map: map,
          title: data.name,
          animation: google.maps.Animation.DROP
        });
        
        // 3. Zoom and Pan
        map.panTo(pos);
        map.setZoom(15);
      });

      // --- Handler 2: Draw Journey (Origin -> Dest) ---
      Shiny.addCustomMessageHandler('drawJourney', function(data) {
        if (!map || !data) return;
        
        // 1. Clear single station marker & previous journey
        if (selectedMarker) selectedMarker.setMap(null);
        clearJourney();
        
        const origin = { lat: data.orig_lat, lng: data.orig_lon };
        const dest =   { lat: data.dest_lat, lng: data.dest_lon };

        // 2. Add Markers (A and B)
        const mk1 = new google.maps.Marker({
            position: origin, 
            map: map, 
            label: 'A',
            title: 'Origin: ' + data.orig_name
        });
        
        const mk2 = new google.maps.Marker({
            position: dest, 
            map: map, 
            label: 'B',
            title: 'Destination: ' + data.dest_name
        });
        
        journeyMarkers.push(mk1, mk2);

        // 3. Draw Line (Dashed Polyline)
        const lineSymbol = {
          path: 'M 0,-1 0,1',
          strokeOpacity: 1,
          scale: 4
        };

        journeyLine = new google.maps.Polyline({
          path: [origin, dest],
          geodesic: true,
          strokeColor: '#007bff', 
          strokeOpacity: 0,       
          icons: [{
            icon: lineSymbol,
            offset: '0',
            repeat: '20px'
          }],
          map: map
        });

        // 4. Fit Map Bounds
        const bounds = new google.maps.LatLngBounds();
        bounds.extend(origin);
        bounds.extend(dest);
        
        // Add padding so pins aren't on the edge
        map.fitBounds(bounds, { top: 50, bottom: 50, left: 50, right: 50 });
      });
  ")))
)


# 2. SERVER LOGIC
# ------------------------------------------------------------------------------
# This function should be called inside your main server.R
# Arguments:
#   station_df: The master dataframe containing (StationName, Latitude, Longitude, NaptanCode)
#   selected_station_id: Reactive returning the NaptanCode of the selected station (from dropdown)
#   journey_data: Reactive returning a list/row with (orig_lat, orig_lon, dest_lat, dest_lon, etc.)

map_server_logic <- function(input, output, session, station_df, selected_station_id, journey_data, active_tab) {
  
  # --- Manage Transit Layer visibility based on Tab ---
  observe({
    req(active_tab())
    # If tab is "Stations", show layer (TRUE). If "Journey", hide it (FALSE).
    should_show <- (active_tab() == "Stations")
    session$sendCustomMessage("toggleTransit", should_show)
  })
  
  # --- Logic 1: Highlight Selected Station ---
  # Only runs if "Stations" tab is active AND a station is selected
  observe({
    req(active_tab() == "Stations")
    req(selected_station_id())
    
    # Filter data
    target <- station_df %>% 
      filter(NaptanCode == selected_station_id()) %>% 
      head(1)
    
    if (nrow(target) > 0) {
      session$sendCustomMessage("highlightStation", list(
        name = target$StationName,
        lat  = target$Latitude,
        lon  = target$Longitude
      ))
    }
  })
  
  # --- Logic 2: Draw Journey ---
  # Only runs if "Journey" tab is active AND journey data exists
  observe({
    req(active_tab() == "Journey")
    req(journey_data())
    
    # Expecting journey_data() to return a list
    data <- journey_data()
    
    session$sendCustomMessage("drawJourney", list(
      orig_name = data$OriginName,
      orig_lat  = data$OriginLat,
      orig_lon  = data$OriginLon,
      dest_name = data$DestName,
      dest_lat  = data$DestLat,
      dest_lon  = data$DestLon
    ))
  })
}