library(shiny)
source("4-station-autofill.R")
source("4-map-ui.R")

# --- Component Functions (UI Blocks) ---

component_station_search <- function() {
  tagList(
    h4("Station Search"),
    # Using the data loaded from 3-api-map.R
    selectizeInput("station_selector", "Find a Station:", 
                   choices = c("", sort(unique(station_map_data$name))),
                   options = list(placeholder = 'Type e.g. "Bank"', maxOptions = 3)),
    hr(),
    wellPanel(h5("Station Info"), p("Details will appear here..."))
  )
}

component_journey_planner <- function() {
  tagList(
    h4("Plan a Journey"),
    textInput("j_origin", "From:"),
    textInput("j_dest", "To:"),
    actionButton("plan_btn", "Go", class = "btn-primary width-100")
  )
}

component_bottom_overlay <- function() {
  div(id = "bottom-info-box",
      div(class = "box-header", h4("Live Insights"), icon("chevron-down")),
      div(class = "box-content", 
          checkboxInput("transit_toggle", "Show Transit Layer", value = TRUE), # Moved Toggle Here
          p("Predictive data...")
      )
  )
}

# ==============================================================================
# 2. MAIN UI
# ==============================================================================

ui <- fluidPage(
  class = "dashboard-container",
  
  tags$head(
    # 1. Inject Map CSS & JS Assets from sourced file
    map_assets, 
    
    # 2. Dashboard Specific CSS
    tags$style(HTML("
      html, body { height: 100%; margin: 0; padding: 0; overflow: hidden; }
      .dashboard-container { display: flex; height: 100vh; width: 100vw; }
      
      #left-panel { width: 25%; height: 100%; overflow-y: auto; background: #f8f9fa; padding: 15px; border-right: 1px solid #ddd; z-index: 10; }
      #right-panel { width: 75%; height: 100%; position: relative; }
      
      #map-container { width: 100%; height: 60%; } 
      
      #bottom-info-box { position: absolute; bottom: 0; left: 0; width: 100%; height: 40%; background: rgba(255,255,255,0.9); border-top: 2px solid #007bff; z-index: 20; display: flex; flex-direction: column; }
      .box-header { padding: 10px; background: #fff; border-bottom: 1px solid #eee; display: flex; justify-content: space-between; cursor: pointer; }
      .box-content { padding: 15px; overflow-y: auto; }
    "))
  ),
  
  # --- Left Panel ---
  div(id = "left-panel",
      tabsetPanel(
        tabPanel("Stations", br(), component_station_search()),
        tabPanel("Journey", br(), component_journey_planner())
      )
  ),
  
  # --- Right Panel ---
  div(id = "right-panel",
      # 1. Map Container
      div(id = "map-container",
          div(id = "map") # The Google Map injects here
      ),
      # 2. Bottom Overlay
      component_bottom_overlay()
  ),
  
  # Load Google API Script (Must be at end of body)
  google_api_script
)

# ==============================================================================
# 3. SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  # Initialize Map Logic (from 3-api-map.R)
  map_server_logic(input, output, session)
  
  # Other dashboard logic...
  observeEvent(input$station_selector, {
    # Future: Zoom map to selected station
  })
}

shinyApp(ui, server)