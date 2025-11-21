library(shiny)
source("4-station-autofill.R")

# ==============================================================================
# 1. COMPONENT FUNCTIONS
#    These functions separate the code into manageable blocks.
# ==============================================================================

# --- A. Left Panel: Station Search Tab ---
component_station_search <- function() {
  tagList(
    h4("Station Search"),
    
    # i. Station search field box (Placeholder for Selectize/Autocomplete)
    # selectInput("station_selector", "Find a Station:", 
    #             choices = NULL, # To be populated from server
    #             width = "100%",
    #             multiple = FALSE),
    
    station_search_ui("station_selector", c("Oxford Circus", "Waterloo", "Paddington", "Bank")),
    # the station inputs are temp
    
    hr(),
    
    # ii. Relevant station information block (TfL API placeholder)
    wellPanel(
      h5("Station Information"),
      uiOutput("station_info_ui"), # Dynamic content goes here
      helpText("Station facilities, accessibility, and status will appear here.")
    ),
    
    # iii. Relevant places UI block (Google Places API placeholder)
    wellPanel(
      h5("Nearby Places"),
      uiOutput("places_info_ui"),
      helpText("Restaurants, cafes, and landmarks will appear here.")
    )
  )
}

# --- B. Left Panel: Journey Planning Tab ---
component_journey_planner <- function() {
  tagList(
    h4("Plan a Journey"),
    
    # i. Origin and Destination fields
    textInput("journey_origin", "Origin:", placeholder = "From..."),
    textInput("journey_dest", "Destination:", placeholder = "To..."),
    actionButton("plan_route_btn", "Plan Route", class = "btn-primary", width = "100%"),
    
    hr(),
    
    # ii. Relevant journey information block
    div(class = "journey-results-container",
        h5("Journey Details"),
        uiOutput("journey_details_ui"),
        helpText("Route steps, timing, and disruptions will appear here.")
    )
  )
}

# --- C. Right Panel: Bottom Information Box (Predictive Info) ---
component_bottom_overlay <- function() {
  div(id = "bottom-info-box",
      div(class = "box-header", 
          h4("Live Insights"),
          actionButton("toggle_box", label = NULL, icon = icon("chevron-down"), class = "btn-xs")
      ),
      div(class = "box-content",
          fluidRow(
            column(6, 
                   h5("Crowding Levels"),
                   # Placeholder for Crowding API visualization
                   plotOutput("crowding_plot", height = "150px")
            ),
            column(6, 
                   h5("Local Weather"),
                   # Placeholder for Weather API
                   uiOutput("weather_widget")
            )
          )
      )
  )
}

# ==============================================================================
# 2. MAIN UI STRUCTURE
# ==============================================================================

ui <- fluidPage(
  # Remove default padding to ensure full screen app feel
  class = "dashboard-container",
  
  # --- CSS STYLING ---
  tags$head(
    tags$style(HTML("
      /* 1. General Layout: Full Viewport Height */
      html, body { height: 100%; margin: 0; padding: 0; overflow: hidden; }
      .container-fluid.dashboard-container { padding: 0; height: 100vh; display: flex; }

      /* 2. Left Panel: 25% Width, Scrollable */
      #left-panel {
        width: 25%;
        height: 100%;
        overflow-y: auto;
        background-color: #f8f9fa;
        border-right: 1px solid #ddd;
        padding: 15px;
        box-shadow: 2px 0 5px rgba(0,0,0,0.05);
        z-index: 10;
      }

      /* 3. Right Panel: 75% Width, Relative Positioning for Overlays */
      #right-panel {
        width: 75%;
        height: 100%;
        position: relative; /* Crucial for absolute positioning of children */
      }

      /* 4. Map Container: Fills the Right Panel */
      #map-container {
        width: 100%;
        height: 100%;
        background-color: #e9e9e9; /* Placeholder color for map */
      }
      
      /* Actual Google Map Div */
      #map { height: 100%; width: 100%; }

      /* 5. Bottom Overlay Box: Half height, aligned bottom */
      #bottom-info-box {
        position: absolute;
        bottom: 0;
        left: 0;
        width: 100%;
        height: 50%; /* As requested */
        background-color: rgba(255, 255, 255, 0.95);
        border-top: 2px solid #007bff;
        box-shadow: 0 -2px 10px rgba(0,0,0,0.1);
        transition: height 0.3s ease; /* Smooth toggle animation */
        z-index: 20;
        display: flex;
        flex-direction: column;
      }
      
      .box-header {
        padding: 10px 20px;
        background: #fff;
        border-bottom: 1px solid #eee;
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      
      .box-content {
        padding: 20px;
        overflow-y: auto;
        flex-grow: 1;
      }
    "))
  ),
  
  # --- UI BODY ---
  
  # A. Left Panel Container
  div(id = "left-panel",
      tabsetPanel(type = "tabs",
                  
                  # Tab 1: Station Search
                  tabPanel("Stations", 
                           br(),
                           component_station_search()
                  ),
                  
                  # Tab 2: Journey Planning
                  tabPanel("Journey", 
                           br(),
                           component_journey_planner()
                  )
      )
  ),
  
  # B. Right Panel Container
  div(id = "right-panel",
      
      # i. Map Layer (Full Height)
      div(id = "map-container",
          # We will inject the Google Map here later using the existing JS logic
          div(id = "map", h3("Google Map Will Load Here", style="text-align:center; padding-top: 200px; color: #999;"))
      ),
      
      # ii. Bottom Overlay Box
      component_bottom_overlay()
  )
)

# ==============================================================================
# 3. SERVER SKELETON
# ==============================================================================

server <- function(input, output, session) {
  
  # --- 1. Map Initialization Logic ---
  # (Placeholder: Load existing logic from previous files here)
  
  # --- 2. Left Panel Logic ---
  
  # Populate station choices (Mock data for now)
  observe({
    updateSelectInput(session, "station_selector")
  })
  
  # --- 3. Bottom Panel Logic ---
  
  # Placeholder for crowding plot
  output$crowding_plot <- renderPlot({
    plot(rnorm(10), type="l", main="Predicted Crowding (Mock)", col="blue", lwd=2)
  })
  
  # Optional: Collapse functionality for the bottom box
  observeEvent(input$toggle_box, {
    # We can add JS logic here later to toggle the height between 50% and 5%
    showNotification("Toggle visibility logic to be added via JS", type="message")
  })
}

shinyApp(ui, server)