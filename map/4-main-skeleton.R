library(shiny)
library(dplyr)

# Ensure these files are in the same directory or adjust paths accordingly
source("4-station-autofill.R")
source("2-station-info-testing/facility-combined.R") 

# ==============================================================================
# 1. COMPONENT FUNCTIONS
# ==============================================================================

# --- A. Left Panel: Station Search Tab ---
component_station_search <- function() {
  tagList(
    h4("Station Search"),
    
    # i. Station search field box (using your custom UI function)
    station_search_ui("station_selector", station_table_data$StationName),
    
    hr(),
    
    # ii. Relevant station information block
    uiOutput("station_info_ui"), 
    
    # iii. Relevant places UI block
    br(),
    wellPanel(
      h5(icon("map-marker-alt"), " Nearby Places"),
      uiOutput("places_info_ui"),
      helpText("Restaurants, cafes, and landmarks will appear here.")
    )
  )
}

# --- B. Left Panel: Journey Planning Tab ---
component_journey_planner <- function() {
  tagList(
    h4("Plan a Journey"),
    textInput("journey_origin", "Origin:", placeholder = "From...", width = "100%"),
    textInput("journey_dest", "Destination:", placeholder = "To...", width = "100%"),
    actionButton("plan_route_btn", "Plan Route", class = "btn-primary", width = "100%"),
    hr(),
    div(class = "journey-results-container",
        h5("Journey Details"),
        uiOutput("journey_details_ui"),
        helpText("Route steps, timing, and disruptions will appear here.")
    )
  )
}

# --- C. Right Panel: Bottom Information Box ---
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
                   plotOutput("crowding_plot", height = "150px")
            ),
            column(6, 
                   h5("Local Weather"),
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
  class = "dashboard-container",
  
  tags$head(
    tags$style(HTML("
      html, body { height: 100%; margin: 0; padding: 0; overflow: hidden; }
      .dashboard-container { padding: 0; height: 100vh; display: flex; width: 100vw; }

      #left-panel {
        width: 25%;
        min-width: 300px; /* Prevent it getting too small */
        height: 100%;
        overflow-y: auto;
        background-color: #f8f9fa;
        border-right: 1px solid #ddd;
        padding: 20px;
        z-index: 10;
      }

      #right-panel {
        width: 75%;
        height: 100%;
        position: relative;
      }

      #map-container { width: 100%; height: 100%; background-color: #e9e9e9; }
      #map { height: 100%; width: 100%; }

      #bottom-info-box {
        position: absolute;
        bottom: 0;
        left: 0;
        width: 100%;
        height: 30%; 
        background-color: rgba(255, 255, 255, 0.95);
        border-top: 2px solid #007bff;
        box-shadow: 0 -2px 10px rgba(0,0,0,0.1);
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
      .box-content { padding: 20px; overflow-y: auto; flex-grow: 1; }
      
      /* Custom styles for Station Info Card */
      .station-card {
        background: white;
        border: 1px solid #ddd;
        border-radius: 5px;
        padding: 15px;
        margin-top: 10px;
        border-left: 5px solid #007bff;
      }
      .stat-row { display: flex; justify-content: space-between; margin-bottom: 8px; }
      .stat-label { font-weight: bold; color: #555; }
      .stat-val { font-weight: normal; color: #000; }
      .status-yes { color: green; font-weight: bold; }
      .status-no { color: #999; }
    "))
  ),
  
  # --- UI BODY ---
  div(id = "left-panel",
      tabsetPanel(type = "tabs",
                  tabPanel("Stations", br(), component_station_search()),
                  tabPanel("Journey", br(), component_journey_planner())
      )
  ),
  
  div(id = "right-panel",
      div(id = "map-container",
          div(id = "map", h3("Google Map Will Load Here", style="text-align:center; padding-top: 200px; color: #999;"))
      ),
      component_bottom_overlay()
  )
)

# ==============================================================================
# 3. SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {
  
  # --- 1. Map Initialization Logic (Placeholder) ---
  # map_server_logic(input, output, session) 
  
  # --- 2. Station Information Panel Logic ---
  output$station_info_ui <- renderUI({
    req(station_table_data) # Ensure data is loaded
    
    selected_name <- input$station_selector
    
    # If no station is selected, show a placeholder
    if (is.null(selected_name) || selected_name == "") {
      return(wellPanel(
        style = "background: #fff; color: #777; text-align: center; padding: 20px;",
        icon("subway", class = "fa-2x"),
        p("Select a station above to see details.")
      ))
    }
    
    # Find the station in the data
    # Note: Using 'head(1)' to handle potential duplicates safely
    station_data <- station_table_data %>% 
      filter(StationName == selected_name) %>% 
      head(1)
    
    if (nrow(station_data) == 0) return(NULL)
    
    # Extract values for display
    # Use backticks for "Interchange step-free" because of spaces
    has_step_free_interchange <- station_data[["Interchange step-free"]]
    
    # Create the UI Card
    div(class = "station-card",
        h3(station_data$StationName, style = "margin-top: 0; color: #007bff;"),
        hr(),
        
        # Lifts Row
        div(class = "stat-row",
            span(class = "stat-label", icon("elevator"), " Lifts Available:"),
            span(class = "stat-val", station_data$Lift)
        ),
        
        # Interchange Row
        div(class = "stat-row",
            span(class = "stat-label", icon("exchange-alt"), " Step-Free Interchange:"),
            span(class = if(isTRUE(has_step_free_interchange)) "stat-val status-yes" else "stat-val status-no",
                 if(isTRUE(has_step_free_interchange)) icon("check-circle") else icon("times-circle"),
                 if(isTRUE(has_step_free_interchange)) "Yes" else "No"
            )
        ),
        
        # Placeholder for future data (e.g. Zone)
        div(class = "stat-row",
            span(class = "stat-label", icon("map-pin"), " Naptan Code:"),
            span(class = "stat-val", station_data$NaptanCode)
        )
    )
  })
  
  # --- 3. Bottom Panel Logic ---
  output$crowding_plot <- renderPlot({
    plot(rnorm(10), type="l", main="Predicted Crowding (Mock)", col="blue", lwd=2)
  })
}

shinyApp(ui, server)