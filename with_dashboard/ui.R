library(shiny)
source("components/4-station-autofill.R")
source("components/4-map-ui.R")
source("components/5-journey-output.R")
source("components/6-station-crowd-plot.R")

# --- Component Functions (UI Blocks) ---

# --- A. Left Panel: Station Search Tab ---
component_station_search <- function() {
  tagList(
    h4("Station Search"),
    
    # i. Station search field box (Placeholder for Selectize/Autocomplete)
    # selectInput("station_selector", "Find a Station:", 
    #             choices = NULL, # To be populated from server
    #             width = "100%",
    #             multiple = FALSE),
    
    station_search_ui("station_selector", 
                     if(exists("station_master_data") && is.data.frame(station_master_data) && nrow(station_master_data) > 0) {
                       station_master_data$StationName
                     } else if(exists("station_table_data") && is.data.frame(station_table_data) && nrow(station_table_data) > 0) {
                       station_table_data$StationName
                     } else {
                       character(0)
                     }),
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
    selectInput("origin_station", "Origin Station:",
                choices = station_choices,
                selected = if(exists("default_naptan_code")) default_naptan_code else NULL,
                width = "100%"),
    selectInput("destination_station", "Destination Station:",
                choices = station_choices,
                selected = if(exists("default_destination_naptan_code")) default_destination_naptan_code else NULL,
                width = "100%"),
    
    hr(),
    
    # Journey preferences
    h5("Journey Preference:"),
    radioButtons("journey_preference", NULL,
                choices = list("Least Interchange" = "LeastInterchange",
                              "Least Time" = "LeastTime",
                              "Least Walking" = "LeastWalking"),
                selected = "LeastTime",
                inline = FALSE),
    
    hr(),
    
    # Accessibility preferences
    h5("Accessibility Preference:"),
    radioButtons("accessibility_preference", NULL,
                choices = list("No Requirements" = "NoRequirements",
                              "No Solid Stairs" = "NoSolidStairs",
                              "No Escalators" = "NoEscalators",
                              "No Elevators" = "NoElevators",
                              "Step Free to Vehicle" = "StepFreeToVehicle",
                              "Step Free to Platform" = "StepFreeToPlatform"),
                selected = "NoRequirements",
                inline = FALSE),
    
    hr(),
    
    actionButton("plan_journey", "Plan Journey", class = "btn-primary", width = "100%"),
    
    hr(),
    
    # ii. Relevant journey information block
    div(class = "journey-results-container",
        h5("Journey Route"),
        journey_router_ui("journeyRouteOutput")
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
          # Main crowding plot (for Stations tab)
          conditionalPanel(
            condition = "input.left_tabs == 'Stations'",
            fluidRow(
              column(6, 
                     uiOutput("station_crowding_title"),
                     plotOutput("crowding_plot", height = "350px", width = "100%")
              ),
              column(6, 
                     h5("Local Weather"),
                     uiOutput("weather_widget")
              )
            )
          ),
          # Journey tab: Origin and Destination crowding plots
          conditionalPanel(
            condition = "input.left_tabs == 'Journey'",
            fluidRow(
              column(6, 
                     uiOutput("origin_crowding_title"),
                     plotOutput("origin_crowding_plot", height = "350px", width = "100%")
              ),
              column(6, 
                     uiOutput("destination_crowding_title"),
                     plotOutput("destination_crowding_plot", height = "350px", width = "100%")
              )
            )
          )
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
        id = "left_tabs",
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