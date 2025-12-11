# ==============================================================================
# UI: MAIN COMPONENT BLOCKS
# ==============================================================================

# --- A. Left Panel Tab 1: Station Search Content ---
component_station_search <- function() {
  tagList(
    div(class = "mt-3",
        # Station search: Initialise with default station
        station_search_ui(id="station_selector",
                          choices_map = station_choices,
                          place_holder = "Station Search",
                          label_text = "Station Search: ",
                          default = default_naptan_code),
        
        hr(),
        
        # Information card 
        div(class = "card station-info-card mb-3",
            uiOutput("station_info_ui")
        ),
        
        # Comfort index and advice box
        mod_advice_ui("advice_box_1")
    )
  )
}

# --- B. Left Panel Tab 2: Journey Planning Content ---
component_journey_planner <- function() {
  tagList(
    div(class = "mt-3",
        # Origin / Destination search
        div(class = "card p-3 shadow-sm mb-3 bg-light",
            station_search_ui(id="origin_station",
                              choices_map = station_choices,
                              place_holder = "Origin Station",
                              label_text = "Origin Station: ",
                              default = default_origin_naptan_code),
            station_search_ui(id="destination_station",
                              choices_map = station_choices,
                              place_holder = "Destination Station",
                              label_text = "Destination Station: ",
                              default = default_destination_naptan_code)
        ),
        
        # Accordion for journey preferences
        accordion(
          id = "journey_preferences_accordion",
          open = FALSE, 
          multiple = FALSE,
          accordion_panel(
            title = "Journey Mode",
            icon = icon("route"),
            radioButtons("journey_preference", NULL,
                         choices = list(
                           "Fastest" = "LeastTime", 
                           "Least Walking" = "LeastWalking", 
                           "Fewest Changes" = "LeastInterchange"
                         ),
                         selected = "LeastTime")
          ),
          
          accordion_panel(
            title = "Accessibility",
            icon = icon("wheelchair"),
            radioButtons("accessibility_preference", NULL,
                         choices = list(
                           "None" = "NoRequirements", 
                           "Step-free to Train" = "StepFreeToVehicle",
                           "Step-free to Platform" = "StepFreeToPlatform",
                           "No Stairs" = "NoSolidStairs",
                           "No Escalators" = "NoEscalators",
                           "No Elevators" = "NoElevators"
                         ),
                         selected = "NoRequirements")
          )
        ),
        
        br(),
        
        actionButton("plan_journey", "Plan Journey", 
                     icon = icon("paper-plane"),
                     class = "btn-primary btn-lg w-100 shadow-sm"),
        
        div(class = "journey-results-container mt-3",
            h5("Suggested Route"),
            journey_router_ui("journeyRouteOutput")
        )
    )
  )
}

# --- C. Right Section: Insights Area ---
component_insights_panel <- function() {
  div(class = "insights-container",
      h6("Live Insights", class = "mb-3 text-muted"),
      
      # Logic to switch views based on active tab
      conditionalPanel(
        condition = "input.left_tabs == 'Stations'",
        fluidRow(
          column(6, 
                 div(class = "chart-box",
                     uiOutput("station_crowding_title"),
                     plotOutput("crowding_plot", height = "300px")
                 )
          ),
          column(6, 
                 div(class = "chart-box",
                     h5("Weather and Comfort Forecast"),
                     mod_comfort_ui("comfort_1")
                 )
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.left_tabs == 'Journey'",
        fluidRow(
          column(6, 
                 div(class = "chart-box",
                     uiOutput("origin_crowding_title"),
                     plotOutput("origin_crowding_plot", height = "300px")
                 )
          ),
          column(6, 
                 div(class = "chart-box",
                     uiOutput("destination_crowding_title"),
                     plotOutput("destination_crowding_plot", height = "300px")
                 )
          )
        )
      )
  )
}


# ==============================================================================
# 2. MAIN UI - ALL COMBINED
# ==============================================================================

ui <- fluidPage(
  theme = bs_theme(version = 5, preset = "zephyr"),
  
  # Inject Map Assets and Custom CSS
  tags$head(
    if(exists("map_assets")) map_assets else tags$div(), 
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  # --- Layout Structure ---
  # 1. FIXED LEFT SIDEBAR
  div(id = "dashboard",
    div(class = "sidebar-fixed",
        # Branding Header
        div(class = "brand-header",
            div(
              span("MOLE", class = "brand-title"),
              span("Underground Comfort Assistant", class = "brand-sub"),
              mod_profile_badge_ui("profile_badge_1")
            )
        ),
        
        # Tabs
        div(class = "custom-tabs",
            tabsetPanel(
              id = "left_tabs",
              type = "pills",
              tabPanel("Stations", component_station_search()),
              tabPanel("Journey", component_journey_planner())
            )
        )
    ),
    
    # 2. SCROLLABLE RIGHT CONTENT
    div(class = "main-content",
        
        # Map Section
        div(class = "map-wrapper",
            div(id = "map") # Google Map Target
        ),
        
        # Insights Section (Flows naturally below map)
        component_insights_panel()
    ),
  ),
  
  # Load Google API Script
  if(exists("google_api_script")) google_api_script else tags$script()
)