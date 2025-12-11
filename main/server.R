# ==============================================================================
# server.R
# Contains main server
# ==============================================================================

server <- function(input, output, session) {
  
  # === Modal Popup: User Preference (Initial and On Click) === 
  click_trigger <- reactiveVal(0) # Bridge variable
  
  # Initial Modal Trigger
  user_data <- mod_traveler_modal_server(
    id = "user_modal", 
    user_profiles = user_profiles, 
    trigger_open  = reactive(click_trigger()) # Pass the bridge
  )
  
  # Button Modal Trigger
  badge_click <- mod_profile_badge_server("profile_badge_1", user_data$key)
  
  observeEvent(badge_click(), {
    click_trigger(click_trigger() + 1)
  })
  
  user_weights <- user_data$weights
  
  
  # === Selected Station Info Data ===
  # Identifying Selected Station Information
  current_station <- reactive({
    req(input$station_selector)
    station_master_data %>%
      filter(NaptanCode == input$station_selector)
  })
  
  # Data from Currently Selected Station
  station_naptan <- reactive({
    req(current_station())
    current_station()$NaptanCode
  })
  
  station_latitude <- reactive({
    req(current_station())
    current_station()$Latitude
  })
  
  station_longitude <- reactive({
    req(current_station())
    current_station()$Longitude
  })
  
  station_name <- reactive({
    req(current_station())
    current_station()$StationName
  })
  
  # === Selected Station Forecast Data ===
  # A. Weather Forecast
  weather_df <- reactive({
    get_weather_forecast(GMAP_API, station_latitude(), station_longitude(), 3)
  })
  
  weather_fc <- reactive({
    get_weather_forecast(GMAP_API, station_latitude(), station_longitude(), 3)
  })
  
  # B. Crowd Forecasts - Hourly / 15 Minute
  crowd_df_2 <- reactive({
    get_hourly_crowd_forecast(station_naptan(), 4)
  })
  
  crowd_hourly_fc <- reactive({
    get_hourly_crowd_forecast(station_naptan(), 4)
  })
  
  crowd_df <- reactive({
    get_15min_crowd_forecast(station_naptan(), 4)
  })
  
  crowd_15min_fc <- reactive({
    get_15min_crowd_forecast(station_naptan(), 4)
  })
  
  # C. Live Disruptions - Line Delay / Access
  disrupt_delay <- reactive({
    interpret_delay(station_naptan())
  })
  
  disrupt_access <- reactive({
    interpret_access(station_naptan())
  })
  
  # D. Accessibility 
  access_data <- reactive({
    get_station_accessibility(station_naptan(), disrupt_access(), station_master_data)
  })
  
  
  # === Selected Station Comfort Data ===
  station_comfort <- reactive({
    calculate_comfort_index(weather_fc(), crowd_hourly_fc(), access_data(), disrupt_delay(), user_weights())
  })
  
  mole_insight <- reactive({
    generate_prescriptive_suggestions(station_comfort(), weather_fc(), crowd_15min_fc())
  })
  
  
  # === Selected Station Comfort Servers ===
  mod_comfort_server("comfort_1", station_comfort)
  mod_advice_server("advice_box_1", station_comfort, mole_insight)
  
  
  # == Map Server ==
  map_server_logic(
    input, output, session,
    station_df = station_master_data,
    
    selected_station_id = reactive(input$station_selector),
    journey_data = current_journey
  )
  
  # ============================================================================
  # Station info ui card
  output$station_info_ui <- renderUI({
    req(input$station_selector)
    station_card_ui(input$station_selector, station_master_data, station_toilet_data)
  })
  
  # === Reactive for Journey Data ===
  # This triggers when a user clicks a "Search" button
  current_journey <- eventReactive(input$search_button, {
    req(input$origin_select, input$dest_select)
    
    # Lookup lat/lons for origin and dest
    orig <- station_master_data %>% filter(NaptanCode == input$origin_select)
    dest <- station_master_data %>% filter(NaptanCode == input$dest_select)
    
    list(
      OriginName = orig$StationName,
      OriginLat  = orig$Latitude,
      OriginLon  = orig$Longitude,
      DestName   = dest$StationName,
      DestLat    = dest$Latitude,
      DestLon    = dest$Longitude
    )
  })
  
  # === JOURNEY PLANNER LOGIC ===
  journey_route_data <- eventReactive(input$plan_journey, {
    message("\n>>> Journey Planner Button Clicked <<<")
    message("Input origin: ", input$origin_station)
    message("Input destination: ", input$destination_station)
    
    req(input$origin_station, input$destination_station)
    
    if (input$origin_station == input$destination_station) {
      message("ERROR: Same origin and destination")
      showNotification("Origin and destination stations must be different", type = "warning", duration = 3)
      return(NULL)
    }
    
    # Store station names at the time of button click
    origin_name <- names(station_choices[station_choices == input$origin_station])[1] %||% input$origin_station
    dest_name <- names(station_choices[station_choices == input$destination_station])[1] %||% input$destination_station
    
    # Get preferences from UI inputs (with defaults if not set)
    access_pref <- input$accessibility_preference %||% "NoRequirements"
    journey_pref <- input$journey_preference %||% "LeastTime"
    
    message("Calling fetch_journey_api()...")
    message("  Accessibility preference: ", access_pref)
    message("  Journey preference: ", journey_pref)
    tryCatch({
      parsed_data <- fetch_journey_api(input$origin_station, input$destination_station, 
                                       access_pref, journey_pref)
      message(">>> Journey route fetched successfully! <<<\n")
      # Return a list with both parsed data and station names
      return(list(
        parsed_data = parsed_data,
        origin_name = origin_name,
        dest_name = dest_name
      ))
    }, error = function(e) {
      message("Error fetching journey: ", e$message)
      showNotification(
        paste("Failed to fetch journey route from", origin_name, "to", dest_name, ". Please check the station codes or try again."),
        type = "error",
        duration = 8
      )
      return(NULL)
    })
  })
  
  
  # === Journey Output Render UI ===
  output$journeyRouteOutput <- renderUI({
    message("\n>>> Rendering journey route output <<<")
    
    # Force dependency on the button click AND the station inputs
    # This ensures re-rendering even if the reactive returns cached data
    input$plan_journey  # Force dependency on button
    input$origin_station  # Force dependency on origin
    input$destination_station  # Force dependency on destination
    
    # Clear any previous output - start fresh
    journey_result <- journey_route_data()
    
    if (is.null(journey_result)) {
      message("  Journey result is NULL - showing default message")
      return(div(class = "journey-container", 
                 p("Select origin and destination stations, then click 'Plan Journey' to find a route.")))
    }
    
    # Extract parsed data and station names from the stored result
    parsed_data <- journey_result$parsed_data
    origin_name <- journey_result$origin_name
    dest_name <- journey_result$dest_name
    
    tryCatch({
      # Clear any previous state variables
      journey_details <- NULL
      journey_data_formatted <- NULL
      
      # Get journey details for the first journey
      message("  Step 1: Extracting journey details...")
      journey_details <- extract_journey_details(parsed_data, journey_index = 1)
      
      # Validate journey_details before proceeding
      if (is.null(journey_details) || !is.data.frame(journey_details) || nrow(journey_details) == 0) {
        stop("Invalid journey details: empty or null data frame")
      }
      message("  Step 1 complete: Journey details extracted, ", nrow(journey_details), " rows")
      
      # Transform journey_details to format expected by generate_journey_html()
      message("  Step 2: Transforming journey details...")
      journey_data_formatted <- tryCatch({
        transform_journey_details(journey_details)
      }, error = function(e) {
        message("  ERROR in transform_journey_details: ", e$message)
        stop(paste("Transform error:", e$message))
      })
      
      # Validate transformed data
      if (is.null(journey_data_formatted) || !is.data.frame(journey_data_formatted) || nrow(journey_data_formatted) == 0) {
        stop("Invalid transformed journey data: empty or null data frame")
      }
      message("  Step 2 complete: Data transformed, ", nrow(journey_data_formatted), " rows")
      
      # Generate HTML using the component function (clears its own state internally)
      message("  Step 3: Generating HTML...")
      journey_html <- tryCatch({
        generate_journey_html(journey_data_formatted)
      }, error = function(e) {
        message("  ERROR in generate_journey_html: ", e$message)
        stop(paste("HTML generation error:", e$message))
      })
      message("  Step 3 complete: HTML generated")
      
      # Add journey summary header
      num_legs <- attr(journey_details, "num_legs")
      if (is.null(num_legs)) {
        num_legs <- nrow(journey_details)
      }
      total_duration <- sum(journey_details$duration, na.rm = TRUE)
      
      summary_header <- div(
        style = "margin-bottom: 15px; background-color: #f8f9fa; border-radius: 5px;",
        p(style = "margin-bottom: 0; font-size: 12px; color: #666;",
          paste("Total Duration: ", total_duration, " minutes | ", "Number of Legs: ", num_legs))
      )
      
      message("  Output formatted successfully")
      message("  Number of legs: ", num_legs)
      message(">>> Rendering complete <<<\n")
      return(tagList(summary_header, journey_html))
      
    }, error = function(e) {
      message("  Error extracting journey details: ", e$message)
      message("  Error traceback: ", paste(capture.output(traceback()), collapse = "\n"))
      return(div(class = "journey-container", 
                 p(style = "color: red;", paste("Error processing journey route:", e$message))))
    })
  })


  # === STATION CROWDING TAB ===
  crowd_naptan_reactive <- eventReactive(input$crowd_update, {
    req(input$crowd_naptan)
    trimws(input$crowd_naptan)
  })

  output$crowding_tab_plot <- renderPlot({
    naptan <- crowd_naptan_reactive()
    validate(
      need(naptan != "", "Please enter a valid Naptan station code.")
    )
    plot_crowd(naptan)
  })

  # ==============================================================================
  # JOURNEY TAB CROWDING PLOTS (Origin and Destination)
  # Origin station crowding title
  output$origin_crowding_title <- renderUI({
    origin_naptan <- input$origin_station
    if (is.null(origin_naptan) || length(origin_naptan) == 0 || origin_naptan == "") {
      return(h5("Crowding at Origin Station"))
    }
    
    origin_naptan <- as.character(origin_naptan)[1]
    # Look up station name from naptan code
    station_name <- names(station_choices[station_choices == origin_naptan])[1]
    if (is.null(station_name) || is.na(station_name) || station_name == "") {
      station_name <- "Origin Station"
    }
    return(h5(paste("Crowding at", station_name)))
  })
  
  # Destination station crowding title
  output$destination_crowding_title <- renderUI({
    dest_naptan <- input$destination_station
    if (is.null(dest_naptan) || length(dest_naptan) == 0 || dest_naptan == "") {
      return(h5("Crowding at Destination Station"))
    }
    
    dest_naptan <- as.character(dest_naptan)[1]
    # Look up station name from naptan code
    station_name <- names(station_choices[station_choices == dest_naptan])[1]
    if (is.null(station_name) || is.na(station_name) || station_name == "") {
      station_name <- "Destination Station"
    }
    return(h5(paste("Crowding at", station_name)))
  })
  
  # ============================================================================
  # Origin station crowding plot
  output$origin_crowding_plot <- renderPlot({
    # Force reactive dependency
    input$origin_station
    
    origin_naptan <- input$origin_station
    
    if (is.null(origin_naptan) || length(origin_naptan) == 0 || origin_naptan == "") {
      return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, 
                     label = "Select origin station", 
                     size = 4, hjust = 0.5, vjust = 0.5) +
             theme_void())
    }
    
    origin_naptan <- as.character(origin_naptan)[1]
    if (is.na(origin_naptan) || origin_naptan == "") {
      return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, 
                     label = "Select origin station", 
                     size = 4, hjust = 0.5, vjust = 0.5) +
             theme_void())
    }
    
    tryCatch({
      p <- plot_crowd(origin_naptan)
      return(p)
    }, error = function(e) {
      return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, 
                     label = paste("Error:\n", e$message), 
                     size = 3, hjust = 0.5, vjust = 0.5) +
             theme_void())
    })
  })
  
  # ============================================================================
  # Destination station crowding plot
  output$destination_crowding_plot <- renderPlot({
    # Force reactive dependency
    input$destination_station
    
    dest_naptan <- input$destination_station
    
    if (is.null(dest_naptan) || length(dest_naptan) == 0 || dest_naptan == "") {
      return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, 
                     label = "Select destination station", 
                     size = 4, hjust = 0.5, vjust = 0.5) +
             theme_void())
    }
    
    dest_naptan <- as.character(dest_naptan)[1]
    if (is.na(dest_naptan) || dest_naptan == "") {
      return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, 
                     label = "Select destination station", 
                     size = 4, hjust = 0.5, vjust = 0.5) +
             theme_void())
    }
    
    tryCatch({
      p <- plot_crowd(dest_naptan)
      return(p)
    }, error = function(e) {
      return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, 
                     label = paste("Error:\n", e$message), 
                     size = 3, hjust = 0.5, vjust = 0.5) +
             theme_void())
    })
  })
  
  # ==============================================================================
  # BOTTOM CROWDING PLOT (for Stations tab)
  # ==============================================================================
  
  # Station crowding title (for Stations tab)
  output$station_crowding_title <- renderUI({
    station_name <- station_name()
    if (is.null(station_name) || length(station_name) == 0 || station_name == "") {
      return(h5("Crowding Levels"))
    }
    
    station_name <- as.character(station_name)[1]
    if (is.na(station_name) || station_name == "") {
      return(h5("Crowding Levels"))
    }
    return(h5(paste("Crowding at", station_name)))
  })
  
  output$crowding_plot <- renderPlot({
    # 1. Force reactive dependencies
    input$station_selector
    input$destination_station
    input$left_tabs
    
    # 2. Determine Logic based on Tab
    active_tab <- input$left_tabs %||% "Stations"
    naptan <- NULL
    
    if (active_tab == "Stations") {
      # Input is already the Code
      naptan <- input$station_selector
      
    } else if (active_tab == "Journey") {
      # Input is destination code
      naptan <- input$destination_station
      
    } else {
      # Fallback to manual naptan if used
      n <- crowd_naptan_reactive()
      if (!is.null(n) && n != "") naptan <- as.character(n)
    }
    
    # 3. Clean the Input
    if (!is.null(naptan)) {
      naptan <- as.character(naptan)[1]
      if (is.na(naptan) || naptan == "") naptan <- NULL
    }
    
    plot_crowd(naptan)
  })
}
