server <- function(input, output, session) {
  
  # ==============================================================================
  # MAP SERVER LOGIC
  # ==============================================================================
  # Initialize Map Logic (from 4-map-ui.R)
  map_server_logic(input, output, session)
  
  # Station selector logic
  observeEvent(input$station_selector, {
    # Future: Zoom map to selected station
    if (!is.null(input$station_selector) && input$station_selector != "") {
      message("Station selected: ", input$station_selector)
    }
  }, ignoreNULL = FALSE)
  
  # ==============================================================================
  # JOURNEY PLANNER LOGIC
  # ==============================================================================
  
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
        style = "margin-bottom: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
        h5(style = "margin-top: 0;", paste("Journey: ", origin_name, " → ", dest_name)),
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
  
  # ==============================================================================
  # LEGACY CODE (from refactor version - kept for reference, may not be used)
  # ==============================================================================
  
  selected_station_info <- reactive({
    req(input$selected_station_naptan)
    if (!exists("master_tube_locations") || !is.data.frame(master_tube_locations) || nrow(master_tube_locations) == 0) {
      return(data.frame())
    }
    master_tube_locations %>% dplyr::filter(NaptanCode == input$selected_station_naptan)
  })

  origin_station_info <- reactive({
    req(input$origin_station)
    if (!exists("master_tube_locations") || !is.data.frame(master_tube_locations) || nrow(master_tube_locations) == 0) {
      return(data.frame())
    }
    master_tube_locations %>% dplyr::filter(NaptanCode == input$origin_station)
  })

  output$weatherTitle <- renderText({
    info <- origin_station_info()
    if (is.null(info) || nrow(info) == 0) {
      return("Waiting for origin station selection...")
    }
    if (is.na(info$StationName[1]) || info$StationName[1] == "") {
      return("Station name missing.")
    }
    paste("Current Weather near", info$StationName[1])
  })

  autoInvalidate <- reactiveTimer(intervalMs = 1000 * 60 * 15)

  weather_api_data <- reactive({
    autoInvalidate()
    info <- origin_station_info()
    if (is.null(info) || nrow(info) == 0) return(NULL)
    if (is.na(info$Latitude) || is.na(info$Longitude)) return(NULL)
    fetch_weather_data(google_maps_api, info$Latitude[1], info$Longitude[1], info$StationName[1])
  })

  output$weatherStatement <- renderText({
    data_list <- weather_api_data()
    if (is.null(data_list) || !is.list(data_list)) {
      return("Waiting for weather data or API call failed...")
    }
    df <- tryCatch(as.data.frame(data_list), error = function(e) NULL)
    if (is.null(df)) {
      return("Error processing weather data structure.")
    }
    station_name <- df[["SelectedStationName"]][1] %||% "Selected Location"
    temp <- df[["temperature.degrees"]][1] %||% NA_real_
    condition <- df[["weatherCondition.description.text"]][1] %||% "N/A"
    feels_like <- df[["feelsLikeTemperature.degrees"]][1] %||% NA_real_
    humidity <- df[["relativeHumidity"]][1] %||% NA_integer_
    heat_index <- df[["heatIndex.degrees"]][1] %||% NA_real_
    lines <- list()
    lines$line1 <- paste("Weather at ", station_name, ":")
    lines$line2 <- "--------------------------"
    lines$line3 <- paste(" Condition: ", condition)
    lines$line4 <- paste(" Temp:      ", ifelse(is.na(temp), "N/A", paste0(round(temp, 1), "°C")))
    lines$line5 <- paste(" Feels Like:", ifelse(is.na(feels_like), "N/A", paste0(round(feels_like, 1), "°C")))
    lines$line6 <- paste(" Humidity:  ", ifelse(is.na(humidity), "N/A", paste0(humidity, "%")))
    lines$line7 <- paste(" Heat Index:", ifelse(is.na(heat_index), "N/A", paste0(round(heat_index, 1), "°C")))
    statement <- paste(lines, collapse = "\n")
    if (!is.character(statement)) { statement <- "Error formatting weather statement." }
    return(statement)
  })

  # ==============================================================================
  # STATION CROWDING TAB
  # ==============================================================================
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
  # ==============================================================================
  
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
  # BOTTOM OVERLAY CROWDING PLOT (for Stations tab)
  # ==============================================================================
  output$crowding_plot <- renderPlot({
    # Force reactive dependencies
    input$station_selector
    input$destination_station
    input$left_tabs
    
    # Choose which station to use based on the active left-hand tab
    active_tab <- input$left_tabs %||% "Stations"
    
    message("\n>>> Crowding Plot Render <<<")
    message("Active tab: ", active_tab)
    
    naptan <- NULL
    
    if (active_tab == "Stations") {
      # From the station search tab (autocomplete) - returns station NAME, need to convert to naptan
      station_name <- input$station_selector
      message("Station name from selector: '", station_name, "'")
      
      if (!is.null(station_name) && station_name != "") {
        # Try station_master_data first (from CSV)
        if (exists("station_master_data") && is.data.frame(station_master_data) && nrow(station_master_data) > 0) {
          message("Looking up naptan in station_master_data (CSV) for station: '", station_name, "'")
          message("station_master_data has ", nrow(station_master_data), " rows")
          
          # Trim whitespace and try exact match first
          station_name_trimmed <- trimws(station_name)
          station_match <- station_master_data %>% 
            filter(trimws(StationName) == station_name_trimmed) %>% 
            pull(NaptanCode)
          
          # If no exact match, try case-insensitive
          if (length(station_match) == 0) {
            message("No exact match, trying case-insensitive...")
            station_match <- station_master_data %>% 
              filter(tolower(trimws(StationName)) == tolower(station_name_trimmed)) %>% 
              pull(NaptanCode)
          }
          
          message("Found ", length(station_match), " matches")
          if (length(station_match) > 0) {
            message("First match naptan: ", station_match[1])
          }
          
          if (length(station_match) > 0 && !is.na(station_match[1]) && station_match[1] != "") {
            naptan <- as.character(station_match[1])
            message("Using naptan: ", naptan)
          } else {
            message("No valid naptan found in CSV for station: '", station_name, "'")
            # Show available stations for debugging
            message("Available stations (first 5): ", paste(head(station_master_data$StationName, 5), collapse = ", "))
          }
        } else {
          message("station_master_data not available or empty")
        }
      } else {
        message("Station name is NULL or empty")
      }
    } else if (active_tab == "Journey") {
      # From the journey tab (destination station) - already a naptan code
      dest_naptan <- input$destination_station
      message("Destination naptan from journey tab: ", dest_naptan)
      if (!is.null(dest_naptan) && dest_naptan != "") {
        naptan <- as.character(dest_naptan)
        message("Using naptan: ", naptan)
      }
    } else {
      # Fallback: use the manual Naptan from the Crowding tab if available
      naptan <- tryCatch({
        n <- crowd_naptan_reactive()
        if (!is.null(n) && n != "") as.character(n) else NULL
      }, error = function(e) NULL)
    }
    
    message("Final naptan value: ", if(is.null(naptan)) "NULL" else naptan)
    
    # Convert to character and validate before using validate()
    if (is.null(naptan) || length(naptan) == 0) {
      naptan <- NULL
    } else {
      naptan <- as.character(naptan)[1]  # Take first element and convert to character
      if (is.na(naptan) || naptan == "") {
        naptan <- NULL
      }
    }
    
    # Now validate with a simple check
    if (is.null(naptan) || naptan == "") {
      return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, 
                     label = "Please select a station to see crowding levels.", 
                     size = 4, hjust = 0.5, vjust = 0.5) +
             theme_void())
    }
    
    message("Calling plot_crowd with naptan: ", naptan)
    message("  naptan class: ", class(naptan))
    message("  naptan is.character: ", is.character(naptan))
    
    # Wrap plot_crowd in tryCatch to handle errors gracefully
    tryCatch({
      p <- plot_crowd(naptan)
      message("Plot generated successfully, class: ", class(p))
      if (is.null(p)) {
        message("WARNING: plot_crowd returned NULL")
        return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Plot returned NULL", size = 4) +
               theme_void())
      }
      return(p)
    }, error = function(e) {
      message("ERROR in plot_crowd: ", e$message)
      message("Error traceback: ", paste(capture.output(traceback()), collapse = "\n"))
      # Return an empty plot with error message
      return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, 
                     label = paste("Error loading plot:\n", e$message), 
                     size = 3, hjust = 0.5, vjust = 0.5) +
             theme_void() +
             labs(title = "Crowding Plot Error"))
    })
  })

}


