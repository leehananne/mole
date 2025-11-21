server <- function(input, output, session) {
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

  # Crowding forecast reactive
  crowding_forecast <- eventReactive(input$plan_journey, {
    req(input$selected_station_naptan)
    if (length(station_choices) == 0 || names(station_choices)[1] %in% c("Loading Error")) {
      return(NULL)
    }
    tryCatch({
      forecast_data <- fetch_crowd_api(input$selected_station_naptan)
      return(forecast_data)
    }, error = function(e) {
      message("Error fetching crowding forecast: ", e$message)
      return(NULL)
    })
  })
  
  output$crowdingForecast <- renderText({
    forecast_data <- crowding_forecast()
    if (is.null(forecast_data) || is.null(forecast_data$forecast_message)) {
      return("Waiting for crowding forecast data...")
    }
    return(forecast_data$forecast_message)
  })

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
      # Get journey details for the first journey
      journey_details <- extract_journey_details(parsed_data, journey_index = 1)
      
      # Transform journey_details to format expected by generate_journey_html()
      journey_data_formatted <- transform_journey_details(journey_details)
      
      # Generate HTML using the component function
      journey_html <- generate_journey_html(journey_data_formatted)
      
      # Add journey summary header
      num_legs <- attr(journey_details, "num_legs")
      total_duration <- sum(journey_details$duration, na.rm = TRUE)
      
      summary_header <- div(
        style = "margin-bottom: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
        h5(style = "margin-top: 0;", paste("Journey: ", origin_name, " → ", dest_name)),
        p(style = "margin-bottom: 0; font-size: 12px; color: #666;",
          paste("Total Duration: ", total_duration, " minutes | ", "Number of Legs: ", num_legs))
      )
      
      message("  Output formatted successfully")
      message(">>> Rendering complete <<<\n")
      return(tagList(summary_header, journey_html))
      
    }, error = function(e) {
      message("  Error extracting journey details: ", e$message)
      return(div(class = "journey-container", 
                 p(style = "color: red;", paste("Error processing journey route:", e$message))))
    })
  })
}


