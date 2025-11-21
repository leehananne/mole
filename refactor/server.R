server <- function(input, output, session) {
  robust_breaks <- function(x) {
    num_levels <- length(x)
    if (num_levels == 0) return(character(0))
    if (num_levels < 8) {
      return(x)
    } else {
      indices <- seq(1, num_levels, by = 8)
      if (indices[length(indices)] != num_levels) {
        indices <- c(indices, num_levels)
      }
      return(x[indices])
    }
  }

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

  tfl_crowding_data <- reactive({
    req(input$selected_station_naptan, input$tfl_days)
    if (length(station_choices) == 0 || names(station_choices)[1] %in% c("Loading Error")) {
      return(data.frame())
    }
    # Convert single day value to vector for fetch_crowd_api (radioButtons returns single value)
    fetch_crowd_api(input$selected_station_naptan, c(input$tfl_days))
  })

  output$tflCrowdingPlot <- renderPlotly({
    plot_data <- tfl_crowding_data()
    if (!is.data.frame(plot_data) || nrow(plot_data) == 0) {
      empty_plot <- ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                label = "No crowding data available to plot for the selected station(s) and day(s).\nCheck API status or selection.",
                size = 4, hjust = 0.5, vjust = 0.5) +
        theme_void()
      return(ggplotly(empty_plot))
    }
    station_name <- names(station_choices[station_choices == input$selected_station_naptan])
    station_name <- if (length(station_name) == 0 || is.na(station_name)) input$selected_station_naptan else station_name[1]
    time_col <- "timeBand"; value_col <- "percentageOfBaseLine"
    if (!time_col %in% names(plot_data)) {
      empty_plot <- ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                label = paste("Plotting Error: Column '", time_col, "' not found."),
                size = 4, hjust = 0.5, vjust = 0.5) +
        theme_void()
      return(ggplotly(empty_plot))
    }
    if (!value_col %in% names(plot_data)) {
      empty_plot <- ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                label = paste("Plotting Error: Column '", value_col, "' not found."),
                size = 4, hjust = 0.5, vjust = 0.5) +
        theme_void()
      return(ggplotly(empty_plot))
    }
    p <- ggplot(data = plot_data,
                aes(x = !!sym(time_col), y = !!sym(value_col), group = dayOfWeek, color = dayOfWeek,
                    text = paste("Day:", dayOfWeek, "<br>Time Band:", !!sym(time_col), "<br>Crowding:", round(!!sym(value_col),1), "%"))) +
      geom_line(linewidth = 0.8) +
      labs(title = NULL,
           x = "Time Band (15 min intervals)", y = "Crowding (% of Baseline)", color = "Day") +
      theme_minimal(base_size = 11) +
      scale_x_discrete(breaks = robust_breaks) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 9),
            legend.position = "bottom")
    current_time <- Sys.time()
    message("Current time: ", current_time)
    # Ensure we're working with POSIXct and get time components
    current_time <- as.POSIXct(current_time)
    current_day_abbr <- format(current_time, "%a")
    current_hour <- as.numeric(format(current_time, "%H"))
    current_minute <- as.numeric(format(current_time, "%M"))
    current_time_band_index <- floor((current_hour * 60 + current_minute) / 15)
    message("Current day: ", current_day_abbr, ", Hour: ", current_hour, ", Minute: ", current_minute, ", Time band index: ", current_time_band_index)
    
    # Get actual time band levels from the data (these are the display strings)
    actual_time_bands <- levels(plot_data$timeBand)
    message("Available time bands in data: ", paste(head(actual_time_bands, 10), collapse=", "), if(length(actual_time_bands) > 10) "..." else "")
    
    # Check if current day is selected (input$tfl_days is now a single value from radioButtons)
    if (current_day_abbr == input$tfl_days && length(actual_time_bands) > 0) {
      # Find the time band display string that matches the current time band index
      # Use timeBand_index column if available, otherwise try to match by parsing
      if ("timeBand_index" %in% names(plot_data)) {
        # Find the row(s) with matching index
        matching_rows <- plot_data[plot_data$timeBand_index == current_time_band_index, ]
        if (nrow(matching_rows) > 0) {
          time_band_display <- as.character(matching_rows$timeBand[1])
        } else {
          # Find closest index
          closest_idx <- which.min(abs(plot_data$timeBand_index - current_time_band_index))
          time_band_display <- as.character(plot_data$timeBand[closest_idx])
          message("Exact time band index (", current_time_band_index, ") not found. Using closest available: ", time_band_display)
        }
      } else {
        # Fallback: try to parse time range strings to find match
        # This shouldn't happen if data processing is correct, but handle it gracefully
        time_band_display <- actual_time_bands[1]  # Default to first
        message("timeBand_index column not found, using first time band")
      }
      
      current_time_band_factor <- factor(time_band_display, levels = actual_time_bands)
      p <- p + geom_vline(
        xintercept = current_time_band_factor,
        linetype = "dashed", color = "black", linewidth = 1) +
        annotate( geom = "text", x = current_time_band_factor,
                  y = max(plot_data[[value_col]], na.rm = TRUE) * 0.95,
                  label = "Now", color = "black", vjust = -0.5, size = 3)
      message("Adding vline for current time at band: ", time_band_display)
    } else {
      if (current_day_abbr != input$tfl_days) {
        message("Current day (", current_day_abbr, ") not selected (selected: ", input$tfl_days, "), skipping vline.")
      } else {
        message("No time bands available in plot data, skipping vline.")
      }
    }
    plotly_obj <- ggplotly(p, tooltip = "text")
    plotly_obj <- plotly_obj %>%
      layout(
        legend = list(orientation = "h", x = 0.1, y = -0.25)
      )
    return(plotly_obj)
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


