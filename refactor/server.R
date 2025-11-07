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

  tfl_crowding_data <- reactive({
    req(input$selected_station_naptan, input$tfl_days)
    if (length(station_choices) == 0 || names(station_choices)[1] %in% c("Loading Error")) {
      return(data.frame())
    }
    fetch_tfl_data(input$selected_station_naptan, input$tfl_days)
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
    current_day_abbr <- format(current_time, "%a")
    current_hour <- as.numeric(format(current_time, "%H"))
    current_minute <- as.numeric(format(current_time, "%M"))
    current_time_band_index <- floor((current_hour * 60 + current_minute) / 15)
    current_time_band_factor <- factor(as.character(current_time_band_index), levels = levels(plot_data$timeBand))
    if (current_day_abbr %in% input$tfl_days && !is.na(current_time_band_factor) && current_time_band_factor %in% levels(plot_data$timeBand)) {
      p <- p + geom_vline(
        xintercept = current_time_band_factor,
        linetype = "dashed", color = "black", linewidth = 1) +
        annotate( geom = "text", x = current_time_band_factor,
                  y = max(plot_data[[value_col]], na.rm = TRUE) * 0.95,
                  label = "Now", color = "black", vjust = -0.5, size = 3)
      message("Adding vline for current time.")
    } else {
      message("Current day not selected or time band invalid/not in plot data, skipping vline.")
    }
    plotly_obj <- ggplotly(p, tooltip = "text")
    plotly_obj <- plotly_obj %>%
      layout(
        legend = list(orientation = "h", x = 0.1, y = -0.25)
      )
    return(plotly_obj)
  })

  output$weatherTitle <- renderText({
    info <- selected_station_info()
    if (is.null(info) || nrow(info) == 0) {
      return("Waiting for station selection...")
    }
    if (is.na(info$StationName[1]) || info$StationName[1] == "") {
      return("Station name missing.")
    }
    paste("Current Weather near", info$StationName[1])
  })

  autoInvalidate <- reactiveTimer(intervalMs = 1000 * 60 * 15)

  weather_api_data <- reactive({
    autoInvalidate()
    info <- selected_station_info()
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

  journey_route_data <- eventReactive(input$plan_journey, {
    req(input$origin_station, input$destination_station)
    if (input$origin_station == input$destination_station) {
      showNotification("Origin and destination stations must be different", type = "warning", duration = 3)
      return(NULL)
    }
    result <- fetch_journey_route(input$origin_station, input$destination_station)
    if (is.null(result)) {
      origin_name <- names(station_choices[station_choices == input$origin_station])[1] %||% input$origin_station
      dest_name <- names(station_choices[station_choices == input$destination_station])[1] %||% input$destination_station
      showNotification(
        paste("Failed to fetch journey route from", origin_name, "to", dest_name, ". Please check the station codes or try again."),
        type = "error",
        duration = 8
      )
    }
    return(result)
  })

  output$journeyRouteOutput <- renderText({
    route_data <- journey_route_data()
    if (is.null(route_data)) {
      return("Select origin and destination stations, then click 'Plan Journey' to find a route.")
    }
    
    # Extract journey information from the API response
    origin_name <- names(station_choices[station_choices == input$origin_station])[1] %||% input$origin_station
    dest_name <- names(station_choices[station_choices == input$destination_station])[1] %||% input$destination_station
    
    lines <- list()
    lines$header <- paste("Journey from ", origin_name, " to ", dest_name, ":")
    lines$separator <- "=========================================="
    
    # Check if journeys exist in the response
    if (!is.null(route_data$journeys) && length(route_data$journeys) > 0) {
      journey <- route_data$journeys[[1]]  # Get first journey
      
      # Duration
      if (!is.null(journey$duration)) {
        duration_mins <- round(journey$duration / 60, 1)
        lines$duration <- paste("Duration: ", duration_mins, " minutes")
      }
      
      # Legs information
      if (!is.null(journey$legs) && length(journey$legs) > 0) {
        lines$legs_header <- "\nRoute:"
        leg_details <- c()
        for (i in seq_along(journey$legs)) {
          leg <- journey$legs[[i]]
          if (!is.null(leg$instruction$summary)) {
            leg_details <- c(leg_details, paste("  ", i, ". ", leg$instruction$summary))
          } else if (!is.null(leg$mode$name)) {
            leg_details <- c(leg_details, paste("  ", i, ". ", leg$mode$name))
          }
        }
        if (length(leg_details) > 0) {
          lines$legs <- paste(leg_details, collapse = "\n")
        }
      }
    } else {
      lines$no_route <- "No route found between these stations."
    }
    
    result <- paste(lines, collapse = "\n")
    if (!is.character(result) || result == "") {
      return("Route data received but could not be formatted. Check API response structure.")
    }
    return(result)
  })
}


