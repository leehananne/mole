# --- 0. Load Required Libraries ---
library(shiny)
library(plotly)
library(httr)
library(jsonlite)
library(ggplot2)
library(purrr)
library(dplyr)
library(lubridate) # Keep for time calculations
library(DT)

# --- 1. API Keys ---
# Enter your Google Maps API key
google_maps_api <- "" 

# --- 2. Function to Fetch and Process TfL Station Data ---
fetch_and_process_tfl_stoppoints <- function() {
  urls <- c(
    tube = "https://api.tfl.gov.uk/StopPoint/Mode/tube",
    elizabeth_overground = "https://api.tfl.gov.uk/StopPoint/Mode/elizabeth-line,overground"
  )
  all_stations_list <- list()
  station_choices <<- list("Loading Error" = "")
  master_tube_locations <<- data.frame(NaptanCode=character(), Latitude=numeric(), Longitude=numeric(), StationName=character())
  
  tryCatch({
    for (mode_group in names(urls)) {
      url <- urls[[mode_group]]
      message("Fetching data for: ", mode_group)
      response <- httr::GET(url, timeout(60))
      stop_for_status(response, task = paste("fetch data for", mode_group))
      json_content <- httr::content(response, "text", encoding = "UTF-8")
      parsed_data <- jsonlite::fromJSON(json_content, flatten = TRUE, simplifyDataFrame = TRUE)
      stations_df <- parsed_data$stopPoints
      if (is.data.frame(stations_df) && nrow(stations_df) > 0) {
        stations_df$modeGroupFetched <- mode_group
        all_stations_list[[mode_group]] <- stations_df
      } else {
        warning("No 'stopPoints' data frame found or empty for ", mode_group)
      }
    } # End loop
    
    if (length(all_stations_list) == 0) stop("No station data successfully fetched from any endpoint.")
    
    combined_df <- bind_rows(all_stations_list)
    message("Combined data frame created with ", nrow(combined_df), " total rows before filtering.")
    
    message("Filtering and cleaning data...")
    available_names <- names(combined_df)
    naptan_col <- if ("naptanId" %in% available_names) { "naptanId" }
    else if ("stationNaptan" %in% available_names) { "stationNaptan"}
    else { stop("Required Naptan ID column ('naptanId' or 'stationNaptan') not found.") }
    message("Using column '", naptan_col, "' for NaptanCode.")
    
    master_tube_df <- combined_df %>%
      select(StationName = commonName, NaptanCode = !!sym(naptan_col), Latitude = lat, Longitude = lon, StopType = stopType) %>%
      filter(grepl("NaptanMetroStation|NaptanRailStation", StopType, ignore.case = TRUE),
             !is.na(StationName) & StationName != "",
             !is.na(NaptanCode) & NaptanCode != "",
             !is.na(Latitude), !is.na(Longitude)) %>%
      distinct(NaptanCode, .keep_all = TRUE) %>%
      arrange(StationName)
    
    if (nrow(master_tube_df) == 0) stop("No valid Metro/Rail stations found after filtering.")
    
    station_choices <<- setNames(master_tube_df$NaptanCode, master_tube_df$StationName)
    master_tube_locations <<- master_tube_df %>% select(NaptanCode, Latitude, Longitude, StationName)
    
    message("Station data processing complete. Found ", nrow(master_tube_locations), " unique stations.")
    
  }, error = function(e) {
    message("!!! Error fetching/processing TfL Stoppoint data: ", e$message)
  })
}

# --- 3. Load Station Data on App Startup ---
fetch_and_process_tfl_stoppoints()

# --- 4. Check if Station Data Loaded & Find Default Naptan ---
default_station_name <- "South Kensington Underground Station"
default_naptan_code <- NULL

if (length(station_choices) > 1 && !(names(station_choices)[1] %in% c("Loading Error"))) {
  message("Successfully loaded ", length(station_choices), " stations for dropdowns.")
  default_naptan_code <- station_choices[names(station_choices) == default_station_name]
  if (length(default_naptan_code) == 0 || is.na(default_naptan_code)) {
    warning("Default station '", default_station_name, "' not found. Using the first station instead.")
    default_naptan_code <- station_choices[[1]]
  } else {
    default_naptan_code <- unname(default_naptan_code)
    message("Default station set to: ", default_station_name, " (", default_naptan_code, ")")
  }
} else {
  warning("Failed to load station data from TfL API. Dropdowns will be empty or show error.")
  default_naptan_code <- ""
}

# --- 5. UI Definition (Single Page using fluidPage) ---
ui <- fluidPage(
  title = "London Pulse",
  tags$head(tags$style(HTML(" #weatherStatement { white-space: pre-wrap; word-break: break-word; } "))),
  
  fluidRow(
    column(width = 12, h2("London Station Pulse 🚇☀️"), hr())
  ),
  fluidRow(
    column(width = 4,
           wellPanel(
             h4("Controls"),
             selectInput("selected_station_naptan", "Select Station:",
                         choices = station_choices,
                         selected = default_naptan_code),
             checkboxGroupInput("tfl_days", "Select Day(s) for Crowding:",
                                choices = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                                selected = "Mon", inline = TRUE)
           )
    ),
    column(width = 8,
           # Using Shiny's box function requires shinydashboard, replace with basic divs or panels
           # Box replacement 1: Crowding Plot
           tags$div(class = "panel panel-info", # Using Bootstrap panel classes
                    tags$div(class = "panel-heading",
                             tags$h3(class = "panel-title", "Predicted Crowding Levels (% Baseline)")
                    ),
                    tags$div(class = "panel-body",
                             plotlyOutput("tflCrowdingPlot", height = "450px")
                    )
           ),
           # Box replacement 2: Weather Info
           tags$div(class = "panel panel-primary", style = "margin-top: 20px;", # Add space above
                    tags$div(class = "panel-heading",
                             tags$h3(class = "panel-title", textOutput("weatherTitle")) # Dynamic title
                    ),
                    tags$div(class = "panel-body",
                             verbatimTextOutput("weatherStatement")
                    )
           )
    ) # End column for outputs
  ) # End fluidRow
) # End fluidPage

# --- 6. Server Definition ---
server <- function(input, output, session) {
  
  # --- Robust Breaks Function (Defined once) ---
  robust_breaks <- function(x) {
    # x here is the vector of levels of the factor
    num_levels <- length(x)
    if (num_levels == 0) return(character(0)) # Handle empty case
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
  
  # --- Combined Reactive for Selected Station Info ---
  selected_station_info <- reactive({
    req(input$selected_station_naptan)
    if (!exists("master_tube_locations") || !is.data.frame(master_tube_locations) || nrow(master_tube_locations) == 0) {
      return(data.frame())
    }
    master_tube_locations %>% filter(NaptanCode == input$selected_station_naptan)
  })
  
  # --- TfL Crowding Logic ---
  tfl_crowding_data <- reactive({
    req(input$selected_station_naptan, input$tfl_days)
    if (length(station_choices) == 0 || names(station_choices)[1] %in% c("Loading Error")) {
      return(data.frame())
    }
    naptan_code <- input$selected_station_naptan
    
    all_days_data <- map_dfr(input$tfl_days, function(day) {
      api_url <- paste0("https://api.tfl.gov.uk/crowding/", naptan_code, "/", day)
      tryCatch({
        response <- httr::GET(api_url, timeout(15))
        stop_for_status(response)
        json_content <- httr::content(response, "text", encoding = "UTF-8")
        parsed_data <- jsonlite::fromJSON(json_content, flatten = TRUE)
        if (is.data.frame(parsed_data$timeBands) && nrow(parsed_data$timeBands) > 0) {
          day_dataframe <- parsed_data$timeBands
          day_dataframe$dayOfWeek <- day
          day_dataframe$naptan <- naptan_code
          if (!all(c("timeBand", "percentageOfBaseLine") %in% names(day_dataframe))) return(NULL)
          if(is.character(day_dataframe$timeBand) && all(grepl("^[0-9]+$", day_dataframe$timeBand))) {
            day_dataframe$timeBand <- factor(day_dataframe$timeBand, levels = as.character(0:95))
          } else if (is.numeric(day_dataframe$timeBand)) {
            day_dataframe$timeBand <- factor(day_dataframe$timeBand, levels = sort(unique(as.numeric(day_dataframe$timeBand))))
          }
          return(day_dataframe)
        } else {
          message("No valid timeBands data frame returned for Naptan ", naptan_code, " on ", day)
          showNotification(paste("No crowding data available for", names(station_choices[station_choices == naptan_code]), "on", day), type = "warning", duration=3)
          return(NULL)
        }
      }, error = function(e) {
        status_code <- NA # Initialize status code
        # Attempt to get status code safely
        if(exists("response") && !is.null(response)) {
          try(status_code <- httr::status_code(response), silent = TRUE)
        }
        err_message <- paste("Error fetching TfL crowding data for", day)
        if (!is.na(status_code)) { err_message <- paste(err_message, "- HTTP Status:", status_code) }
        err_message <- paste(err_message, "-", e$message)
        message(err_message)
        showNotification(err_message, type = "error", duration = 8)
        return(NULL)
      })
    }) # End map_dfr
    if (is.null(all_days_data) || nrow(all_days_data) == 0) return(data.frame())
    return(all_days_data)
  })
  
  # Render the crowding plot
  output$tflCrowdingPlot <- renderPlotly({
    plot_data <- tfl_crowding_data()
    if (!is.data.frame(plot_data) || nrow(plot_data) == 0) {
      # Create an empty plot with error message
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
      # Create an empty plot with error message
      empty_plot <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                label = paste("Plotting Error: Column '", time_col, "' not found."), 
                size = 4, hjust = 0.5, vjust = 0.5) +
        theme_void()
      return(ggplotly(empty_plot))
    }
    if (!value_col %in% names(plot_data)) {
      # Create an empty plot with error message
      empty_plot <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                label = paste("Plotting Error: Column '", value_col, "' not found."), 
                size = 4, hjust = 0.5, vjust = 0.5) +
        theme_void()
      return(ggplotly(empty_plot))
    }
    
    # --- Create ggplot ---
    p <- ggplot(data = plot_data,
                aes(x = !!sym(time_col), y = !!sym(value_col), group = dayOfWeek, color = dayOfWeek,
                    text = paste("Day:", dayOfWeek, "<br>Time Band:", !!sym(time_col), "<br>Crowding:", round(!!sym(value_col),1), "%"))) +
      geom_line(linewidth = 0.8) +
      labs(title = NULL, # Remove title from ggplot object itself
           x = "Time Band (15 min intervals)", y = "Crowding (% of Baseline)", color = "Day") +
      theme_minimal(base_size = 11) +
      scale_x_discrete(breaks = robust_breaks) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 9),
            legend.position = "bottom")
    
    # --- Get Current Time Info ---
    current_time <- Sys.time()
    current_day_abbr <- format(current_time, "%a") # e.g., "Fri"
    current_hour <- as.numeric(format(current_time, "%H"))
    current_minute <- as.numeric(format(current_time, "%M"))
    current_time_band_index <- floor((current_hour * 60 + current_minute) / 15)
    # Convert index to the character/factor format used on the x-axis
    current_time_band_factor <- factor(as.character(current_time_band_index), levels = levels(plot_data$timeBand))
    
    # --- Add Vertical Line for Current Time ---
    if (current_day_abbr %in% input$tfl_days && !is.na(current_time_band_factor) && current_time_band_factor %in% levels(plot_data$timeBand)) {
      p <- p + geom_vline(
        xintercept = current_time_band_factor, # Use factor level directly
        linetype = "dashed", color = "black", linewidth = 1) +
        annotate( geom = "text", x = current_time_band_factor,
                  y = max(plot_data[[value_col]], na.rm = TRUE) * 0.95, # Position near top
                  label = "Now", color = "black", vjust = -0.5, size = 3)
      message("Adding vline for current time.")
    } else {
      message("Current day not selected or time band invalid/not in plot data, skipping vline.")
    }
    
    # --- Convert to plotly ---
    # Need to handle potential issues if ggplotly has problems with the vline/annotation factor levels
    # Try converting first, then add layout
    plotly_obj <- ggplotly(p, tooltip = "text")
    
    # Apply layout AFTER ggplotly conversion
    plotly_obj <- plotly_obj %>%
      layout(
        # title = list(text = paste("Predicted Crowding Trend for", station_name), y = 0.98), # Add title via layout
        legend = list(orientation = "h", x = 0.1, y = -0.25)
      )
    
    return(plotly_obj) # Return the final plotly object
    
  }) # End renderPlotly
  
  
  # --- Weather Data Logic ---
  # Dynamic title for the weather box
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
  
  # Timer to refresh weather data
  autoInvalidate <- reactiveTimer(intervalMs = 1000 * 60 * 15) # 15 minutes
  
  # Fetch weather data
  weather_api_data <- reactive({
    autoInvalidate()
    info <- selected_station_info()
    if (is.null(info) || nrow(info) == 0) {
      return(NULL)
    }
    if (is.na(info$Latitude) || is.na(info$Longitude)) {
      return(NULL)
    }
    
    base_url <- "https://weather.googleapis.com/v1/currentConditions:lookup"
    api_key_google <- google_maps_api
    
    full_url <- modify_url(base_url, query = list(
      key = api_key_google,
      "location.latitude" = info$Latitude,
      "location.longitude" = info$Longitude
    ))
    
    tryCatch({
      message("Fetching Google Weather for: ", info$StationName[1])
      response <- httr::GET(full_url, timeout(10))
      stop_for_status(response)
      json_content <- httr::content(response, "text", encoding = "UTF-8")
      parsed_data <- jsonlite::fromJSON(json_content, flatten = TRUE)
      parsed_data$SelectedStationName <- info$StationName[1]
      return(parsed_data) # Return list
    }, error = function(e) {
      message("!!! Error fetching Google Weather data: ", e$message)
      showNotification(paste("Error fetching weather:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Render the simple weather statement
  output$weatherStatement <- renderText({
    data_list <- weather_api_data()
    if (is.null(data_list) || !is.list(data_list)) {
      return("Waiting for weather data or API call failed...")
    }
    
    # Convert list to local data frame for easier access & checking names
    df <- tryCatch(as.data.frame(data_list), error = function(e) NULL)
    if (is.null(df)) {
      return("Error processing weather data structure.")
    }
    
    # Extract using direct data frame access, providing default NA
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
  
} # End server function

# --- 7. Run the App ---
shinyApp(ui, server)