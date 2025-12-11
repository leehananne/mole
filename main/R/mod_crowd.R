# ==============================================================================
# CROWD PLOT MODULE
# Function: Plots the crowd level of the selected station
# ==============================================================================
# Argument: naptan_code

plot_crowd <- function(naptan_code) {
  
  # Ensure naptan_code is a character string
  if (is.null(naptan_code) || length(naptan_code) == 0) {
    stop("Naptan code is required.")
  }
  naptan_code <- as.character(naptan_code)[1]
  if (is.na(naptan_code) || naptan_code == "") {
    stop("Naptan code is required.")
  }
  
  # --- 1. Time Setup ---
  now <- as.POSIXct(Sys.time())
  current_day_abbr <- format(now, "%a")
  current_hour <- as.numeric(format(now, "%H"))
  current_minute <- as.numeric(format(now, "%M"))
  
  # Calculate index (1-96)
  current_idx <- (current_hour * 4) + floor(current_minute / 15) + 1
  
  # --- 2. API Call & Data Retrieval ---
  base_url <- paste0("https://api.tfl.gov.uk/crowding/", naptan_code, "/", current_day_abbr)
  
  # Prepare variables
  crowding_data <- NULL
  is_synthetic_data <- FALSE
  
  # Safe API Call
  response <- tryCatch({
    httr::GET(base_url, httr::timeout(15))
  }, error = function(e) NULL)
  
  # Logic to determine if we have valid data
  if (is.null(response) || httr::status_code(response) != 200) {
    is_synthetic_data <- TRUE
  } else {
    json_content <- httr::content(response, "text", encoding = "UTF-8")
    
    # Handle empty response body
    if (nchar(json_content) == 0) {
      is_synthetic_data <- TRUE
    } else {
      parsed_data <- tryCatch({
        jsonlite::fromJSON(json_content, flatten = TRUE)
      }, error = function(e) NULL)
      
      # CHECK: Explicitly look for "isFound": false OR missing timeBands
      if (is.null(parsed_data)) {
        is_synthetic_data <- TRUE
      } else if (!is.null(parsed_data$isFound) && parsed_data$isFound == FALSE) {
        is_synthetic_data <- TRUE
      } else if (is.null(parsed_data$timeBands) || nrow(parsed_data$timeBands) == 0) {
        is_synthetic_data <- TRUE
      } else {
        # Valid Data Found
        crowding_data <- parsed_data$timeBands
      }
    }
  }
  
  # --- 3. Data Processing (Real vs Synthetic) ---
  
  if (is_synthetic_data) {
    # GENERATE SYNTHETIC DATA (Flat 0.4 baseline for all 96 intervals)

    # Generate time strings "00:00", "00:15" ... "23:45"
    hours <- rep(0:23, each = 4)
    mins  <- rep(c("00", "15", "30", "45"), 24)
    time_strs <- sprintf("%02d:%s", hours, mins)
    
    crowding_data <- data.frame(
      timeBand = time_strs,
      percentageOfBaseLine = 0.4, # Fixed 40% baseline
      stringsAsFactors = FALSE
    )
  }
  
  # Standardize columns
  crowding_data$percentage <- crowding_data$percentageOfBaseLine * 100
  crowding_data$id <- 1:nrow(crowding_data)
  # Extract HH:MM from timeBand (Handles "HH:MM" or "HH:MM-HH:MM")
  crowding_data$label_time <- sub("-.*", "", crowding_data$timeBand)
  
  # Window Logic: +/- 3 hours (16 slots each way)
  window_start <- max(1, current_idx - 12)
  window_end <- min(nrow(crowding_data), current_idx + 12)
  plot_data <- crowding_data[window_start:window_end, ]
  
  # Recalculate id for the filtered data (1 to nrow(plot_data))
  plot_data$id <- 1:nrow(plot_data)
  
  # Calculate current_idx relative to the filtered window
  current_idx_relative <- current_idx - window_start + 1
  
  # Filter x-axis labels to only show full hours (e.g. 17:00)
  breaks_subset <- plot_data[grepl(":00$", plot_data$label_time), ]
  
  # --- 4. Plotting ---
  
  # Base Plot
  p <- ggplot(plot_data, aes(x = id, y = percentage)) +
    
    # Context Zones (Green/Yellow) - Kept for both scenarios
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 50, alpha = 0.05, fill = "green") +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 50, ymax = 100, alpha = 0.05, fill = "yellow") +
    
    # Context Labels
    annotate("text", x = 1, y = 15, label = " Quiet", hjust = 0, vjust = 0, color = "darkgreen", size = 4, fontface = "italic") +
    annotate("text", x = 1, y = 65, label = " Busy", hjust = 0, vjust = 0, color = "orange", size = 4, fontface = "italic") +
    
    # Vertical Line at NOW
    geom_vline(xintercept = current_idx_relative, linetype = "dotted", color = "black", linewidth = 0.8) +
    
    # Scales
    scale_color_gradientn(
      colors = c("#00b33c", "#ffcc00", "#e32017"), 
      values = scales::rescale(c(0, 50, 80)),
      name = "Crowd Intensity"
    ) +
    
    scale_x_continuous(breaks = breaks_subset$id, labels = breaks_subset$label_time) +
    
    # Ensure y-axis covers 0-100
    expand_limits(y = c(0, 100)) +
    
    labs(x = NULL, y = NULL) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 10, face = "bold"),
      legend.position = "none" 
    )
  
  # --- CONDITIONAL LAYERS based on Data Quality ---
  
  if (is_synthetic_data) {
    # SCENARIO: MISSING DATA
    p <- p + 
      # Dashed Line
      geom_line(color = "grey60", linewidth = 1, linetype = "dashed") +
      # Missing Data Label (Replaces "NOW")
      annotate("label", x = current_idx_relative, y = 55, 
               label = "Missing crowd data", 
               size = 5, fontface = "bold", color = "grey40", fill = "white")
    
  } else {
    # SCENARIO: REAL DATA
    p <- p + 
      # Solid Line
      geom_line(color = "grey60", linewidth = 1) +
      # Gradient Points
      geom_point(aes(color = percentage), size = 3.5) +
      # "NOW" Label
      annotate("label", x = current_idx_relative, y = max(plot_data$percentage) + 5, 
               label = "NOW", size = 5, fontface = "bold", fill = "white")
  }
  
  return(p)
}