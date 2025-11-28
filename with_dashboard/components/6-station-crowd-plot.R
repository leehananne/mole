library(ggplot2)
library(httr)
library(jsonlite)
library(scales)

get_station_name <- function(naptan_code) {
  url <- paste0("https://api.tfl.gov.uk/StopPoint/", naptan_code)
  resp <- httr::GET(url, httr::timeout(10))
  if (httr::status_code(resp) == 200) {
    data <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
    return(if (!is.null(data$commonName)) data$commonName else naptan_code)
  }
  return(naptan_code)
}

plot_crowd <- function(naptan_code) {
  
  if (is.null(naptan_code) || naptan_code == "") stop("Naptan code is required.")
  
  # --- 1. Time Setup ---
  now <- as.POSIXct(Sys.time())
  current_day_abbr <- format(now, "%a")
  current_hour <- as.numeric(format(now, "%H"))
  current_minute <- as.numeric(format(now, "%M"))
  
  # Calculate index (1-96)
  current_idx <- (current_hour * 4) + floor(current_minute / 15) + 1
  
  # --- 2. API Call ---
  base_url <- paste0("https://api.tfl.gov.uk/crowding/", naptan_code, "/", current_day_abbr)
  response <- httr::GET(base_url, httr::timeout(15))
  
  if (httr::status_code(response) == 400) stop("Bad Request.")
  httr::stop_for_status(response)
  
  json_content <- httr::content(response, "text", encoding = "UTF-8")
  parsed_data <- jsonlite::fromJSON(json_content, flatten = TRUE)
  crowding_data <- parsed_data$timeBands
  
  if (is.null(crowding_data) || nrow(crowding_data) == 0) stop("No data returned.")
  
  station_name <- get_station_name(naptan_code)
  
  # --- 3. Data Processing ---
  crowding_data$percentage <- crowding_data$percentageOfBaseLine * 100
  crowding_data$id <- 1:nrow(crowding_data)
  crowding_data$label_time <- sub("-.*", "", crowding_data$timeBand)
  
  # Window Logic: +/- 4 hours (16 slots each way)
  window_start <- max(1, current_idx - 16)
  window_end <- min(nrow(crowding_data), current_idx + 16)
  plot_data <- crowding_data[window_start:window_end, ]
  
  # Filter x-axis labels to only show hours (e.g. 17:00)
  breaks_subset <- plot_data[grepl(":00$", plot_data$label_time), ]
  
  # --- 4. Plotting ---
  p <- ggplot(plot_data, aes(x = id, y = percentage)) +
    
    # --- Context Zones (Optional: Adds background color blocks) ---
    # These rectangles tell the user what the % actually FEELS like
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 50, alpha = 0.05, fill = "green") +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 50, ymax = 100, alpha = 0.05, fill = "yellow") +
    
    # --- Context Labels (Text inside the graph) ---
    annotate("text", x = window_start, y = 15, label = " Quiet", hjust = 0, vjust = 0, color = "darkgreen", size = 4, fontface = "italic") +
    annotate("text", x = window_start, y = 65, label = " Busy", hjust = 0, vjust = 0, color = "orange", size = 4, fontface = "italic") +
    
    # The Line
    geom_line(color = "grey60", linewidth = 1) +
    
    # The Gradient Points
    geom_point(aes(color = percentage), size = 3.5) +
    
    # Current Time Marker
    geom_vline(xintercept = current_idx, linetype = "dotted", color = "black", linewidth = 0.8) +
    annotate("label", x = current_idx, y = max(plot_data$percentage) + 5, 
             label = "NOW", size = 5, fontface = "bold", fill = "white", label.size = 0) +
    
    # Scales
    scale_color_gradientn(
      colors = c("#00b33c", "#ffcc00", "#e32017"), 
      values = scales::rescale(c(0, 50, 80)), # Adjusted: 0=Green, 50=Yellow, 80+=Red
      name = "Crowd Intensity"
    ) +
    
    scale_x_continuous(breaks = breaks_subset$id, labels = breaks_subset$label_time) +
    
    # Ensure y-axis starts at 0 and has room for labels
    expand_limits(y = c(0, max(100, max(plot_data$percentage) + 10))) +
    
    labs(
      title = paste("Crowd forecast for", station_name),
      x = NULL,
      y = NULL
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 10, face = "bold"),
      legend.position = "none" 
    )
  
  return(p)
}