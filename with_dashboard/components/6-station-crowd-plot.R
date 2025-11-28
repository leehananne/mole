plot_crowd <- function(naptan_code) {
  # --- 1. Setup Time ---
  now <- as.POSIXct(Sys.time())
  current_day_abbr <- format(now, "%a")
  current_hour <- as.numeric(format(now, "%H"))
  current_minute <- as.numeric(format(now, "%M"))

  # Calculate the 1-based index for the current 15-minute slot
  # (Hour * 4) + (Minute / 15) + 1 for R indexing
  current_slot_index <- (current_hour * 4) + floor(current_minute / 15) + 1

  message("Looking for data for: ", current_day_abbr)
  message("Current Time Slot Index: ", current_slot_index, " (approx ", format(now, "%H:%M"), ")")

  # --- 2. Call API ---
  base_url <- paste0("https://api.tfl.gov.uk/crowding/", naptan_code, "/", current_day_abbr)
  response <- httr::GET(base_url, httr::timeout(15))

  if (httr::status_code(response) == 400) {
    stop("Bad Request: Please check the Naptan code.")
  }
  httr::stop_for_status(response)

  # --- 3. Parse Data ---
  json_content <- httr::content(response, "text", encoding = "UTF-8")
  parsed_data <- jsonlite::fromJSON(json_content, flatten = TRUE)

  # Extract the data frame
  crowding_data <- parsed_data$timeBands

  # --- 4. Logic for Highlighting ---
  crowding_data$is_current <- FALSE

  if (current_slot_index <= nrow(crowding_data)) {
    crowding_data$is_current[current_slot_index] <- TRUE
    message("Highlighting time band: ", crowding_data$timeBand[current_slot_index])
  }

  # --- 5. Plotting ---
  p <- ggplot(crowding_data, aes(x = timeBand, y = percentageOfBaseLine * 100, fill = is_current)) +
    geom_col(width = 0.8) +
    scale_fill_manual(values = c("FALSE" = "#0019a8", "TRUE" = "#E32017"), guide = "none") +
    geom_hline(yintercept = 100, linetype = "dashed", color = "gray50") +
    labs(
      title = paste("Crowding Profile:", parsed_data$naptan),
      subtitle = paste("Day:", parsed_data$dayOfWeek, "| Red bar indicates current time"),
      x = "Time of Day (15 min intervals)",
      y = "% of Baseline Crowd"
    ) +
    theme_minimal() +
    scale_x_discrete(breaks = crowding_data$timeBand[seq(1, nrow(crowding_data), by = 8)]) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      panel.grid.major.x = element_blank()
    )

  print(p)
}


