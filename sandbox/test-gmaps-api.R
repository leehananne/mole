library(httr)
library(jsonlite)
library(dplyr) # For easier data manipulation later

# --- Your API Key and Location ---
# Enter your Google Maps API key and Map ID
your_api_key <- ""
latitude <- 37.4220
longitude <- -122.0841

# --- Construct URL ---
base_url <- "https://weather.googleapis.com/v1/currentConditions:lookup"
full_url <- modify_url(base_url, query = list(
  key = your_api_key,
  "location.latitude" = latitude,
  "location.longitude" = longitude
))

# --- Make Request ---
response <- httr::GET(full_url)

# --- Process Response ---
if (status_code(response) == 200) {
  content_text <- content(response, "text", encoding = "UTF-8")
  
  # --- Key Step: Use flatten = TRUE ---
  weather_data_flat <- jsonlite::fromJSON(content_text, flatten = TRUE)
  
  # --- Convert the ENTIRE thing to a data frame (or tibble) ---
  # Often, the whole flattened list can become a 1-row data frame
  weather_df <- as.data.frame(weather_data_flat)
  # Or use tibble for potentially better handling
  # library(tibble)
  # weather_df <- as_tibble(weather_data_flat)
  
  
  # --- Add back location info ---
  # Since location isn't in the response, add it manually
  weather_df$Latitude <- latitude
  weather_df$Longitude <- longitude
  
  
  # --- Inspect the flattened data frame ---
  print("Flattened Weather Data Frame:")
  print(weather_df)
  str(weather_df) # Check the structure and column names
  
} else {
  print(paste("API Call Failed with status:", status_code(response)))
  print(content(response, "text", encoding = "UTF-8"))
}