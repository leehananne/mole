library(dplyr)
library(readr)

# 1. Read the CSV file
data <- read_csv("2-station-info-testing/stationpoints.csv", show_col_types = FALSE)

# 2. Analyze the data
# Group by StationUniqueId and count how many distinct levels exist for each
stations_single_level <- data %>%
  group_by(StationUniqueId) %>%
  summarise(
    Distinct_Levels_Count = n_distinct(Level),
    The_Level = first(Level) # Capture the level (since they are all the same)
  ) %>%
  filter(Distinct_Levels_Count == 1)

# 3. Display the results
if (nrow(stations_single_level) > 0) {
  print(paste("Found", nrow(stations_single_level), "stations located entirely on a single level."))
  
  # Print the ID and the Level of the found stations
  print(stations_single_level %>% select(StationUniqueId, The_Level))
  
  # Optional: If you want to see the full original rows for these stations:
  # full_details <- data %>% 
  #   filter(StationUniqueId %in% stations_single_level$StationUniqueId)
  # View(full_details)
  
} else {
  print("No stations found where all entries are on the same level.")
}
