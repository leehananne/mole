# Example script to explore journeys structure
# This demonstrates how to use explore_journeys_structure()
# 
# Note: This script should be run from the 'refactor/' directory
# If running from a different directory, adjust the source path accordingly

# Source the journey_routing.R file to load the functions
# Try multiple possible paths
if (file.exists("R/journey_routing.R")) {
  source("R/journey_routing.R")
} else if (file.exists("refactor/R/journey_routing.R")) {
  source("refactor/R/journey_routing.R")
} else {
  stop("Cannot find journey_routing.R. Please run this script from the 'refactor/' directory.")
}

# Example: Fetch parsed_data for a journey
# You can replace these with your own naptan codes
origin_naptan <- "940GZZLUSKS"  # South Kensington
destination_naptan <- "940GZZLUWSP"  # St. Paul's

# Option 1: If you already have parsed_data, just use it directly
# explore_journeys_structure(parsed_data)

# Option 2: Fetch parsed_data using the helper function
message("Fetching journey data...")
parsed_data <- fetch_journey_parsed_data(origin_naptan, destination_naptan)

# Now explore the structure
explore_journeys_structure(parsed_data)

# Extract specific journey details
message("\n=== EXTRACTING JOURNEY DETAILS ===")
journey_details <- extract_journey_details(parsed_data, journey_index = 1)

# Show number of legs
message("Number of legs in journey 1: ", attr(journey_details, "num_legs"))

# Display the extracted details
print(journey_details)

