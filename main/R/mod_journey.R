# JOURNEY MODULE
#   Travel journey routing
#   Journey output card

# ----- 1. -----
# Helper function to separate leg chunks from a flattened list
# Each chunk starts with "departureTime" and ends with "obstacles"
separate_leg_chunks <- function(legs_list) {
  if (!is.list(legs_list) || length(legs_list) == 0) {
    return(list())
  }
  
  # Check if legs_list itself is already a leg object (single leg case)
  if (!is.null(names(legs_list)) && 
      ("departureTime" %in% names(legs_list) || "$type" %in% names(legs_list))) {
    # This is a single leg object - wrap it in a list
    return(list(legs_list))
  }
  
  # Check if this is already a list of leg objects (properly structured)
  if (length(legs_list) > 0 && is.list(legs_list[[1]]) && length(legs_list[[1]]) > 0 && 
      !is.null(names(legs_list[[1]])) && 
      ("departureTime" %in% names(legs_list[[1]]) || "$type" %in% names(legs_list[[1]]))) {
    # Already separated into leg objects - return as is
    return(legs_list)
  }
  
  # It's a flat list - need to group by chunks
  leg_chunks <- list()
  current_chunk <- list()
  in_chunk <- FALSE
  
  for (i in seq_along(legs_list)) {
    item <- legs_list[[i]]
    
    # Check if this item indicates start of a chunk
    if (is.list(item) && !is.null(names(item)) && "departureTime" %in% names(item)) {
      # If we were already in a chunk, save it first
      if (in_chunk && length(current_chunk) > 0) {
        leg_chunks[[length(leg_chunks) + 1]] <- current_chunk
      }
      # Start new chunk
      current_chunk <- list(item)
      in_chunk <- TRUE
    } else if (in_chunk) {
      # We're in a chunk - add this item
      current_chunk[[length(current_chunk) + 1]] <- item
      
      # Check if this item indicates end of chunk
      if (is.list(item) && !is.null(names(item)) && "obstacles" %in% names(item)) {
        # End of chunk - save it
        leg_chunks[[length(leg_chunks) + 1]] <- current_chunk
        current_chunk <- list()
        in_chunk <- FALSE
      }
    }
  }
  
  # Add any remaining chunk
  if (in_chunk && length(current_chunk) > 0) {
    leg_chunks[[length(leg_chunks) + 1]] <- current_chunk
  }
  
  # Merge each chunk into a single leg object
  leg_objects <- list()
  for (chunk in leg_chunks) {
    leg_obj <- list()
    for (item in chunk) {
      if (is.list(item) && !is.null(names(item))) {
        # Merge this item's key-value pairs into leg_obj
        for (key in names(item)) {
          leg_obj[[key]] <- item[[key]]
        }
      }
    }
    if (length(leg_obj) > 0) {
      leg_objects[[length(leg_objects) + 1]] <- leg_obj
    }
  }
  
  return(leg_objects)
}


fetch_journey_api <- function(origin_naptan, destination_naptan, 
                               access_pref = "NoRequirements", 
                               journey_pref = "LeastTime") {
  # Input validation
  if (is.null(origin_naptan) || origin_naptan == "" || 
      is.null(destination_naptan) || destination_naptan == "") {
    stop("Missing origin or destination naptan codes")
  }
  
  # Validate and set defaults for preferences
  if (is.null(access_pref) || access_pref == "") {
    access_pref <- "NoRequirements"
  }
  if (is.null(journey_pref) || journey_pref == "") {
    journey_pref <- "LeastTime"
  }
  
  # Build API Call URL
  base_url <- paste0("https://api.tfl.gov.uk/Journey/JourneyResults/", 
                     origin_naptan, "/to/", destination_naptan)
  
  # Query parameters matching TfL website format
  query_params <- list(
    nationalSearch = "false",
    accessibilityPreference = access_pref,
    journeyPreference = journey_pref
  )
  
  # Make API request
  response <- httr::GET(base_url, query = query_params, httr::timeout(15))
  
  # Handle errors
  if (httr::status_code(response) == 400) {
    error_content <- httr::content(response, "text", encoding = "UTF-8")
    error_parsed <- tryCatch(
      jsonlite::fromJSON(error_content, flatten = TRUE),
      error = function(e) NULL
    )
    error_msg <- if (!is.null(error_parsed) && !is.null(error_parsed$message)) {
      error_parsed$message
    } else {
      error_content
    }
    stop(paste("Bad Request:", error_msg))
  }
  
  httr::stop_for_status(response)
  
  # Parse JSON response
  json_content <- httr::content(response, "text", encoding = "UTF-8")
  parsed_data <- jsonlite::fromJSON(json_content, simplifyVector = FALSE)
  
  return(parsed_data)
}

# Extract specific journey information from parsed_data
# Returns a data frame with leg details for the first journey
extract_journey_details <- function(parsed_data, journey_index = 1) {
  # Check if journeys exists
  if (is.null(parsed_data$journeys)) {
    stop("parsed_data$journeys is NULL")
  }
  
  journeys <- parsed_data$journeys
  
  # Handle both list and data.frame structures
  if (is.data.frame(journeys)) {
    if (journey_index > nrow(journeys)) {
      stop("journey_index exceeds number of journeys")
    }
    journey <- journeys[journey_index, ]
    legs <- journey$legs[[1]]
      } else if (is.list(journeys)) {
    if (journey_index > length(journeys)) {
      stop("journey_index exceeds number of journeys")
    }
    journey <- journeys[[journey_index]]
    legs <- journey$legs
  } else {
    stop("Unexpected journeys structure")
  }
  
  # Separate into leg chunks if needed
  legs_separated <- separate_leg_chunks(legs)
  num_legs <- length(legs_separated)
  
  # Extract information for each leg
  leg_details <- lapply(seq_along(legs_separated), function(n) {
    leg <- legs_separated[[n]]
    
    # Extract fields with safe access (handles NULLs)
    instruction_detailed <- if (!is.null(leg$instruction) && !is.null(leg$instruction$detailed)) {
      leg$instruction$detailed
              } else {
      NA_character_
    }
    
    duration <- if (!is.null(leg$duration)) {
      leg$duration
    } else {
      NA_integer_
    }
    
    departure_name <- if (!is.null(leg$departurePoint) && !is.null(leg$departurePoint$commonName)) {
      leg$departurePoint$commonName
    } else {
      NA_character_
    }
    
    arrival_name <- if (!is.null(leg$arrivalPoint) && !is.null(leg$arrivalPoint$commonName)) {
      leg$arrivalPoint$commonName
    } else {
      NA_character_
    }
    
    route_name <- if (!is.null(leg$routeOptions) && length(leg$routeOptions) > 0 && 
                      !is.null(leg$routeOptions[[1]]) && !is.null(leg$routeOptions[[1]]$name)) {
      leg$routeOptions[[1]]$name
    } else {
      NA_character_
    }
    
    departure_time <- if (!is.null(leg$departureTime)) {
      # Extract time portion (HH:MM) from datetime string e.g. "2025-11-12T13:57:00"
      # Get substring starting after "T" and take first 5 characters (HH:MM)
      time_str <- sub(".*T", "", leg$departureTime)
      substr(time_str, 1, 5)  # Extract HH:MM
    } else {
      NA_character_
    }
    
    arrival_time <- if (!is.null(leg$arrivalTime)) {
      # Extract time portion (HH:MM) from datetime string e.g. "2025-11-12T14:12:00"
      time_str <- sub(".*T", "", leg$arrivalTime)
      substr(time_str, 1, 5)  # Extract HH:MM
    } else {
      NA_character_
    }
    
    return(list(
      leg_number = n,
      instruction_detailed = instruction_detailed,
      duration = duration,
      departure_name = departure_name,
      arrival_name = arrival_name,
      route_name = route_name,
      departure_time = departure_time,
      arrival_time = arrival_time
    ))
  })
  
  # Convert to data frame
  result <- do.call(rbind, lapply(leg_details, function(x) {
    data.frame(
      leg_number = x$leg_number,
      instruction_detailed = x$instruction_detailed,
      duration = x$duration,
      departure_name = x$departure_name,
      arrival_name = x$arrival_name,
      route_name = x$route_name,
      departure_time = x$departure_time,
      arrival_time = x$arrival_time,
      stringsAsFactors = FALSE
    )
  }))
  
  # Add summary information
  attr(result, "num_legs") <- num_legs
  attr(result, "journey_index") <- journey_index
  
  # Print the result
  message("\n=== EXTRACTED JOURNEY DETAILS ===")
  message("Journey Index: ", journey_index)
  message("Number of Legs: ", num_legs)
  message("\nJourney Details:")
  print(result)
  message("=== END EXTRACTED JOURNEY DETAILS ===\n")
  
  return(result)
}

