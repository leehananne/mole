# Helper function to separate leg chunks from a flattened list
# Each chunk starts with "departureTime" and ends with "obstacles"
separate_leg_chunks <- function(legs_list) {
  if (!is.list(legs_list) || length(legs_list) == 0) {
    return(list())
  }
  
  # Check if this is already a list of leg objects (properly structured)
  if (is.list(legs_list[[1]]) && length(legs_list[[1]]) > 0 && 
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


# Helper function to print key-value pairs in a readable format
print_key_values <- function(obj, indent = "      ", max_depth = 3, current_depth = 0) {
  if (current_depth >= max_depth) {
    return()
  }
  
  if (is.null(obj)) {
    return()
  }
  
  if (is.list(obj)) {
    for (key in names(obj)) {
      value <- obj[[key]]
      
      if (is.null(value)) {
        message(indent, key, ": NULL")
      } else if (is.list(value) && length(value) > 0) {
        # Check if it's a simple list or nested structure
        if (length(value) <= 3 && all(sapply(value, function(x) !is.list(x) || length(x) == 0))) {
          # Simple list - print inline
          message(indent, key, ": [", paste(value, collapse = ", "), "]")
        } else {
          # Complex nested structure
          message(indent, key, ":")
          if (is.data.frame(value)) {
            message(indent, "  (data.frame: ", nrow(value), " rows, ", ncol(value), " cols)")
          } else if (length(value) > 0 && is.list(value[[1]])) {
            message(indent, "  (list of ", length(value), " items)")
            # Print first item's keys as example
            if (!is.null(value[[1]]) && is.list(value[[1]])) {
              message(indent, "  First item keys: ", paste(names(value[[1]]), collapse = ", "))
            }
          } else {
            print_key_values(value, paste0(indent, "  "), max_depth, current_depth + 1)
          }
        }
      } else if (is.character(value) && length(value) == 1 && nchar(value) > 100) {
        message(indent, key, ": ", substr(value, 1, 100), "... (", nchar(value), " chars)")
      } else if (is.data.frame(value)) {
        message(indent, key, ": (data.frame: ", nrow(value), " rows, columns: ", paste(names(value), collapse = ", "), ")")
      } else {
        # Simple value
        value_str <- paste(value, collapse = ", ")
        if (nchar(value_str) > 150) {
          value_str <- paste0(substr(value_str, 1, 150), "...")
        }
        message(indent, key, ": ", value_str)
      }
    }
  } else {
    message(indent, obj)
  }
}


# Core function to fetch journey data from TfL API
# Returns the full parsed_data object
fetch_journey_api <- function(origin_naptan, destination_naptan) {
  # Input validation
  if (is.null(origin_naptan) || origin_naptan == "" || 
      is.null(destination_naptan) || destination_naptan == "") {
    stop("Missing origin or destination naptan codes")
  }
  
  # Build URL
  base_url <- paste0("https://api.tfl.gov.uk/Journey/JourneyResults/", 
                     origin_naptan, "/to/", destination_naptan)
  
  # Query parameters matching TfL website format
  query_params <- list(
    nationalSearch = "false",
    journeyPreference = "LeastTime",
    accessibilityPreference = "NoRequirements"
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
  # Use simplifyVector = FALSE to preserve nested list structures
  parsed_data <- jsonlite::fromJSON(json_content, simplifyVector = FALSE)
  
  return(parsed_data)
}


# Simple wrapper to fetch and return parsed_data (for exploration/debugging)
fetch_journey_parsed_data <- function(origin_naptan, destination_naptan) {
  return(fetch_journey_api(origin_naptan, destination_naptan))
}


# Function to explore the structure of parsed_data$journeys and its legs
explore_journeys_structure <- function(parsed_data) {
  message("\n=== EXPLORING JOURNEYS STRUCTURE ===\n")
  
  # Step 1: Check if journeys exists
  if (is.null(parsed_data$journeys)) {
    message("ERROR: parsed_data$journeys is NULL")
    return(invisible(NULL))
  }
  
  journeys <- parsed_data$journeys
  
  # Step 2: Check type and length
  message("Step 1: Checking journeys object type and length")
  journeys_class <- class(journeys)
  journeys_length <- length(journeys)
  message("  class(parsed_data$journeys): ", paste(journeys_class, collapse = ", "))
  message("  length(parsed_data$journeys): ", journeys_length)
  message("")
  
  # Step 3: Handle different structures
  if (is.data.frame(journeys)) {
    message("Journeys is a data frame with ", nrow(journeys), " rows")
    message("Columns: ", paste(names(journeys), collapse = ", "))
    message("")
    
    # Check if legs column exists
    if ("legs" %in% names(journeys)) {
      message("Found 'legs' column in journeys data frame")
      message("")
      
      # Iterate through each journey
      for (i in seq_len(nrow(journeys))) {
        message("--- JOURNEY ", i, " ---")
        legs <- journeys$legs[[i]]
        
        if (is.null(legs)) {
          message("  Legs is NULL")
          message("")
          next
        }
        
        message("  Legs type: ", paste(class(legs), collapse = ", "))
        message("  Legs length: ", length(legs))
        message("")
        
        # Convert to list if it's a data frame
        if (is.data.frame(legs)) {
          message("  Converting legs data frame to list...")
          legs_list <- lapply(seq_len(nrow(legs)), function(j) {
            leg_list <- list()
            for (col in names(legs)) {
              leg_list[[col]] <- legs[[col]][[j]]
            }
            return(leg_list)
          })
          legs <- legs_list
        }
        
        # Now iterate through legs
        if (is.list(legs) && length(legs) > 0) {
          # Separate into leg chunks if needed
          legs_separated <- separate_leg_chunks(legs)
          
          for (j in seq_along(legs_separated)) {
            leg <- legs_separated[[j]]
            message("--- NEW LEG ", j, " (Journey ", i, ") ---")
            str(leg, max.level = 2)
            message("")
          }
        } else {
          message("  Legs is not a list or is empty")
          message("")
        }
      }
    } else {
      message("No 'legs' column found in journeys data frame")
      message("Available columns: ", paste(names(journeys), collapse = ", "))
    }
    
  } else if (is.list(journeys)) {
    message("Journeys is a list")
    message("")
    
    # Check if first element has a 'legs' field (meaning journeys is a list of journey objects)
    if (length(journeys) > 0 && is.list(journeys[[1]]) && "legs" %in% names(journeys[[1]])) {
      message("Journeys appears to be a list of journey objects (each with a 'legs' field)")
      message("")
      
      # Iterate through each journey
      for (i in seq_along(journeys)) {
        journey <- journeys[[i]]
        message("--- JOURNEY ", i, " ---")
        
        if (is.null(journey$legs)) {
          message("  Legs is NULL")
          message("")
          next
        }
        
        legs <- journey$legs
        message("  Legs type: ", paste(class(legs), collapse = ", "))
        message("  Legs length: ", length(legs))
        message("")
        
        # Separate into leg chunks if needed
        legs_separated <- separate_leg_chunks(legs)
        
        # Iterate through legs
        for (j in seq_along(legs_separated)) {
          leg <- legs_separated[[j]]
          message("--- NEW LEG ", j, " (Journey ", i, ") ---")
          str(leg, max.level = 2)
          message("")
        }
      }
    } else {
      # Maybe journeys is directly a list of legs?
      message("Journeys appears to be a list of legs (not journey objects)")
      message("")
      
      # Separate into leg chunks if needed
      legs_separated <- separate_leg_chunks(journeys)
      
      for (j in seq_along(legs_separated)) {
        leg <- legs_separated[[j]]
        message("--- NEW LEG ", j, " ---")
        str(leg, max.level = 2)
        message("")
      }
    }
  } else {
    message("Unexpected journeys type: ", paste(journeys_class, collapse = ", "))
  }
  
  message("=== END EXPLORING JOURNEYS STRUCTURE ===\n")
  return(invisible(journeys))
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
    
    return(list(
      leg_number = n,
      instruction_detailed = instruction_detailed,
      duration = duration,
      departure_name = departure_name,
      arrival_name = arrival_name,
      route_name = route_name
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
      stringsAsFactors = FALSE
    )
  }))
  
  # Add summary information
  attr(result, "num_legs") <- num_legs
  attr(result, "journey_index") <- journey_index
  
  return(result)
}


# Main function to fetch journey routes
# Returns the journeys data (data.frame or list)
# For detailed exploration, use explore_journeys_structure() on the parsed_data
fetch_journey_route <- function(origin_naptan, destination_naptan) {
  # Fetch data using core API function
  parsed_data <- fetch_journey_api(origin_naptan, destination_naptan)
  
  # Return journeys if available
  if (!is.null(parsed_data$journeys)) {
    journeys <- parsed_data$journeys
    
    # Return as-is (data.frame or list)
    return(journeys)
  }
  
  # Fallback: return full parsed data if journeys not found
  # return(parsed_data)
}

