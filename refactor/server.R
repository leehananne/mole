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

# Helper function to explore and print TfL entity structure
explore_tfl_entity <- function(entity, entity_name = "Entity", indent = "      ") {
  if (is.null(entity)) {
    message(indent, entity_name, ": NULL")
    return()
  }
  
  # Check if it's a TfL entity (has $type field)
  if (is.list(entity) && !is.null(entity$`$type`)) {
    message(indent, entity_name, " [", entity$`$type`, "]:")
    # Print all keys except $type (we already showed it)
    keys <- names(entity)
    keys <- keys[keys != "$type"]
    
    for (key in keys) {
      value <- entity[[key]]
      if (is.null(value)) {
        message(indent, "  ", key, ": NULL")
      } else if (is.list(value)) {
        if (length(value) == 0) {
          message(indent, "  ", key, ": [] (empty list)")
        } else if (!is.null(value$`$type`)) {
          # It's a nested TfL entity
          explore_tfl_entity(value, key, paste0(indent, "  "))
        } else if (is.data.frame(value)) {
          message(indent, "  ", key, ": data.frame with ", nrow(value), " rows, columns: ", paste(names(value), collapse = ", "))
        } else if (length(value) > 0 && is.list(value[[1]]) && !is.null(value[[1]]$`$type`)) {
          # It's a list of TfL entities
          message(indent, "  ", key, ": list of ", length(value), " entities")
          for (k in seq_along(value)) {
            if (!is.null(value[[k]])) {
              explore_tfl_entity(value[[k]], paste0(key, "[", k, "]"), paste0(indent, "    "))
            }
          }
        } else {
          # Regular list - show first few items
          if (length(value) <= 3) {
            message(indent, "  ", key, ": ", paste(value, collapse = ", "))
          } else {
            message(indent, "  ", key, ": list with ", length(value), " items (first: ", value[[1]], ")")
          }
        }
      } else if (is.character(value) && length(value) == 1 && nchar(value) > 100) {
        message(indent, "  ", key, ": ", substr(value, 1, 100), "... (", nchar(value), " chars)")
      } else {
        message(indent, "  ", key, ": ", paste(value, collapse = ", "))
      }
    }
  } else if (is.list(entity)) {
    # Regular list without $type
    message(indent, entity_name, " (list):")
    message(indent, "  Keys: ", paste(names(entity), collapse = ", "))
    # Show a few key fields if they exist
    if ("commonName" %in% names(entity)) {
      message(indent, "  commonName: ", entity$commonName)
    }
    if ("lat" %in% names(entity) && "lon" %in% names(entity)) {
      message(indent, "  Location: (", entity$lat, ", ", entity$lon, ")")
    }
  } else {
    message(indent, entity_name, ": ", entity, " (", class(entity), ")")
  }
}

# Helper function to format ISO 8601 datetime strings to readable format
format_datetime <- function(datetime_str) {
  if (is.null(datetime_str) || datetime_str == "" || is.na(datetime_str)) {
    return("")
  }
  
  # If it's already a POSIXct object, format it directly
  if (inherits(datetime_str, "POSIXct") || inherits(datetime_str, "POSIXlt")) {
    formatted <- format(datetime_str, "%d %B %Y, %I:%M %p")
    return(formatted)
  }
  
  # Convert to character if it's not already
  datetime_str <- as.character(datetime_str)
  
  tryCatch({
    # Handle various ISO 8601 formats:
    # "2025-11-12T10:39:00"
    # "2025-11-12T10:39:00Z"
    # "2025-11-12T10:39:00+00:00"
    # "2025-11-12T10:39:00.000Z" (with milliseconds)
    
    # Remove milliseconds if present (e.g., ".000")
    clean_str <- gsub("\\.\\d+", "", datetime_str)
    # Remove timezone info if present (e.g., "Z" or "+00:00" or "-00:00")
    clean_str <- gsub("Z$|[+-]\\d{2}:\\d{2}$", "", clean_str)
    
    # Try parsing with the standard format
    dt <- as.POSIXct(clean_str, format = "%Y-%m-%dT%H:%M:%S", tz = "Europe/London")
    
    # Check if parsing was successful (NA means it failed)
    if (is.na(dt)) {
      # Try alternative format without seconds
      dt <- as.POSIXct(clean_str, format = "%Y-%m-%dT%H:%M", tz = "Europe/London")
    }
    
    # If still NA, return original
    if (is.na(dt)) {
      return(datetime_str)
    }
    
    # Format to: "12 November 2025, 10:39 AM"
    formatted <- format(dt, "%d %B %Y, %I:%M %p")
    return(formatted)
  }, error = function(e) {
    # If parsing fails, return original string
    message("Warning: Could not parse datetime string: ", datetime_str, " - Error: ", e$message)
    return(datetime_str)
  })
}

fetch_journey_route <- function(origin_naptan, destination_naptan) {
  message("\n=== DEBUG: fetch_journey_route START ===")
  message("Step 1: Input validation")
  message("  Origin naptan: ", origin_naptan)
  message("  Destination naptan: ", destination_naptan)
  
  if (is.null(origin_naptan) || origin_naptan == "" || 
      is.null(destination_naptan) || destination_naptan == "") {
    message("  ERROR: Missing inputs - returning NULL")
    message("=== DEBUG END (Missing inputs) ===\n")
    return(NULL)
  }
  
  message("Step 2: Building base URL")
  base_url <- paste0("https://api.tfl.gov.uk/Journey/JourneyResults/", 
                     origin_naptan, "/to/", destination_naptan)
  message("  Base URL: ", base_url)
  
  message("Step 3: Setting up query parameters")
  query_params <- list(
    journeyPreference = "LeastInterchange",
    mode = "tube",  
    accessibilityPreference = "NoRequirements",
    cyclePreference = "None",
    alternativeCycle = "false",
    alternativeWalking = "false",
    useMultiModalCall = "false",
    taxiOnlyTrip = "false",
    routeBetweenEntrances = "true",
    useRealTimeLiveArrivals = "true",
    calcOneDirection = "true",
    includeAlternativeRoutes = "false"
  )
  message("  Number of query params: ", length(query_params))
  message("  Query param names: ", paste(names(query_params), collapse = ", "))
  
  tryCatch({
    message("Step 4: Making GET request...")
    response <- httr::GET(base_url, query = query_params, httr::timeout(15))
    
    message("Step 5: Response received")
    status <- httr::status_code(response)
    message("  HTTP Status Code: ", status)
    message("  Full URL called: ", response$url)
    
    # Check status and get error details if 400
    if (status == 400) {
      message("Step 6: ERROR 400 - Bad Request detected")
      error_content <- httr::content(response, "text", encoding = "UTF-8")
      message("  Error response content:")
      message("  ", error_content)
      
      error_parsed <- tryCatch(
        jsonlite::fromJSON(error_content, flatten = TRUE),
        error = function(e) {
          message("  Could not parse error as JSON")
          return(NULL)
        }
      )
      
      if (!is.null(error_parsed) && !is.null(error_parsed$message)) {
        message("  Parsed error message: ", error_parsed$message)
        stop(paste("Bad Request:", error_parsed$message))
      } else {
        stop(paste("Bad Request:", error_content))
      }
    }
    
    message("Step 6: Checking response status...")
    httr::stop_for_status(response)
    
    message("Step 7: Extracting JSON content")
    json_content <- httr::content(response, "text", encoding = "UTF-8")
    message("  JSON content length: ", nchar(json_content), " characters")
    message("  First 200 chars: ", substr(json_content, 1, 200))
    
    message("Step 8: Parsing JSON...")
    # Parse with simplifyVector = FALSE to preserve nested list structures
    # This prevents jsonlite from converting arrays to data frames when they contain objects
    parsed_data <- jsonlite::fromJSON(json_content, simplifyVector = FALSE)
    message("  Parsing successful!")
    message("  Parsed data type: ", class(parsed_data))
    message("  Top-level keys: ", paste(names(parsed_data), collapse = ", "))
    
    # Check if journeys exists and handle both list and data frame cases
    if (!is.null(parsed_data$journeys)) {
      journeys <- parsed_data$journeys
      message("  Journeys type: ", class(journeys))
      
      if (is.data.frame(journeys)) {
        message("  Journeys is a data frame with ", nrow(journeys), " rows and ", ncol(journeys), " columns")
        if (nrow(journeys) > 0) {
          message("\n  === JOURNEYS DATA FRAME STRUCTURE ===")
          message("  Column names: ", paste(names(journeys), collapse = ", "))
          message("\n  Column details:")
          for (col in names(journeys)) {
            col_data <- journeys[[col]]
            col_type <- class(col_data)
            if (is.list(col_data)) {
              message("    ", col, ": list (", length(col_data), " elements)")
              if (length(col_data) > 0) {
                first_elem <- col_data[[1]]
                if (!is.null(first_elem)) {
                  message("      First element type: ", class(first_elem))
                  if (is.list(first_elem)) {
                    message("      First element keys: ", paste(names(first_elem), collapse = ", "))
                  } else if (is.data.frame(first_elem)) {
                    message("      First element: data.frame (", nrow(first_elem), " rows, ", ncol(first_elem), " cols)")
                  }
                }
              }
            } else if (is.data.frame(col_data)) {
              message("    ", col, ": data.frame (", nrow(col_data), " rows, ", ncol(col_data), " cols)")
            } else {
              message("    ", col, ": ", paste(col_type, collapse = ", "))
              if (length(col_data) <= 5) {
                message("      Values: ", paste(col_data, collapse = ", "))
              } else {
                first_n <- min(3, length(col_data))
                message("      First ", first_n, " values: ", paste(col_data[seq_len(first_n)], collapse = ", "))
              }
            }
          }
          message("\n  === JOURNEYS DATA FRAME CONTENTS ===")
          for (i in seq_len(nrow(journeys))) {
            message("\n  Row ", i, " (Journey ", i, "):")
            for (col in names(journeys)) {
              value <- journeys[[col]][[i]]
              if (is.null(value)) {
                message("    ", col, ": NULL")
              } else if (is.list(value)) {
                message("    ", col, ": list (", length(value), " items)")
                if (length(value) > 0 && is.list(value[[1]])) {
                  message("      First item keys: ", paste(names(value[[1]]), collapse = ", "))
                }
              } else if (is.data.frame(value)) {
                message("    ", col, ": data.frame (", nrow(value), " rows)")
              } else if (is.character(value) && length(value) == 1 && nchar(value) > 100) {
                message("    ", col, ": ", substr(value, 1, 100), "... (", nchar(value), " chars)")
              } else {
                value_str <- paste(value, collapse = ", ")
                if (nchar(value_str) > 150) {
                  value_str <- paste0(substr(value_str, 1, 150), "...")
                }
                message("    ", col, ": ", value_str)
              }
            }
          }
          message("\n  === END JOURNEYS DATA FRAME ===\n")
          
          # Print data frame as a table
          message("\n  === JOURNEYS DATA FRAME AS TABLE ===")
          message("  (Complex columns like 'legs' are summarized)")
          
          # Create a simplified version for table display
          journeys_table <- journeys
          
          # Simplify complex columns for display
          for (col in names(journeys_table)) {
            if (is.list(journeys_table[[col]])) {
              # Convert list column to character summary
              journeys_table[[col]] <- sapply(journeys_table[[col]], function(x) {
                if (is.null(x)) {
                  "NULL"
                } else if (is.list(x)) {
                  paste0("list (", length(x), " items)")
                } else if (is.data.frame(x)) {
                  paste0("data.frame (", nrow(x), "x", ncol(x), ")")
                } else {
                  paste(x, collapse = ", ")
                }
              })
            } else if (is.data.frame(journeys_table[[col]])) {
              journeys_table[[col]] <- paste0("data.frame (", nrow(journeys_table[[col]]), "x", ncol(journeys_table[[col]]), ")")
            }
          }
          
          # Print the table
          print(journeys_table)
          
          # Also print the full structure using str() for reference
          message("\n  === JOURNEYS DATA FRAME FULL STRUCTURE (str) ===")
          str(journeys, max.level = 2, give.attr = FALSE)
          
          message("\n  === END TABLE DISPLAY ===\n")
          
          if ("duration" %in% names(journeys)) {
            duration <- journeys$duration[1]
            message("  First journey duration: ", duration, " minutes")
          }
          if ("startDateTime" %in% names(journeys)) {
            raw_start <- journeys$startDateTime[1]
            message("  startDateTime (raw): ", raw_start, " (class: ", class(raw_start), ")")
            formatted_start <- format_datetime(raw_start)
            message("  startDateTime (formatted): ", formatted_start)
          }
          if ("arrivalDateTime" %in% names(journeys)) {
            raw_arrival <- journeys$arrivalDateTime[1]
            message("  arrivalDateTime (raw): ", raw_arrival, " (class: ", class(raw_arrival), ")")
            formatted_arrival <- format_datetime(raw_arrival)
            message("  arrivalDateTime (formatted): ", formatted_arrival)
          }
          
          # Print legs for each journey
          message("\n  === JOURNEY LEGS ===")
          message("  Checking if legs column exists: ", "legs" %in% names(journeys))
          if ("legs" %in% names(journeys)) {
            message("  Legs column type: ", class(journeys$legs))
            if (length(journeys$legs) > 0) {
              message("  First journey legs type: ", class(journeys$legs[[1]]))
              message("  First journey legs is.null: ", is.null(journeys$legs[[1]]))
              if (!is.null(journeys$legs[[1]])) {
                message("  First journey legs structure: ", paste(class(journeys$legs[[1]]), collapse = ", "))
                if (is.data.frame(journeys$legs[[1]])) {
                  message("  First journey legs is a data frame with ", nrow(journeys$legs[[1]]), " rows")
                  message("  First journey legs columns: ", paste(names(journeys$legs[[1]]), collapse = ", "))
                }
              }
            }
          }
          
          for (i in seq_len(nrow(journeys))) {
            message("\n  Journey ", i, ":")
            if ("legs" %in% names(journeys) && !is.null(journeys$legs[[i]])) {
              legs_raw <- journeys$legs[[i]]
              
              # Convert legs to a list if it's a data frame
              if (is.data.frame(legs_raw)) {
                message("    Legs is a data frame with ", nrow(legs_raw), " rows")
                message("    Legs columns: ", paste(names(legs_raw), collapse = ", "))
                
                # Convert each row to a list, handling nested structures
                legs <- lapply(seq_len(nrow(legs_raw)), function(j) {
                  leg_list <- list()
                  for (col in names(legs_raw)) {
                    value <- legs_raw[[col]][[j]]
                    if (is.data.frame(value)) {
                      # If it's a data frame (nested), convert to list of lists
                      if (nrow(value) == 1) {
                        leg_list[[col]] <- as.list(value[1, ])
                      } else {
                        leg_list[[col]] <- lapply(seq_len(nrow(value)), function(k) as.list(value[k, ]))
                      }
                    } else if (is.list(value) && length(value) > 0 && is.data.frame(value[[1]])) {
                      # List of data frames
                      leg_list[[col]] <- lapply(value, function(df) {
                        if (is.data.frame(df) && nrow(df) == 1) {
                          as.list(df[1, ])
                        } else {
                          df
                        }
                      })
                    } else {
                      leg_list[[col]] <- value
                    }
                  }
                  return(leg_list)
                })
                message("    Converted to list of ", length(legs), " legs")
              } else if (is.list(legs_raw)) {
                legs <- legs_raw
              } else {
                message("    Unexpected legs type: ", class(legs_raw))
                legs <- list()
              }
              
              message("    Number of legs: ", length(legs))
              
              # Handle legs - should now be a list
              if (is.list(legs)) {
                if (length(legs) == 0) {
                  message("    Legs is an empty list")
                } else {
                  # Separate into leg chunks
                  legs_separated <- separate_leg_chunks(legs)
                  message("    Number of leg chunks: ", length(legs_separated))
                  
                  # Print each leg chunk separately
                  for (j in seq_along(legs_separated)) {
                    leg <- legs_separated[[j]]
                    message("\n    ", rep("=", 60))
                    message("    Leg Chunk ", j, ":")
                    message("    ", rep("=", 60))
                    if (is.list(leg)) {
                      # Print all key-value pairs for this leg
                      print_key_values(leg, "      ")
                    } else {
                      message("      Value: ", paste(leg, collapse = ", "), " (", class(leg), ")")
                    }
                    message("    ", rep("-", 60))
                  }
                }
              } else if (is.data.frame(legs)) {
                message("    Legs is a data frame with ", nrow(legs), " rows")
                message("    Leg columns: ", paste(names(legs), collapse = ", "))
              } else {
                message("    Legs is of unexpected type: ", class(legs))
                message("    Legs value: ", paste(legs, collapse = ", "))
              }
            } else {
              message("    No legs found for journey ", i)
              if ("legs" %in% names(journeys)) {
                message("    But legs column exists! Type: ", class(journeys$legs))
                message("    legs[[", i, "]] is.null: ", is.null(journeys$legs[[i]]))
                message("    legs[[", i, "]] class: ", class(journeys$legs[[i]]))
                if (!is.null(journeys$legs[[i]])) {
                  message("    legs[[", i, "]] length: ", length(journeys$legs[[i]]))
                }
              } else {
                message("    legs column does not exist in journeys data frame")
              }
            }
          }
          message("\n  === END JOURNEY LEGS ===\n")
        }
      } else if (is.list(journeys)) {
        # Handle case where journeys is a list
        message("  Journeys is a list with ", length(journeys), " elements")
        message("\n  === JOURNEY LEGS ===")
        for (i in seq_along(journeys)) {
          message("\n  Journey ", i, ":")
          journey <- journeys[[i]]
          if (is.list(journey) && !is.null(journey$legs)) {
            legs_raw <- journey$legs
            message("    Raw legs count: ", length(legs_raw))
            
            # Separate into leg chunks
            legs <- separate_leg_chunks(legs_raw)
            message("    Number of leg chunks: ", length(legs))
            
            # Print each leg chunk separately
            for (j in seq_along(legs)) {
              leg <- legs[[j]]
              message("\n    ", rep("=", 60))
              message("    Leg Chunk ", j, ":")
              message("    ", rep("=", 60))
              if (is.list(leg) && length(leg) > 0) {
                # Print all key-value pairs for this leg
                print_key_values(leg, "      ")
              } else {
                message("      Value: ", paste(leg, collapse = ", "), " (", class(leg), ")")
              }
              message("    ", rep("-", 60))
            }
          }
        }
        message("\n  === END JOURNEY LEGS ===\n")
      }
      
      message("=== DEBUG END (SUCCESS) ===\n") 
    }
  }, error = function(e) {
    message("=== DEBUG END (ERROR) ===\n")
    message("Error: ", e$message)
    stop(e)
  })
  
  # return(parsed_data)
}

