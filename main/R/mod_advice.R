# PRESCRIPTIVE ADVICE MODULE
# Combines the visual "Mole" score with Prescriptive Text Advice
#   generate_prescriptive_suggestions
#   mod_advice_ui
#   mod_advice_server

# ==============================================================================
# Function: Generate Prescriptive Suggestions Based on Data
# ==============================================================================
# Arguments: forecast dataframe

generate_prescriptive_suggestions <- function(comfort_data, weather_data, crowd_data) {
  
  # Initialise the suggestions list
  suggestions <- list(
    comfort = NULL,
    weather = NULL,
    crowd   = NULL
  )
  
  # Comfort Logic
  
  if (!is.null(comfort_data) && nrow(comfort_data) >= 1) {
    comfort_max <- which.max(comfort_data$Comfort_Index) # Index of max
    
    # Convert relative hour (1, 2...) to "HH:00" string
    get_time_display <- function(rel_hour) {
      future_time <- Sys.time() + (rel_hour * 3600)
      format(future_time, "%H:00")
    }
    
    # Logic A: Now (Hour 1) is best
    if (comfort_max == 1) {
      suggestions$comfort <- "Now is the time with the best comfort index for the next 4 hours."
    } else {
      # Check trend (Is it strictly increasing?)
      # Check difference between consecutive elements
      is_increasing <- all(diff(comfort_data$Comfort_Index) >= 0)
      
      if (is_increasing) {
        # Logic B: Trend Increasing
        suggestions$comfort <- "Comfort index is projected to increase over the next 4 hours."
      } else {
        # Logic C: Max in between
        best_time <- get_time_display(comfort_data$Hour[comfort_max])
        suggestions$comfort <- paste0("The best comfort level of ", comfort_data$Comfort_Index[comfort_max], " is expected at ", best_time, ".")
      }
    }
  } else {
    suggestions$comfort <- "Insufficient comfort data available."
  }
  
  
  # Weather Logic
  # Columns: Hour (0=Current, 1..N=Forecast), PrecipProb (0-100)
  
  if (!is.null(weather_data) && nrow(weather_data) >= 1) {
    
    # Filter for FUTURE hours only (Hour >= 1) to ignore "Current Condition" row
    df_w <- weather_data[weather_data$Hour >= 1, ]
    
    if (nrow(df_w) > 0) {
      n_check <- min(nrow(df_w), 4)
      df_w <- df_w[1:n_check, ]
      
      current_pop <- df_w$PrecipProb[1] # "Hour 1" forecast
      max_pop     <- max(df_w$PrecipProb, na.rm = TRUE)
      min_pop     <- min(df_w$PrecipProb, na.rm = TRUE)
      
      max_idx <- which.max(df_w$PrecipProb)
      min_idx <- which.min(df_w$PrecipProb)
      
      # Convert relative hour to "HH:00"
      get_time_display_w <- function(rel_hour) {
        future_time <- Sys.time() + (rel_hour * 3600)
        format(future_time, "%H:00")
      }
      
      # Logic A: Stable (Difference < 5%)
      if ((max_pop - min_pop) < 5) {
        suggestions$weather <- "The weather condition is likely to be the constant for the next 4 hours."
        
      } else {
        # Determine if the dominant change is Increase or Decrease
        rise_mag <- max_pop - current_pop
        fall_mag <- current_pop - min_pop
        
        if (rise_mag >= fall_mag) {
          # Logic B: Increasing Rain Probability
          time_max <- get_time_display_w(df_w$Hour[max_idx])
          suggestions$weather <- paste0(
            "The probability of precipitation is likely to increase from ", 
            round(current_pop), "% to ", round(max_pop), "% around ", time_max, "."
          )
        } else {
          # Logic C: Decreasing Rain Probability
          time_min <- get_time_display_w(df_w$Hour[min_idx])
          suggestions$weather <- paste0(
            "The probability of precipitation is likely to reduce from ", 
            round(current_pop), "% (now) to ", round(min_pop), "% around ", time_min, "."
          )
        }
      }
    } else {
      suggestions$weather <- "Insufficient forecast data available."
    }
  } else {
    suggestions$weather <- "Insufficient weather data available."
  }
  
  # Crowd Logic (Next 2 hours / Every 15 minutes)
  # Columns: TimeBand, CrowdingScore (0.0 - 1.0)
  
  if (!is.null(crowd_data) && nrow(crowd_data) >= 1) {
    # Limit to next 2 hours (8 data points)
    n_check <- min(nrow(crowd_data), 8)
    df_cr <- crowd_data[1:n_check, ]
    
    # Convert score (0.4) to percentage (40)
    df_cr$Pct <- df_cr$CrowdingScore * 100
    crowd_msgs <- c()
    
    # Check 1: Lowest Moment
    min_val <- min(df_cr$Pct, na.rm = TRUE)
    min_idx <- which.min(df_cr$Pct)
    
    # Only suggest if the minimum is NOT now (index 1)
    if (min_idx > 1) {
      current_val <- df_cr$Pct[1]
      diff_pp <- round(current_val - min_val, 1) # percentage point difference
      best_crowd <- df_cr$TimeBand[min_idx]
      
      msg_lowest <- paste0(
        "If you depart at ", best_crowd, ",",
        " the crowd will be ", diff_pp, "%p less than now. (", round(min_val), "%)"
      )
      crowd_msgs <- c(crowd_msgs, msg_lowest)
    }
    
    # Check 2: Soonest Decreasing Moment
    # Calculate drops between consecutive periods
    if (nrow(df_cr) > 1) {
      changes <- diff(df_cr$Pct) # Length is n-1
      
      # We want the most negative change (largest drop)
      min_change <- min(changes, na.rm = TRUE)
      
      # Only report if there is an actual drop (change < 0)
      if (min_change < 0) {
        drop_idx <- which.min(changes) 
        
        # Target time is the destination of the drop (i+1 in diff corresponds to index i+1 in original)
        time_drop <- df_cr$TimeBand[drop_idx + 1]
        drop_amount <- abs(round(min_change, 1))
        
        msg_drop <- paste0(
          "The soonest time with less crowd is ", time_drop, 
          " (-", drop_amount, "%p less)."
        )
        crowd_msgs <- c(crowd_msgs, msg_drop)
      }
    }
    
    # Combine messages (Max 2)
    if (length(crowd_msgs) == 0) {
      suggestions$crowd <- "This is the time with the lowest crowd level for next 2 hours." # Crowd levels are expected to remain stable or high.
    } else {
      suggestions$crowd <- paste(crowd_msgs, collapse = " ")
    }
    
  } else {
    suggestions$crowd <- "Insufficient crowd data available."
  }
  
  return(suggestions)
}


# ==============================================================================
# UI for Left Panel Advice Block
# ==============================================================================

mod_advice_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
        /* Container */
        .advice-widget {
          font-family: 'Arial', sans-serif;
          max-width: 500px;
          margin: 10px 0;
        }

        /* --- Top Section: Mole + Bubble --- */
        .advice-top {
          display: flex;
          align-items: center;
          margin-bottom: 15px;
        }

        .mole-container {
          flex-shrink: 0;
          margin-right: 20px;
        }
        
        .mole-img {
          width: 120px;
          height: 100px;
          object-fit: contain;
        }

        /* Speech Bubble Style */
        .score-bubble {
          position: relative;
          background: #ffffff;
          border: 1px solid #e0e0e0;
          border-radius: 15px;
          padding: 15px 25px;
          box-shadow: 0 2px 5px rgba(0,0,0,0.05);
          flex-grow: 1;
          display: flex;
          flex-direction: column;
          justify-content: center;
        }

        .score-bubble::before {
          content: ''; position: absolute; left: -10px; top: 40%; transform: translateY(-50%);
          width: 0; height: 0; border-top: 10px solid transparent; border-bottom: 10px solid transparent;
          border-right: 10px solid #e0e0e0;
        }
        
        .score-bubble::after {
          content: ''; position: absolute; left: -8px; top: 40%; transform: translateY(-50%);
          width: 0; height: 0; border-top: 9px solid transparent; border-bottom: 9px solid transparent;
          border-right: 9px solid #ffffff;
        }

        .bubble-label {
          font-size: 14px; font-weight: bold; color: #555; margin-bottom: 5px; text-transform: capitalize;
        }

        .bubble-value-row { display: flex; align-items: baseline; }

        .score-value {
          font-family: 'Darumadrop';
          font-size: 42px; line-height: 1; margin-right: 5px;
        }

        .score-max { font-size: 18px; color: #999; font-weight: bold; }

        /* --- Bottom Section: Advice Box --- */
        .advice-box-container {
          background-color: #f0f7f4; /* Very Light Green/Grey */
          border-left: 5px solid #00b33c;
          border-radius: 4px;
          padding: 15px;
          min-height: 100px;
        }

        .advice-title {
          font-weight: bold; font-size: 16px; margin-bottom: 12px; color: #2c3e50;
        }
        
        .advice-item {
          display: flex; align-items: flex-start; margin-bottom: 8px; font-size: 14px; color: #34495e;
        }
        
        .advice-item i {
          margin-top: 3px; margin-right: 10px; width: 20px; text-align: center; color: #00b33c;
        }
      "))
    ),
    
    # The Output Container
    uiOutput(ns("advice_output"))
  )
}


# ==============================================================================
# Server: Server for displaying the prescriptive mole advices
# ==============================================================================

mod_advice_server <- function(id, comfort_df, suggestions) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive Data Fetching
    advice_data <- reactive({
      req(comfort_df(), suggestions())
      df <- comfort_df()
      sug <- suggestions()
      list(comfort_df = df, suggestions = sug)
    })

    # Helpers for Colour & Image
    get_text_color <- function(score) {
      if (is.na(score)) return("#000000")
      s <- max(0, min(10, score))
      
      if (s <= 5) {
        ratio <- s / 5; palette_func <- colorRamp(c("#e32017", "#ffcc00"))
      } else {
        ratio <- min(1, (s - 5) / (8 - 5)); palette_func <- colorRamp(c("#ffcc00", "#00b33c"))
      }
      rgb_vals <- palette_func(ratio)
      return(rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], maxColorValue = 255))
    }
    
    get_mole_image <- function(score, max_future_score) {
      if (is.na(score)) return("mole-1.png")
      
      # If current score is "Okay" (4-6) but it is the maximum in the forecast window,
      # Make it smiling :) instead of neutral
      if (score >= 4 && score < 6) {
        if (!is.na(max_future_score) && score >= max_future_score) {
          return("mole-4.png") 
        }
      }
      if (score < 3) return("mole-1.png")
      if (score < 4.5) return("mole-2.png")
      if (score < 6) return("mole-3.png")
      if (score < 7.4) return("mole-4.png")
      return("mole-5.png")
    }
    
    # Render UI
    output$advice_output <- renderUI({
      
      res <- advice_data()
      
      # Default State (No Data)
      if (is.null(res) || is.null(res$comfort_df)) {
        score <- 0
        score_text <- "-"
        mole_file <- "mole-1.png"
        text_color <- "#333"
        suggestions <- list(comfort="-", weather="-", crowd="-")
      } else {
        # Extract Current Score (Row 1)
        current_data <- res$comfort_df[1, ]
        score <- current_data$Comfort_Index
        
        max_future <- max(res$comfort_df$Comfort_Index, na.rm = TRUE)
        score_text <- format(round(score, 1), nsmall = 1)
        mole_file <- get_mole_image(score, max_future)
        text_color <- get_text_color(score)
        suggestions <- res$suggestions
      }
      
      div(class = "advice-widget",
          
          # Top Section (Mole + Bubble)
          div(class = "advice-top",
              div(class = "mole-container",
                  tags$img(src = mole_file, class = "mole-img", alt = "Mole Character")
              ),
              div(class = "score-bubble",
                  div(class = "bubble-label", "Current Comfort"),
                  div(class = "bubble-value-row",
                      span(class = "score-value", style = paste0("color: ", text_color, ";"), score_text),
                      span(class = "score-max", "/ 10")
                  )
              )
          ),
          
          # Bottom Section (Advice Box)
          div(class = "advice-box-container",
              div(class = "advice-title", "Mole's Travel Insights:"), 
              
              # Dynamic Advice Items
              div(class = "advice-item", icon("users"), span(suggestions$crowd)),
              div(class = "advice-item", icon("smile"), span(suggestions$comfort)),
              div(class = "advice-item", icon("cloud"), span(suggestions$weather))
          )
      )
    })
  })
}