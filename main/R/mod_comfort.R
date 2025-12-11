# ==============================================================================
# COMFORT TABLE MODULE
#   mod_comfort_ui
#   mod_comfort_server
# ==============================================================================

# 1. UI COMPONENT
# ------------------------------------------------------------------------------
mod_comfort_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
        .comfort-container {
          width: 100%;
          font-family: 'Arial', sans-serif;
          margin-top: 10px;
          overflow-x: auto;
        }
        
        /* Table Reset */
        .comfort-table {
          width: 100%;
          border-collapse: collapse;
          text-align: center;
          background: white;
        }
        
        /* Header Row (Times) */
        .comfort-header th {
          padding: 10px 5px;
          font-size: 14px;
          font-weight: bold;
          color: #333;
          border-bottom: 2px solid #333;
          min-width: 80px;
        }
        
        /* Row Styling */
        .comfort-row td {
          padding: 15px 5px;
          vertical-align: middle;
        }
        
        /* 1. Comfort Row Specifics */
        .row-comfort {
          font-size: 24px;
          font-weight: bold;
          color: #000; /* Text color stays black for readability */
        }
        
        /* 2. Feels-like Row Specifics */
        .row-temp {
          font-size: 20px;
          font-weight: bold;
          color: #000;
          border-top: 1px dashed #ddd; 
        }
        
        /* 3. Weather Row Specifics */
        .row-weather {
          border-top: 1px dashed #ddd; 
        }
        .weather-icon-img {
          width: 40px;
          height: 40px;
          object-fit: contain;
          display: block;
          margin: 0 auto;
        }
        
        /* Labels Column (First Column) */
        .col-label {
          text-align: left;
          font-size: 14px;
          font-weight: bold;
          color: #000;
          width: 100px;
          padding-left: 10px !important;
        }
      "))
    ),
    
    # The Output Container
    div(class = "comfort-container",
        uiOutput(ns("comfort_table_output"))
    )
  )
}


# 2. SERVER LOGIC
# ------------------------------------------------------------------------------
mod_comfort_server <- function(id, comfort_df) {
  moduleServer(id, function(input, output, session) {
    
    # Helper: Color Gradient Calculator
    get_score_color <- function(score) {
      if (is.na(score)) return("#ffffff")
      
      # Clamp score to 0-10 range for safety, though 8 is our 'max green' reference
      s <- max(0, min(10, score))
      
      # Gradient Logic
      if (s <= 5) {
        # Scale s from [0, 5] to [0, 1]
        ratio <- s / 5
        palette_func <- colorRamp(c("#e32017", "#ffcc00"))
        rgb_vals <- palette_func(ratio)
      } else {
        # Scale s from [5, 8] to [0, 1]
        # If s > 8, ratio becomes > 1, which we clamp
        ratio <- min(1, (s - 5) / (8 - 5))
        palette_func <- colorRamp(c("#ffcc00", "#00b33c"))
        rgb_vals <- palette_func(ratio)
      }
      
      # RGB matrix to Hex string
      return(rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], maxColorValue = 255))
    }
    
    # Render UI Table
    output$comfort_table_output <- renderUI({
      
      df <- comfort_df()
      
      if (is.null(df) || nrow(df) == 0) {
        return(div(style="padding:20px; text-align:center; color:#777;", "Data unavailable for this station."))
      }
      
      # 4-hour window
      df <- head(df, 4)
      
      # HTML Structure
      # 1. Header Row (Time)
      header_cells <- lapply(1:nrow(df), function(i) {
        time_label <- if (i == 1) {
          paste0("Now (", format(Sys.time(), "%H:00"), ")")
        } else {
          future_time <- Sys.time() + (3600 * (i-1))
          format(future_time, "%H:00")
        }
        tags$th(time_label)
      })
      
      # 2. Comfort Row (With Gradient Background)
      comfort_cells <- lapply(df$Comfort_Index, function(score) {
        bg_color <- get_score_color(score)
        tags$td(class = "row-comfort",
                style = paste0("background-color:", bg_color, ";"), # Apply calculated color here
                format(round(score, 1), nsmall = 1))
      })
      
      # 3. Feels-like Row
      temp_cells <- lapply(df$Temp, function(temp) {
        tags$td(class = "row-temp", paste0(round(temp), "°C"))
      })
      
      # 4. Weather Row
      weather_cells <- lapply(df$ConditionIcon, function(base_uri) {
        
        # Check if the base URI is valid
        if (!is.na(base_uri) && base_uri != "") {
          full_icon_url <- paste0(base_uri, ".png")
          tags$td(class = "row-weather", 
                  tags$img(src = full_icon_url, class = "weather-icon-img", alt = "Weather Icon"))
        } else {
          tags$td(class = "row-weather", tags$span("-"))
        }
      })
      
      # Assemble Table
      tags$table(class = "comfort-table",
                 tags$tr(class = "comfort-header",
                         tags$th(class = "col-label", ""), 
                         tagList(header_cells)
                 ),
                 tags$tr(class = "comfort-row",
                         tags$td(class = "col-label", "Comfort"),
                         tagList(comfort_cells)
                 ),
                 tags$tr(class = "comfort-row",
                         tags$td(class = "col-label", "Feels-like"),
                         tagList(temp_cells)
                 ),
                 tags$tr(class = "comfort-row",
                         tags$td(class = "col-label", "Weather"),
                         tagList(weather_cells)
                 )
      )
    })
  })
}