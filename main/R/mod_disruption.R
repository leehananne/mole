# ==============================================================================
# DISRUPTION MODULE
# Displays service disruptions (delays) in a red alert box
# ==============================================================================

# 1. UI COMPONENT
# ------------------------------------------------------------------------------
mod_disruption_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # --- Custom CSS for Red Box ---
    tags$head(
      tags$style(HTML("
        /* Red Disruption Box */
        .disruption-box-container {
          background-color: #fcf2f2; /* Very Light Red */
          border-left: 5px solid #e32017; 
          border-radius: 4px;
          padding: 15px;
          margin-bottom: 15px; /* Spacing below the box */
        }

        .disruption-title {
          font-weight: bold; 
          font-size: 16px; 
          margin-bottom: 10px; 
          color: #b71c1c; /* Darker Red for Title text */
        }
        
        .disruption-item {
          display: flex; 
          align-items: flex-start; 
          margin-bottom: 8px; 
          font-size: 14px; 
          color: #2c3e50; /* Dark grey text for readability */
          line-height: 1.4;
        }
        
        .disruption-item:last-child {
          margin-bottom: 0;
        }
      "))
    ),
    
    # The Output Container
    uiOutput(ns("disruption_output"))
  )
}

# 2. SERVER COMPONENT
# ------------------------------------------------------------------------------
mod_disruption_server <- function(id, delay_update, access_update) {
  moduleServer(id, function(input, output, session) {
    
    output$disruption_output <- renderUI({
      
      # Get Data (Expects dataframe with column 'message')
      d_delay  <- delay_update()
      d_access <- access_update()
      
      messages <- c()
      
      # Extract messages from Delay Data
      if (!is.null(d_delay) && nrow(d_delay) > 0) {
        # Assuming column name is 'message'
        messages <- c(messages, d_delay$message)
      }
      
      # Extract messages from Access Data
      if (!is.null(d_access) && nrow(d_access) > 0) {
        messages <- c(messages, d_access$message)
      }
      
      # Logic: HIDE completely if no messages collected
      if (length(messages) == 0) {
        return(NULL)
      }
      
      # Create List of Messages
      # Loop through the 'message' column
      message_list <- lapply(messages, function(msg) {
        div(class = "disruption-item", msg)
      })
      
      # Render the Red Box
      div(class = "disruption-box-container",
          div(class = "disruption-title", "Service Updates:"),
          tagList(message_list)
      )
    })
  })
}