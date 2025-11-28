# ERROR PERSISTS!

library(shiny)
library(DT)
library(dplyr)


# ========================================================
# 1. SETUP / MOCK DATA (Replace this with your actual loading logic)
# ========================================================

source("components/comfort_index.R")

# ========================================================
# 2. SHINY UI
# ========================================================

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5), # Optional: Modern Bootstrap 5 theme
  
  titlePanel("Station Comfort Forecast"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Station Status"),
      p("Forecast for the next 6 hours based on crowding, weather, and accessibility."),
      width = 3
    ),
    
    mainPanel(
      h3("Hourly Comfort Details"),
      # The Output element for the table
      DTOutput("comfort_table"),
      width = 9
    )
  )
)

# ========================================================
# 3. SHINY SERVER
# ========================================================

server <- function(input, output) {
  
  output$comfort_table <- renderDT({
    
    # Process the dataframe for display
    display_df <- df %>%
      mutate(
        # 1. Format Hour: Add ":00" for readability
        Time = paste0(sprintf("%02d", Hour), ":00"),
        
        # 2. Format Comfort Index: Color coding logic could go here (optional CSS)
        `Comfort Index` = round(Comfort_Index, 1),
        
        # 3. Format Crowding: Convert ratio (0.1) to percentage (10%)
        Crowding = paste0(round(Crowd_Ratio * 100, 0), "%"),
        
        # 4. Format Temperature: Add unit
        Temperature = paste0(Temp, "°C"),
        
        # 5. Format Icon: Create HTML <img> tag
        # We set height to 30px to fit the table row
        # Weather = paste0('<img src="', ConditionIcon, '" height="35"></img>')
      ) %>%
      # Select and Reorder columns for the final table
      select(Time, `Comfort Index`, Crowding, Temperature, Weather)
    
    # Render the DataTable
    datatable(
      display_df, 
      escape = FALSE, # CRITICAL: Allows the <img> tag to render as HTML
      options = list(
        dom = 't',        # Shows only the table (hides search/pagination)
        paging = FALSE,   # Disable pagination (show all 6 hours)
        ordering = FALSE, # Disable sorting arrows
        columnDefs = list(
          list(className = 'dt-center', targets = "_all") # Center align text
        )
      ),
      rownames = FALSE # Hide row numbers
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)