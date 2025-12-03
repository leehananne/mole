library(shiny)
library(DT)
library(dplyr)

source("./comfort_index.R")

# ========================================================
# 2. SHINY UI
# ========================================================

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
  
  # Custom CSS to improve table spacing and alignment
  tags$head(
    tags$style(HTML("
      .table > tbody > tr > td {
        vertical-align: middle;
        font-size: 16px;
      }
      .metric-label {
        font-weight: bold;
        color: #555;
        text-align: right; 
        background-color: #f8f9fa;
      }
    "))
  ),
  
  titlePanel("Station Comfort Forecast"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Station Status"),
      p("Real-time forecast for South Kensington."),
      p("This dashboard helps you decide the best time to travel based on comfort, crowding, and weather conditions."),
      hr(),
      helpText("Comfort Index ranges from 0 (Bad) to 10 (Excellent)."),
      width = 3
    ),
    
    mainPanel(
      h3("Forecast Summary"),
      # The Output element for the transposed table
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
        Crowding = paste0(round(Crowd_Ratio.CrowdingScore * 100, 0), "%"),
        
        # 4. Format Temperature: Add unit
        Temperature = paste0(Temp, "°C"),
        
        # 5. Format Icon: Create HTML <img> tag
        # We set height to 30px to fit the table row
        # Weather = paste0('<img src="', ConditionIcon, '" height="35"></img>')
      ) %>%
      # Select and Reorder columns for the final table
      select(Time, `Comfort Index`, Crowding, Temperature)
    
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