library(shiny)

# 1. Define Tube Colors (CSS Hex Codes)
tube_colors <- c(
  "Victoria" = "#0098D4",
  "District" = "#00782A",
  "Central" = "#E32017",
  "Piccadilly" = "#003688",
  "Northern" = "#000000",
  "Jubilee" = "#A0A5A9",
  "Walking" = "#dotted" 
)

# 2. Mock Data: What your API/Logic returns
journey_data <- data.frame(
  Leg = 1:3,
  StartStation = c("South Kensington", "Victoria", "Oxford Circus"),
  EndStation = c("Victoria", "Oxford Circus", "St. Paul's"),
  Line = c("District", "Victoria", "Central"),
  Duration = c(4, 3, 6),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  # Add custom CSS in the head
  tags$head(
    tags$style(HTML("
      /* The container for the whole journey */
      .journey-container {
        max-width: 400px;
        margin: 20px;
        font-family: 'Arial', sans-serif;
      }
      
      /* A single step in the journey */
      .journey-step {
        display: flex;
        min-height: 80px; /* Space for the line */
      }
      
      /* The left column with the graphic */
      .graphic-col {
        display: flex;
        flex-direction: column;
        align-items: center;
        margin-right: 15px;
        min-width: 30px;
      }
      
      /* The station circle */
      .station-dot {
        width: 20px;
        height: 20px;
        border-radius: 50%;
        background: white;
        border: 4px solid #333; /* Default color */
        z-index: 2;
      }
      
      /* The connecting line */
      .connector-line {
        width: 6px;
        flex-grow: 1; /* Fills the vertical space */
        background-color: #ccc; /* Default color */
        margin-top: -2px; /* Overlap slightly */
        margin-bottom: -2px;
        z-index: 1;
      }
      
      /* The text info */
      .info-col {
        padding-top: 0px;
      }
      
      .station-name { font-weight: bold; font-size: 16px; }
      .line-info { font-size: 14px; color: #666; margin-top: 4px;}
      .duration-badge { 
        background: #eee; 
        padding: 2px 6px; 
        border-radius: 4px; 
        font-size: 12px; 
      }
    "))
  ),
  
  h3("Your Journey"),
  uiOutput("journey_ui") # <--- This is where we inject our HTML
)

server <- function(input, output, session) {
  
  output$journey_ui <- renderUI({
    
    # We loop through each leg of the journey
    # lapply is like .map() in JavaScript/React
    steps_html <- lapply(1:nrow(journey_data), function(i) {
      
      row <- journey_data[i, ]
      color <- tube_colors[[row$Line]]
      
      # Create the HTML for this leg
      tags$div(class = "journey-step",
               
               # 1. Graphic Column (Dot + Line)
               tags$div(class = "graphic-col",
                        # The Dot (Start Station)
                        tags$div(class = "station-dot", 
                                 style = paste0("border-color: ", color, ";")),
                        # The Line
                        tags$div(class = "connector-line", 
                                 style = paste0("background-color: ", color, ";"))
               ),
               
               # 2. Info Column (Text)
               tags$div(class = "info-col",
                        div(class = "station-name", row$StartStation),
                        div(class = "line-info", 
                            paste(row$Line, "Line"),
                            span(class = "duration-badge", paste(row$Duration, "mins"))
                        )
               )
      )
    })
    
    # Add the Final Destination (Just a dot, no line after it)
    last_leg <- journey_data[nrow(journey_data), ]
    final_color <- tube_colors[[last_leg$Line]]
    
    final_step <- tags$div(class = "journey-step",
                           tags$div(class = "graphic-col",
                                    tags$div(class = "station-dot", 
                                             style = paste0("border-color: ", final_color, ";"))
                                    # No connector line here
                           ),
                           tags$div(class = "info-col",
                                    div(class = "station-name", last_leg$EndStation),
                                    div(class = "line-info", "Arrive")
                           )
    )
    
    # Return the list of all HTML elements wrapped in a container
    tagList(
      div(class = "journey-container", steps_html, final_step)
    )
  })
}

shinyApp(ui, server)