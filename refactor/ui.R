ui <- fluidPage(
  title = "London Pulse",
  tags$head(tags$style(HTML(" #weatherStatement { white-space: pre-wrap; word-break: break-word; } "))),
  fluidRow(
    column(width = 12, h2("London Station Pulse 🚇☀️"), hr())
  ),
  fluidRow(
    column(width = 4,
           wellPanel(
             h4("Controls"),
             selectInput("selected_station_naptan", "Select Station:",
                         choices = station_choices,
                         selected = default_naptan_code),
             checkboxGroupInput("tfl_days", "Select Day(s) for Crowding:",
                                choices = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                                selected = "Mon", inline = TRUE)
           ),
           wellPanel(
             h4("Journey Planner"),
             selectInput("origin_station", "Origin Station:",
                         choices = station_choices,
                         selected = default_naptan_code),
             selectInput("destination_station", "Destination Station:",
                         choices = station_choices,
                         selected = default_destination_naptan_code),
             actionButton("plan_journey", "Plan Journey", class = "btn-primary")
           )
    ),
    column(width = 8,
           tags$div(class = "panel panel-info",
                    tags$div(class = "panel-heading",
                             tags$h3(class = "panel-title", "Predicted Crowding Levels (% Baseline)")
                    ),
                    tags$div(class = "panel-body",
                             plotlyOutput("tflCrowdingPlot", height = "450px")
                    )
           ),
           tags$div(class = "panel panel-primary", style = "margin-top: 20px;",
                    tags$div(class = "panel-heading",
                             tags$h3(class = "panel-title", textOutput("weatherTitle"))
                    ),
                    tags$div(class = "panel-body",
                             verbatimTextOutput("weatherStatement")
                    )
           ),
           tags$div(class = "panel panel-success", style = "margin-top: 20px;",
                    tags$div(class = "panel-heading",
                             tags$h3(class = "panel-title", "Journey Route")
                    ),
                    tags$div(class = "panel-body",
                             verbatimTextOutput("journeyRouteOutput")
                    )
           )
    )
  )
)


