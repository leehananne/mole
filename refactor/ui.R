ui <- fluidPage(
  title = "London Pulse",
  tags$head(tags$style(HTML(" 
    #weatherStatement { white-space: pre-wrap; word-break: break-word; } 
    #crowdingForecast { white-space: pre-wrap; word-break: break-word; font-size: 14px; padding: 10px; }
  "))),
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
             radioButtons("tfl_days", "Select Day for Crowding:",
                          choices = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                          selected = if(exists("default_tfl_day")) default_tfl_day else format(Sys.time(), "%a"), inline = TRUE)
           ),
           wellPanel(
             h4("Journey Planner"),
             selectInput("origin_station", "Origin Station:",
                         choices = station_choices,
                         selected = default_naptan_code),
             selectInput("destination_station", "Destination Station:",
                         choices = station_choices,
                         selected = default_destination_naptan_code),
             hr(),
             h5("Journey Preference:"),
             radioButtons("journey_preference", NULL,
                         choices = list("Least Interchange" = "LeastInterchange",
                                       "Least Time" = "LeastTime",
                                       "Least Walking" = "LeastWalking"),
                         selected = "LeastTime",
                         inline = FALSE),
             hr(),
             h5("Accessibility Preference:"),
             radioButtons("accessibility_preference", NULL,
                         choices = list("No Requirements" = "NoRequirements",
                                       "No Solid Stairs" = "NoSolidStairs",
                                       "No Escalators" = "NoEscalators",
                                       "No Elevators" = "NoElevators",
                                       "Step Free to Vehicle" = "StepFreeToVehicle",
                                       "Step Free to Platform" = "StepFreeToPlatform"),
                         selected = "NoRequirements",
                         inline = FALSE),
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
           tags$div(class = "panel panel-warning", style = "margin-top: 20px;",
                    tags$div(class = "panel-heading",
                             tags$h3(class = "panel-title", "Crowding Forecast")
                    ),
                    tags$div(class = "panel-body",
                             textOutput("crowdingForecast")
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


