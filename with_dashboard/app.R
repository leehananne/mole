# Main Shiny Application File
# This file sources all necessary components and runs the application

# Source global configuration and data loading
source("global.R")

# Source UI
source("ui.R")

# Source Server
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)

