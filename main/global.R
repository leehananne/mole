# ==============================================================================
# global.R
# Contains global static data and initial setup
# ==============================================================================

# Library
library(shiny)
library(bslib)
library(plotly)
library(httr)
library(jsonlite)
library(ggplot2)
library(purrr)
library(dplyr)
library(lubridate)
library(DT)

library(htmltools)
library(scales)

# Load Master Data 
station_master_data <- read.csv("data/data_master.csv", stringsAsFactors = FALSE)
station_toilet_data <- read.csv("data/Toilets.csv", stringsAsFactors = FALSE)

# TfL Line Colors
tube_colors <- c(
  "victoria" = "#039be5",
  "district" = "#007d32",
  "central" = "#dc241f",
  "piccadilly" = "#0019a8",
  "northern" = "#000000",
  "jubilee" = "#838d93",
  "bakerloo" = "#b26300",
  "metropolitan" = "#9b0058",
  "circle" = "#ffc80a",
  "hammersmith-city" = "#f589a6",
  "waterloo-city" = "#76d0bd",
  "elizabeth" = "#60399e",
  "dlr" = "#00afad",
  "tram" = "#5fb526",
  
  "overground" = "#fa7b05",
  "liberty" = "#5d6061",
  "lioness" = "#faa61a",
  "mildmay" = "#0077ad",
  "suffragette" = "#5bbd72",
  "weaver" = "#823a62",
  "windrush" = "#ed1b00"
)

# User Profiles for Comfort Weighting
user_profiles <- list(
  "standard" = list(
    label = "Daily Commuter",
    icon  = "briefcase",
    desc  = "Balanced mix of crowd, weather, and accessibility.",
    weights = c(w_weather = 0.3, w_crowd = 0.6, w_access = 0.1)
  ),
  "accessibility" = list(
    label = "Accessibility Focused",
    icon  = "wheelchair",
    desc  = "Prioritizes step-free access and low crowding.",
    weights = c(w_weather = 0.1, w_crowd = 0.3, w_access = 0.6)
  ),
  "thermal" = list(
    label = "Weather Sensitive",
    icon  = "umbrella",
    desc  = "Prioritizes avoiding rain and temperature extremes.",
    weights = c(w_weather = 0.6, w_crowd = 0.3, w_access = 0.1)
  )
)

# Shiny input: Mapping Naptan and Station Name 
station_choices <- setNames(station_master_data$NaptanCode, station_master_data$StationName)

default_station_name <- "South Kensington Underground Station"
default_origin_name <- "South Kensington Underground Station"
default_destination_name <- "St. Paul's Underground Station"

default_naptan_code <- station_choices[default_station_name]
default_origin_naptan_code <- station_choices[default_origin_name]
default_destination_naptan_code <- station_choices[default_destination_name]

# API Configurations
TFL_API <- ""
GMAP_API <- Sys.getenv("GMAP_API_KEY")
MAP_ID <- Sys.getenv("MAP_ID_KEY")

if (GMAP_API == ""||MAP_ID == "") {
  stop("Security Alert: API keys are missing. Please check your .Renviron file.")
}