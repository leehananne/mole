# ==========================================
# 1. DEFINE INPUT DATA
# Use temporary data input
# ==========================================

# Crowd Forecast: 24 data points (15-min intervals for 6 hours)
# Value 0 (Empty) to 1 (Full capacity)

source("components/station_crowd_forecast.R")
source("components/weather_forecast.R")
station_data <- read.csv("data/data_master.csv")

api_key <- "AIzaSyCkp9eNSjWSoLJ_s0NX61yg21lcwCAaD8Q"
n_scope <- 6

station_data <- read.csv("data/data_master.csv")
station_name <- "South Kensington Underground Station"

station_info <- station_data %>%
  filter(StationName == station_name) %>%
  select(NaptanCode, Latitude, Longitude)

crowd_hourly_avg <- get_crowd_forecast(station_info$NaptanCode, n_hours = n_scope)
weather_hourly <- get_weather_forecast(api_key, station_info$Latitude, station_info$Longitude, n_forecast = n_scope-1)

temp_forecast <- weather_hourly$Temp
weather_condition <- weather_hourly$PrecipProb

station_acc_data <- station_data %>%
  filter(StationName == station_name) %>%
  select(Lift, SameLevel, Interchange) %>%
  mutate(
    # Ensure Lift is numeric just in case it was loaded as character
    Lift = as.numeric(Lift),
    
    AccessScore = case_when(
      # Condition 1: Step-free access exists (via Lift or Same Level)
      Lift > 0 | SameLevel == TRUE ~ 10,
      
      # Condition 2: No step-free to street, but full interchange available
      Lift == 0 & Interchange == 2 ~ 6,
      
      # Condition 3: No step-free to street, partial interchange available
      Lift == 0 & Interchange == 1 ~ 3,
      
      # Default: No accessibility features
      TRUE ~ 0
    )
  )

acc_score <- station_acc_data$AccessScore

# Weights Configuration (Must sum to 1)
# Change this variable to 'standard' or 'access_focused' to test scenarios
# This would be an input at the beginning
user_profile <- "standard" 

if (user_profile == "standard") {
  w_t <- 0.4 # Thermal weight
  w_c <- 0.5 # Crowd weight
  w_a <- 0.1 # Accessibility weight
} else if (user_profile == "access_focused") {
  # Accessibility focused
  w_t <- 0.1
  w_c <- 0.1
  w_a <- 0.8
}

# ==========================================
# 2. DATA PROCESSING & ALIGNMENT
# ==========================================

# Create the main dataframe
df <- data.frame(
  Hour = 1:6,
  Temp = temp_forecast,
  Condition = weather_condition,
  Crowd_Ratio = crowd_hourly_avg,
  Access_Val = acc_score
)

# ==========================================
# 3. SCORING FUNCTIONS
# ==========================================

# Function to calculate Thermal Comfort Score (Tc)
calc_thermal_score <- function(temp, precip_prob) {
  # 1. Base calculation: Ideal is 18-22.
  # If inside range, score 10. If outside, deduct distance from range.
  dist_from_ideal <- pmax(0, 14 - temp) + pmax(0, temp - 22)
  score <- 10 - dist_from_ideal
  
  # Clamp score between 0 and 10
  score <- pmax(0, pmin(10, score))
  
  # 2. Weather penalty
  precip_prob <- ifelse(is.na(precip_prob), 0, precip_prob)
  penalty_multiplier <- 1 - (0.5 * (precip_prob / 100))
  
  # Apply penalty
  final_score <- score * penalty_multiplier
  
  return(score)
}

# Function to calculate Crowding Comfort Score (Cc)
# Input is crowding (bad), we want comfort (good).
calc_crowd_score <- function(crowd_ratio) {
  # Invert the ratio: 0.1 crowd -> 0.9 comfort -> 9.0 score
  score <- (1 - crowd_ratio) * 10
  return(score)
}

# Function to calculate Accessibility Score (Ac)
calc_access_score <- function(access_val) {
  return(access_val * 10)
}


# ==========================================
# 4. CALCULATE INDICES
# ==========================================

df$Tc <- calc_thermal_score(df$Temp, df$Condition)
df$Cc <- calc_crowd_score(df$Crowd_Ratio.CrowdingScore)
df$Ac <- acc_score

# Calculate Final Weighted CI
df$Comfort_Index <- (w_t * df$Tc) + (w_c * df$Cc) + (w_a * df$Ac)

# Round for display
df$Comfort_Index <- round(df$Comfort_Index, 2)

# ==========================================
# 5. DISPLAY RESULTS
# ==========================================

print(paste("User Profile:", user_profile))
print(paste("Weights -> T:", w_t, " C:", w_c, " A:", w_a))
print("---------------------------------------------------")
print(df)

# Optional: Simple recommendation
best_hour <- df[which.max(df$Comfort_Index), ]
print("---------------------------------------------------")
print(paste("Recommendation: The most comfortable time to travel is Hour", best_hour$Hour))
print(paste("Reasoning: Temp", best_hour$Temp, "C,", best_hour$Condition, 
            "| Crowd Level:", round(best_hour$Crowd_Ratio.CrowdingScore * 100), "%"))