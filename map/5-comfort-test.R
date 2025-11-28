# ==========================================
# 1. DEFINE INPUT DATA
# Use temporary data input
# ==========================================

# Crowd Forecast: 24 data points (15-min intervals for 6 hours)
# Value 0 (Empty) to 1 (Full capacity)
crowd_forecast_15min <- c(
  0.8, 0.85, 0.9, 0.8,  # Hour 1
  0.6, 0.55, 0.5, 0.45, # Hour 2
  0.4, 0.35, 0.3, 0.25, # Hour 3
  0.2, 0.15, 0.1, 0.1,  # Hour 4
  0.1, 0.15, 0.2, 0.25, # Hour 5
  0.4, 0.50, 0.6, 0.7   # Hour 6
)

# Weather Forecast: 6 data points (Hourly)
temp_forecast <- c(15, 16, 19, 21, 24, 23) # Degrees Celsius
weather_condition <- c("raining", "cloudy", "sunny", "sunny", "sunny", "cloudy")

# Static Accessibility Score (0 to 1)
# 1.0 = Step-free train, 0.5 = Step-free platform, 0.0 = No step-free
accessibility_static <- 0.8

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

# We need to average the 15-min crowd data into hourly blocks
# Create a grouping factor: 1,1,1,1, 2,2,2,2, ...
hour_group <- rep(1:6, each = 4)
crowd_hourly_avg <- tapply(crowd_forecast_15min, hour_group, mean)

# Create the main dataframe
df <- data.frame(
  Hour = 1:6,
  Temp = temp_forecast,
  Condition = weather_condition,
  Crowd_Ratio = crowd_hourly_avg,
  Access_Val = accessibility_static
)

# ==========================================
# 3. SCORING FUNCTIONS
# ==========================================

# Function to calculate Thermal Comfort Score (Tc)
calc_thermal_score <- function(temp, weather) {
  # 1. Base calculation: Ideal is 18-22.
  # If inside range, score 10. If outside, deduct distance from range.
  dist_from_ideal <- pmax(0, 18 - temp) + pmax(0, temp - 22)
  score <- 10 - dist_from_ideal
  
  # Clamp score between 0 and 10
  score <- pmax(0, pmin(10, score))
  
  # 2. Weather penalty
  # If raining, divide score by 2 (using grepl to catch 'raining' or 'rain')
  is_raining <- grepl("raining", weather, ignore.case = TRUE)
  score <- ifelse(is_raining, score / 2, score)
  
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
df$Cc <- calc_crowd_score(df$Crowd_Ratio)
df$Ac <- calc_access_score(df$Access_Val)

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
            "| Crowd Level:", round(best_hour$Crowd_Ratio * 100), "%"))