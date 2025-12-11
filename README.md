# 💗 MOLE

**MOLE** helps commuters plan their journey with a focus on comfort, using data analytics to inform their best course of action. Unlike standard routing apps, MOLE is designed for those who value a calmer travel experience.

### **Target Audience:**

- Daily commuters looking for quieter routes.
- Commuters with accessibility needs.
- Tourists lacking local knowledge of crowding or station facilities.

# 🌟 Key Features

- **Comfort-First Routing:** Prioritizes lower crowding levels over pure speed.
- **Live Crowding Feed:** Real-time data visualization of station traffic.
- **Station Amenities:** View accessibility features (lifts, ramps) and facility details.
- **Customizable Preferences:** Filter by journey mode and specific accessibility requirements.

# 👩🏻‍💻 Tech Stack

- **Language:** R
- **Framework:** Shiny (Dashboard)
- **Data Sources:**
    - **TfL API:** Live transport data, station facilities, and journey routing.
    - **Google Maps API:** Geolocation and map display.
    - **Google Weather API:** Real-time weather integration.

# ⚒️ Installation & Usage

**Prerequisites**

You will need R and RStudio installed. 

1. **Clone the repo** 
    
    ```
    git clone https://github.com/leehananne/mole.git
    ```
    
2. **Install Dependencies** 
    
    In your R terminal,
    
    ```
    install.packages(c("ggplot2", "httr", "jsonlite", "scales", "plotly", "bslib", "purrr", "dplyr", "lubridate", "DT"))
    ```
    
3. **Run the App**
    
    Open `app.R` in RStudio and click “Run App”
