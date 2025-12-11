# ==============================================================================
# PROFILE STATUS - BADGE & MODAL
# Displays the current user persona (Icon + Label)
# ==============================================================================

# 1A. MODAL - UI GENERATOR FUNCTION
# ------------------------------------------------------------------------------
get_traveler_modal_content <- function(ns, current_selection, profiles) {
  
  modalDialog(
    title = NULL, 
    size = "m",
    # Footer with high Z-Index
    footer = div(class = "modal-footer",
                 actionButton(ns("confirm_profile"), "Start Dashboard", 
                              class = "btn-start-dashboard")
    ),
    easyClose = FALSE, 
    fade = TRUE,
    
    # Styles in global style.css
    # Specific radio options were extracted
    
    tags$style(HTML("
      .option-card {
        display: flex;
        align-items: center;
      }
        
        .shiny-input-container:not(.shiny-input-container-inline) {
        width: 100% !important;
      }
        
      /* 3. PROFILE CARDS (Standard Radio Implementation) */
      .profile-selector { width: 100%; padding: 10px 30px; }
        
      /* The Text/Icon Container inside the label */
      .profile-card-content {
        display: flex;
        align-items: center;
        width: 100% !important; 
        margin-left: 10px; /* Space from the radio circle */
      }

      /* Container for each radio option */
      .shiny-options-group .radio {
        margin-bottom: 15px !important;
        margin-top: 0 !important;
      }
    
      /* Style the Label to look like a Card */
      .shiny-options-group label {
        display: flex !important;
        align-items: center;
        width: 100%;
        background-color: #f8f9fa;
        border: 2px solid #eaecf0;
        border-radius: 16px;
        padding: 15px 20px;
        cursor: pointer;
        transition: all 0.2s ease;
      }
    
      /* Hover Effect */
      .shiny-options-group label:hover {
        background-color: #fff;
          border-color: #ccc;
          box-shadow: 0 0 6px rgba(0,0,0,0.06);
      }
    ")),
    
    # --- CONTENT ---
    
    # 1. Header
    div(class = "modal-header-custom",
        img(src = "tube-mole.png", class = "header-logo", alt = "Mole Logo"),
        div(class = "header-title", "MOLE"),
        div(class = "header-subtitle", "Underground Comfort Assistant")
    ),
    
    # 2. Body
    div(class = "profile-selector",
        div(class = "option-card",
        radioButtons(
          inputId = ns("selected_profile"),
          label = NULL, 
          selected = current_selection,
          choiceValues = names(profiles),
          choiceNames = lapply(names(profiles), function(key) {
            info <- profiles[[key]]
            
            # Simple HTML structure inside the label
            HTML(paste0(
              "<div class='profile-card-content'>",
              "<i class='fa fa-", info$icon, " card-icon'></i>",
              "<div class='text-group'>",
              "<span class='text-content card-title'>", info$label, "</span>",
              "<span class='text-content card-desc'>", info$desc, "</span>",
              "</div>",
              "</div>"
            ))
          })
        ))
    )
  )
}


# 1B. MODAL - SERVER COMPONENT
# ------------------------------------------------------------------------------
mod_traveler_modal_server <- function(id, user_profiles, trigger_open = NULL) {
  moduleServer(id, function(input, output, session) {
    
    current_weights     <- reactiveVal(user_profiles$standard$weights)
    current_profile_key <- reactiveVal("standard")
    
    # Function to display modal
    open_modal <- function() {
      showModal(get_traveler_modal_content(session$ns, current_profile_key(), user_profiles))
    }
    
    # 1. Open on Startup (Once)
    observeEvent(session, {
      open_modal()
    }, once = TRUE)
    
    # 2. Open on Trigger (Bridge from Badge)
    if (!is.null(trigger_open)) {
      observeEvent(trigger_open(), {
        open_modal()
      }, ignoreInit = TRUE)
    }
    
    # 3. Handle 'Start Dashboard' Click
    observeEvent(input$confirm_profile, {
      req(input$selected_profile) # Safety check to ensure selection exists
      
      key <- input$selected_profile
      
      # Update reactives
      current_weights(user_profiles[[key]]$weights)
      current_profile_key(key)
      
      removeModal()
    }, ignoreInit = TRUE) 
    
    # Return data
    list(weights = current_weights, key = current_profile_key)
  })
}



# 2A. BADGE - UI COMPONENT
# ------------------------------------------------------------------------------
mod_profile_badge_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML("
      .badge-link { text-decoration: none !important; cursor: pointer; }
      .profile-badge { display: inline-flex; align-items: center; background: transparent; padding: 0; color: #888682; margin-bottom: 12px; transition: all 0.3s ease; }
      .profile-badge i { margin-right: 6px; color: #888682; font-size: 14px; }
      .profile-badge-label { font-weight: 600; text-transform: capitalize; font-size: 14px; }
      .profile-badge:hover, .profile-badge:hover i { color: #555; transform: translateY(-0.5px); }
    "))),
    actionLink(ns("badge_click"), class = "badge-link", uiOutput(ns("badge_output")))
  )
}


# 2B. BADGE - SERVER COMPONENT
# ------------------------------------------------------------------------------
mod_profile_badge_server <- function(id, current_profile_key) {
  moduleServer(id, function(input, output, session) {
    output$badge_output <- renderUI({
      key <- current_profile_key()
      if (is.null(key) || !key %in% names(user_profiles)) key <- "standard"
      info <- user_profiles[[key]]
      div(class = "profile-badge", icon(info$icon), span(class = "profile-badge-label", info$label))
    })
    return(reactive(input$badge_click))
  })
}