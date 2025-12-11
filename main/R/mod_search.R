# ==============================================================================
# Station Search Field with Autofill Dropdown
# ==============================================================================
# Arguments:
#   id: The inputID for the shiny element
#   choices_map: A character vector of station names 
#   place_holder: A placeholder text for the entry field
#   label_text: A label placed on top of the entry field 
#   default_selection: The default station naptan

station_search_ui <- function(id, choices_map, place_holder, label_text, default = NULL) {
  
  selected_val <- if (!is.null(default)) default else ""
  
  # Only use the JS clearer if NO default is provided
  js_init <- if (is.null(default)) {
    I('function() { this.setValue(""); }')
  } else {
    NULL
  }
  
  selectizeInput(
    inputId = id,
    label = label_text,
    choices = c("Select a station" = "", choices_map),
    selected = selected_val,
    multiple = FALSE,
    width = "100%",
    options = list(
      placeholder = place_holder,
      onInitialize = js_init
    )
  )
}