tabPanel(
  "Search",
  value = "search",
  h4("You can search by condition, demographics and location"),
  wellPanel(
    ### search by condition keyword ###
    selectizeInput(
      "condition",
      label = "Enter condition to search",
      choices = CACHE$CONDITION_KEY_WORD_TOP100,
      multiple = TRUE,
      selected = NULL
    ),
    ### end condition search ###
    
    ### search by age ###
    numericInput(
      inputId = "age",
      label = "Enter Age to search",
      value = -1,
      min = 0,
      max = 100
    ),
    ### end ###
    
    ### search by gender ###
    radioButtons(
      inputId = "gender",
      label = "Enter gender to search",
      selected = "All",
      inline = TRUE,
      choices = c("All", "Male", "Female")
    ),
    ### end ###
    
    ### search by healthy_volunteers
    checkboxInput(
      inputId = "ctrl",
      label = "Looking for healthy volunteers",
      value = FALSE
    ),
    ### end ###
    
    ### search by country ###
    selectizeInput(
      inputId = 'country',
      label = 'Select country',
      choices = unique(CACHE$COUNTRY_STATE_TABLE$country),
      multiple = TRUE
    ),
    ### end ###
    
    ### search by state ###
    selectizeInput(
      inputId = 'state',
      label = 'Select state',
      choices = NULL,
      multiple = TRUE
    )
    ### end ###
  ),
  
  # Wrap the button in the function `withBusyIndicatorUI()`
  withBusyIndicatorUI(
    actionButton(
      inputId = "search",
      label = "Search",
      class = "btn-primary"
    )
  )
)