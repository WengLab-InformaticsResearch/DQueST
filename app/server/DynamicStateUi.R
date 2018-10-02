observe({
  x <- input$country
  if (!is.null(x)) {
    updateSelectizeInput(
      session,
      inputId = "state",
      label = "Select state",
      choices = CACHE$COUNTRY_STATE_TABLE[country %in% input$country, state]
    )
  }
})