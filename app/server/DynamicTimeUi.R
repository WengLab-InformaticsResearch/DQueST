observe({
  x <- input$present_as_radio
  if (!is.null(x)) {
    if (x == "Yes") {
      shinyjs::show("time_as_date")
    } else{
      shinyjs::hide("time_as_date")
    }
  } else{
    shinyjs::hide("time_as_date")
  }
})