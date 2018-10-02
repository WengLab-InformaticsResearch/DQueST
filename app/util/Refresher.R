refreshAll = function(session) {
  # javascript used to reset the input value to avoid the input value
  # could not be destroyed even after ui removed.
  session$reload()
}

refreshQA <- function(session) {
  # refresh the placeholder if restart is clicked.
  # used only for shinny server.
  # session: shinny session.
  # cat("Refresh PlaceHolder\n")
  session$sendCustomMessage(type = "resetValue", message = "value_as_numeric")
  session$sendCustomMessage(type = "resetValue", message = "start_as_date")
  session$sendCustomMessage(type = "resetValue", message = "end_as_date")
  session$sendCustomMessage(type = "resetValue", message = "present_as_radio")
  session$sendCustomMessage(type = "resetValue", message = "value_as_numeric_na")
  session$sendCustomMessage(type = "resetValue", message = "time_as_date_na")
  
  
  removeUI(selector = "div#questionbox",immediate = TRUE)
  removeUI(selector = "div#questionbuttonbox",immediate = TRUE)
  
  insertUI(
    selector = "div#question_box_anchor",
    where = "beforeBegin",
    ui = tags$div(id = "questionbox", tags$div(id = "questionplaceholder"),immediate = TRUE)
  )
  
  insertUI(
    selector = "div#question_box_anchor",
    where = "beforeBegin",
    ui = tags$div(id = "questionbuttonbox", tags$div(id = "buttonplaceholder"),immediate = TRUE)
  )
  
}