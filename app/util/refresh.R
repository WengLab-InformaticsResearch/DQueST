refreshAll = function(session){
  # javascript used to reset the input value to avoid the input value 
  # could not be destroyed even after ui removed.
  session$sendCustomMessage(type = "resetValue", message = "header")
  session$sendCustomMessage(type = "resetValue", message = "time")
  session$sendCustomMessage(type = "resetValue", message = "status")
  session$sendCustomMessage(type = "resetValue", message = "value")
  session$sendCustomMessage(type = "resetValue", message = "time_unit")
}

refreshQA <- function(session){
  # refresh the placeholder if restart is clicked.
  # used only for shinny server.
  # session: shinny session.
  # cat("Refresh PlaceHolder\n")
  removeUI(selector = "div#uiInput",immediate = TRUE)
  insertUI(selector = "#start",where = "beforeBegin",
           ui = tags$div(id = "uiInput",tags$div(id = "placeholder")),
           immediate = TRUE)
}