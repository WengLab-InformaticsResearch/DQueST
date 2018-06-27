refreshAll = function(session){
  # javascript used to reset the input value to avoid the input value 
  # could not be destroyed even after ui removed.
  session$reload()
}

refreshQA <- function(session){
  # refresh the placeholder if restart is clicked.
  # used only for shinny server.
  # session: shinny session.
  # cat("Refresh PlaceHolder\n")
  session$sendCustomMessage(type = "resetValue", message = "keyword")
  session$sendCustomMessage(type = "resetValue", message = "age")
  session$sendCustomMessage(type = "resetValue", message = "gender")
  session$sendCustomMessage(type = "resetValue", message = "value")
  session$sendCustomMessage(type = "resetValue", message = "time")
  session$sendCustomMessage(type = "resetValue", message = "time_unit")
  session$sendCustomMessage(type = "resetValue", message = "radio")
  session$sendCustomMessage(type = "resetValue", message = "status")

  removeUI(selector = "div#uiInput1",immediate = TRUE)
  insertUI(selector = "div#uiInput2",where = "beforeBegin",
           ui = tags$div(id = "uiInput1",tags$div(id = "placeholder1")),
           immediate = TRUE)
}