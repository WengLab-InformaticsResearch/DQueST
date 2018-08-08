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
  session$sendCustomMessage(type = "resetValue", message = "value_qa")
  session$sendCustomMessage(type = "resetValue", message = "time_qa")
  session$sendCustomMessage(type = "resetValue", message = "time_unit_qa")
  session$sendCustomMessage(type = "resetValue", message = "time_qa2")
  session$sendCustomMessage(type = "resetValue", message = "time_unit_qa2")
  session$sendCustomMessage(type = "resetValue", message = "radio_qa")
  session$sendCustomMessage(type = "resetValue", message = "checkbox_qa")
  session$sendCustomMessage(type = "resetValue", message = "time")

  

  removeUI(selector = "div#uiInput1",immediate = TRUE)
  removeUI(selector = "div#uiInput3",immediate = TRUE)
  insertUI(selector = "div#uiInput2",where = "beforeBegin",
           ui = tags$div(id = "uiInput1",tags$div(id = "placeholder1")),
           immediate = TRUE)
  insertUI(selector = "div#uiInput2",where = "beforeBegin",
           ui = tags$div(id = "uiInput3",tags$div(id = "placeholder3")),
           immediate = TRUE)
}