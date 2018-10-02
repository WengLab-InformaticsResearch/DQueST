insertButtons <- function(session){
  insertUI(
    selector = "#buttonplaceholder",
    where = "beforeBegin",
    checkboxInput(inputId = "skip",
                  label = "skip this question")
  )
  insertUI(
    selector = "#buttonplaceholder",
    where = "beforeBegin",
    actionButton(
      inputId = "update",
      label = "Update",
      class = "btn-success"
    )
  )
  addTooltip(
    session,
    id = "update",
    title = "Update the above trial list",
    placement = "right",
    trigger = "hover"
  )
  return(NULL)
}


insertRatioButtons <- function(label, choices) {
  insertUI(
    selector = "#questionplaceholder",
    where = "beforeBegin",
    radioButtons(
      inputId = "present_as_radio",
      label = label,
      choices = choices,
      selected = "I don't know"
    )
  )
  return(NULL)
}

insertNumericInput <- function(label) {
  insertUI(
    selector = "#questionplaceholder",
    where = "beforeBegin",
    numericInput(
      inputId = "value_as_numeric",
      label = label,
      value = -1,
      width = "60%"
    )
  )
  insertUI(
    selector = "#questionplaceholder",
    where = "beforeBegin",
    checkboxInput(
      inputId = "value_as_numeric_na",
      label = "I don't know",
      value = FALSE,
      width = "40%"
    )
  )
  return(NULL)
}

insertTimeRangeInput = function(label) {
  insertUI(
    selector = "#questionplaceholder",
    where = "beforeBegin",
    tags$div(
      id = "time_as_date",
      tags$label(label),
      fluidRow(column(
        4,
        dateInput(inputId = "start_as_date", "Start Date:", value = (Sys.Date() - 365))
      ),
      column(
        4,
        dateInput(inputId = "end_as_date", "End Date:", value = Sys.Date())
      ),
      column(4,
             checkboxInput(
               inputId = "time_as_date_na",
               label = "I don't know",
               value = FALSE
             ))), 
      immediate = TRUE
    )
  )
  return(NULL)
}

input2answer = function(input) {
  print(input)
  answer = NULL
  if(input$skip){
    showNotification("You have selected skip the question.")
    answer$value = NULL
    answer$present = NULL
    answer$start = NULL
    answer$end = NULL
    return(answer)
  }
  if (!is.null(input$value_as_numeric)) {
    if (input$value_as_numeric_na) {
      answer$value = NULL
      answer$present = NULL
      answer$start = NULL
      answer$end = NULL
      return(answer)
    } else{
      answer$value = input$value_as_numeric
      answer$present = NULL
      answer$start = NULL
      answer$end = NULL
      return(answer)
    }
  } else{
    if (input$present_as_radio == "I don't know") {
      answer$value = NULL
      answer$present = NULL
      answer$start = NULL
      answer$end = NULL
      return(answer)
    }
    if (input$present_as_radio == "No") {
      answer$value = NULL
      answer$present = FALSE
      answer$start = NULL
      answer$end = NULL
      return(answer)
    }
    if (input$present_as_radio == "Yes") {
      if (input$time_as_date_na) {
        answer$value = NULL
        answer$present = TRUE
        answer$start = NULL
        answer$end = NULL
        return(answer)
      } else{
        if(as.numeric(input$start_as_date - input$end_as_date) >0){
          showNotification("start date should not be later than end date. change both to end date. ")
          answer$value = NULL
          answer$present = TRUE
          answer$start = as.numeric(input$end_as_date - Sys.Date())
          answer$end = as.numeric(input$end_as_date - Sys.Date())
          return(answer)
        }else{
          print(paste0("answer:",answer))
          print(paste0("input$start_as_date:",input$start_as_date))
          answer$value = NULL
          answer$present = TRUE
          answer$start = as.numeric(input$start_as_date - Sys.Date())
          answer$end = as.numeric(input$end_as_date - Sys.Date())
          return(answer)
        } 
      }
    }
  }
}