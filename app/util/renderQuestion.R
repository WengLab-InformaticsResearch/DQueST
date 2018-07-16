renderQuestion = function(question,session){
  domain = question$domain
  renderQuestionDomain(question,domain)
  insertUI(
    selector = "#placeholder1",
    where = "beforeBegin",
    checkboxInput(inputId = "skip",
                  label = "skip this question"),
    immediate = TRUE
  )
  insertUI(
    selector = "#placeholder1",
    where = "beforeBegin",
    actionButton(
      inputId = "update",
      label = "Update",
      class = "btn-success"
    ),
    immediate = TRUE
  )
  addTooltip(
    session,
    id = "submit",
    title = "Update the above trial list",
    placement = "right",
    trigger = "hover"
  )
  return(NULL)
}

renderQuestionDomain = function(question, domain) {
  switch(
    domain,
    "demographic" = renderQuestionDemo(question),
    "condition" = renderQuestionCondition(question),
    "observation" = renderQuestionObservation(question),
    "measurement" = renderQuestionMeasurement(question),
    "procedure" = renderQuestionProcedure(question),
    "drug" = renderQuestionDrug(question)
  )
  return(NULL)
}

# could be ignored if using search.
renderQuestionDemo = function(question){
  label = question$s
  if(question$s_numeric == TRUE){
    insertNumericInput(label)
  }else{
    choices = question$s_choice
    insertCheckboxGroupInput(label,choices)
  }
  return(NULL)
}

renderQuestionCondition = function(question) {
  label1 = question$s1
  choice1 = question$s1_choice = c("YES", "NO")
  insertRatioButtons(label1, choice1)
  label2 = question$s2
  choice2 = question$s2_numeric
  insertTimeInput(label2)
  if (!is.na(question$s3)) {
    label3 = question$s3
    choice3 = question$s3_choice
    insertCheckboxGroupInput(label3, choice3)
  }
  return(NULL)
}

renderQuestionProcedure = function(question) {
  label1 = question$s1
  choice1 = question$s1_choice = c("YES", "NO")
  insertRatioButtons(label1, choice1)
  label2 = question$s2
  choice2 = question$s2_numeric
  insertTimeInput(label2)
  return(NULL)
}

renderQuestionMeasurement = function(question) {
  label1 = question$s1
  choice1 = question$s1_numeric
  insertNumericInput(label1)
  return(NULL)
}

renderQuestionDrug = function(question) {
  label1 = question$s1
  choice1 = question$s1_choice = c("YES", "NO")
  insertRatioButtons(label1, choice1)
  label2 = question$s2
  choice2 = question$s2_numeric
  insertTimeInput(label2)
  return(NULL)
}

renderQuestionObservation = function(question) {
  label1 = question$s1
  choice1 = question$s1_choice = c("YES", "NO")
  insertRatioButtons(label1, choice1)
  return(NULL)
}

insertRatioButtons <- function(label, choices) {
  insertUI(
    selector = "#placeholder1",
    where = "beforeBegin",
    radioButtons(
      inputId = "radio",
      label = label,
      choices = choices
    ),
    immediate = TRUE
  )
  return(NULL)
}

insertCheckboxGroupInput = function(label, choices) {
  insertUI(
    selector = "#placeholder1",
    where = "beforeBegin",
    checkboxGroupInput(
      inputId = "checkbox",
      label = label,
      choices = choices
    ),
    immediate = TRUE
  )
  return(NULL)
}

insertNumericInput <- function(label) {
  insertUI(
    selector = "#placeholder1",
    where = "beforeBegin",
    numericInput(
      inputId = "value",
      label = label,
      value = -1
    ),
    immediate = TRUE
  )
  return(NULL)
}

insertTimeInput = function(label) {
  insertUI(
    selector = "#placeholder1",
    where = "beforeBegin",
    tags$div(tags$label(label),
             fluidRow(
               column(6,
                      numericInput(
                        inputId = "time",
                        label = '',
                        value = -1
                      )),
               column(6,
                      selectInput(
                        inputId = "time_unit",
                        label = '',
                        choices = c("days", "weeks", "months", "years")
                      ))
             )),
    immediate = TRUE
  )
  return(NULL)
}
