library(shiny)
library(shinyjs)
source("../model/search.R")
source("../model/optimize.R")
source("../model/init.R")
source("../model/question.R")
source("../model/update.R")
source("util/formAnswer.R")
source("util/formQuery.R")
source("util/refresh.R")
source("util/renderQuestion.R")
source("util/renderTrialInfo.R")

wMatrix = initByCsv(File = "../resource/mock_w_matrix.csv") # for test only.
ui <- navbarPage(
  "eqacts Shiny app",
  id = "navbar",
  header = tagList(
    useShinyjs(),
    extendShinyjs("www/app-shinyjs.js", functions = c("updateHistory"))
  ),
  
  tabPanel(
    "Search",
    value = "search",
    h2("You can search by condition and demographics"),
    wellPanel(textInput(
      inputId = "keyword",
      label = "Enter string to search",
      value = NULL,
      placeholder = 'cancer'
    ),
    numericInput(
      inputId = "age",
      label = "Enter Age to search",
      value = -1,
      min = 0,
      max = 100
    ),
    radioButtons(
      inputId = "gender",
      label = "Enter gender to search",
      selected = "Male",
      inline = TRUE,
      choiceNames = c("Male", "Female"),
      choiceValues = c("Male", "Female")
    )),
    actionButton(inputId = "search", label = "Search"),
    actionButton(inputId = "restart", label = "Restart")
    
  ),
  tabPanel(
    "Trials",
    value = "trials",
    h2("Search results:"),
    actionButton(
      inputId = "continue",
      text = "Continue",
      label = "Continue"
    ),
    wellPanel(DTOutput(outputId = "trial_info"))
  ),
  tabPanel(
    "QA",
    value = "qa",
    h2("You can search by answering the questions"),
    wellPanel(fluidRow(column(12, tags$div(
      id = "uiInput", tags$div(id = "placeholder")
    )))),
    checkboxInput(inputId = "speed", label = "Fast Filtering", value = TRUE),
    actionButton(inputId = "submit", label = "Submit"),
    actionButton(inputId = "skip", label = "Skip")
    
  ),
  tabPanel(
    "About",
    value = "about",
    "Basic demo of supporting navigation in a Shiny app by",
    tags$a("Dean Attali", href = "http://deanattali.com")
  ),
  
  # javascript embedded.
  tags$script(
    "
    Shiny.addCustomMessageHandler('resetValue', function(variableName) {
    Shiny.onInputChange(variableName, null);
    });
    "
  )
)

# Define server logic
server <- function(input, output, session) {
  # init global var.
  react <- reactiveValues(wMatrix = wMatrix)
  
  # event search button
  observeEvent(input$search, {
    req(input$keyword, input$age, input$gender, react$wMatrix)
    # search items.
    query = formQuery(input, session)
    react$wMatrix = searchByAll(
      wMatrix = react$wMatrix,
      gender = query$gender,
      age = query$age,
      term = query$term
    )
    # restart the value.
    output$trial_info = renderTrialInfo(react$wMatrix, session)
    # go to the trial tab when clicking the button
    updateTabsetPanel(session, inputId = "navbar", selected = "trials")
  })
  
  # event restart button
  observeEvent(input$restart, {
    refreshAll(session)
  })
  
  # event continue button
  observeEvent(input$continue, {
    req(react$wMatrix)
    # optimize.
    react$common_concept_id = findConcept(wMatrix = react$wMatrix)
    # render the question.
    question = questionGet(wMatrix = react$wMatrix,
                           idx = react$common_concept_id)
    output$trial_info = renderTrialInfo(react$wMatrix, session)
    renderQuestion(question, session)
    
    # go to the search tab when clicking the button
    updateTabsetPanel(session, inputId = "navbar", selected = "qa")
  })
  
  # event submit button
  observeEvent(input$submit, {
    req(react$wMatrix, react$common_concept_id)
    # standardize the answer.
    answer = formAnswer(input, session)
    # update the wMatrix.
    react$wMatrix = updateWMatrix(
      wMatrix = react$wMatrix,
      common_concept_id = react$common_concept_id,
      answer = answer,
      speed = input$speed
    )
    output$trial_info = renderTrialInfo(react$wMatrix, session)
    # restart the value.
    # refreshQA(session)
    # go to the trial tab when clicking the button
    updateTabsetPanel(session, inputId = "navbar", selected = "trials")
  })
  
  # event skip button
  observeEvent(input$skip, {
    req(react$wMatrix, react$common_concept_id)
    # update the wMatrix.
    react$wMtrix = updateWMatrix(
      wMatrix = react$wMatrix,
      common_concept_id = common_concept_id,
      answer = NULL
    )
    output$trial_info = renderTrialInfo(react$wMatrix, session)
    # restart the value.
    # refreshQA(session)
    # go to the trial tab when clicking the button
    updateTabsetPanel(session, inputId = "navbar", selected = "trials")
  })
  
}

shinyApp(ui = ui, server = server)