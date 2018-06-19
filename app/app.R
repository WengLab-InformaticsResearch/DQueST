library(shiny)
library(shinyjs)
library(shinyBS)
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
# source("util/buttonHelper.R")
# source("util/SwitchButton.R")


wMatrix = initByCsv(File = "../resource/mock_w_matrix.csv") # for test only.
ui <- navbarPage(
  "eqacts Shiny app",
  id = "navbar",
  header = tagList(
    useShinyjs(),
    extendShinyjs("www/js/app-shinyjs.js", functions = c("updateHistory"))
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
    actionButton(inputId = "search", label = "Search",class = "btn-primary"),
    actionButton(inputId = "restart", label = "Restart",class = "btn-secondary")
    
  ),
  tabPanel(
    "Trials",
    value = "trials",
    h4("Search results:"),
    wellPanel(DTOutput(outputId = "trial_info")),
    h4("You can search by answering the questions"),
    wellPanel(fluidRow(column(12, tags$div(id = "uiInput1", tags$div(id = "placeholder1")))),
              fluidRow(column(12, tags$div(id = "uiInput2", tags$div(id = "placeholder2"))))),
    checkboxInput(inputId = "speed", label = "Fast Filtering", value = TRUE),
    # actionButton(inputId = "skip", label = "Skip", class = "btn-danger"),
    actionButton(
      inputId = "continue",
      label = "Continue",
      class = "btn-info"
    )
  ),
  tabPanel(
    "About",
    value = "about",
    "Basic demo of supporting navigation in a Shiny app by",
    tags$a("Dean Attali", href = "http://deanattali.com")
  ),
  
  # javascript embedded.
  tags$head(tags$script(src="js/app.js")),
  # tags$script(
  #   "
  #   Shiny.addCustomMessageHandler('resetValue', function(variableName) {
  #   Shiny.onInputChange(variableName, null);
  #   });
  #   "
  # )
  # add toolTip
  bsTooltip(id = "continue", title = "Start question or continue to next question", 
            placement = "right", trigger = "hover")
)

# Define server logic
server <- function(input, output, session) {
  # init global var.
  react <- reactiveValues(wMatrix = wMatrix, wMatrix_tmp = wMatrix, common_concept_id = NULL)
  
  # event search button
  observeEvent(input$search, {
    req(input$keyword, input$age, input$gender, react$wMatrix)
    # search items.
    if(dim(react$wMatrix)[1] > 0) {
      query = formQuery(input, session)
      react$wMatrix = searchByAll(
        wMatrix = react$wMatrix,
        gender = query$gender,
        age = query$age,
        term = query$term
      )
      # restart the value.
      react$wMatrix_tmp = react$wMatrix
      output$trial_info = renderTrialInfo(react$wMatrix_tmp, session)
      # go to the trial tab when clicking the button
      updateTabsetPanel(session, inputId = "navbar", selected = "trials")
    } else{
      showNotification("All trials have been filtered out.")
    }
  })
  
  # event restart button
  observeEvent(input$restart, {
    refreshAll(session)
  })
  
  # event continue button
  observeEvent(input$continue, {
    req(react$wMatrix_tmp)
    if(dim(react$wMatrix_tmp)[1] > 0){
      react$wMatrix = react$wMatrix_tmp
      output$trial_info = renderTrialInfo(react$wMatrix, session)
      # optimize.
      react$common_concept_id = findConcept(wMatrix = react$wMatrix)
      # render the question.
      question = questionGet(wMatrix = react$wMatrix,
                             idx = react$common_concept_id)
      refreshQA(session)
      renderQuestion(question, session)
      # go to the search tab when clicking the button
      # updateTabsetPanel(session, inputId = "navbar", selected = "qa")
    } else{
      showNotification("All trials have been filtered out.")
    }
    
  })
  
  # event submit button
  observeEvent(input$submit, {
    req(react$wMatrix, react$common_concept_id)
    if (dim(react$wMatrix)[1] > 0) {
      if (input$skip == TRUE) {
        showNotification("You have selected skip the question.")
        answer = NULL
      } else{
        # standardize the answer.
        answer = formAnswer(input, session)
      }
      # update the wMatrix_tmp.
      react$wMatrix_tmp = updateWMatrix(
        wMatrix = react$wMatrix,
        common_concept_id = react$common_concept_id,
        answer = answer,
        speed = input$speed
      )
      output$trial_info = renderTrialInfo(react$wMatrix_tmp, session)
      # go to the trial tab when clicking the button
      # updateTabsetPanel(session, inputId = "navbar", selected = "trials")
    } else{
      showNotification("All trials have been filtered out.")
    }
  })
  
  # event skip button
  # observeEvent(input$skip, {
  #   req(react$wMatrix, react$common_concept_id)
  #   if(dim(react$wMatrix)[1] > 0) {
  #     # update the wMatrix.
  #     react$wMatrix = updateWMatrix(
  #       wMatrix = react$wMatrix,
  #       common_concept_id = react$common_concept_id,
  #       answer = NULL
  #     )
  #     output$trial_info = renderTrialInfo(react$wMatrix, session)
  #     
  #     # restart the value.
  #     refreshQA(session)
  #     # go to the trial tab when clicking the button
  #     # updateTabsetPanel(session, inputId = "navbar", selected = "trials")
  #   } else{
  #     showNotification("All trials have been filtered out.")
  #   }
  # })
  
}

shinyApp(ui = ui, server = server)