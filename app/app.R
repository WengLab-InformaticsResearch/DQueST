library(shiny)
library(shinyjs)
source("../model/search.R")
source("../model/optimize.R")
source("../model/init.R")
source("../model/question.R")
source("../model/update.R")
source("util/formAnswer.R")
source("util/formQuery.R")
source("util/renderInit.R")
source("util/renderQuestion.R")
source("util/renderTrialInfo.R")

wMatrix = initByCsv("../test/mock_w_matrix.csv")
wMatrix = 1 # for test only.
ui <- navbarPage(
  "eqacts Shiny app",
  id = "navbar",
  header = tagList(
    useShinyjs(),
    extendShinyjs("www/app-shinyjs.js", functions = c("updateHistory"))
  ),
  
  tabPanel("Search", value = "search",
           h2("You can search by condition and demographics"),
           textInput(inputId = "keyword", 
                     label = "Enter string to search",
                     value = NULL,
                     placeholder = 'cancer'),
           numericInput(inputId = "age",
                        label = "Enter Age to search",
                        value = -1,min = 0,max = 100),
           radioButtons(inputId = "gender",
                        label = "Enter gender to search",
                        selected = "Male",inline = TRUE,
                        choiceNames = c("Male","Female"),
                        choiceValues = c("Male","Female")
           ),
           actionButton(inputId = "search", label = "Search"),
           actionButton(inputId = "restart", label = "Restart")
           
  ),
  tabPanel("Trials", value = "trials",
           h2("Search results:"),
           actionButton(inputId = "continue",
                        text = "Continue",
                        label = "Continue"),
           textOutput("trial_info", inline = TRUE)
  ),
  tabPanel("About", value = "about",
           "Basic demo of supporting navigation in a Shiny app by",
           tags$a("Dean Attali", href = "http://deanattali.com")
  ),
  
  # javascript embedded.
  tags$script("
    Shiny.addCustomMessageHandler('resetValue', function(variableName) {
              Shiny.onInputChange(variableName, null);
              });
              ")
)    

# Define server logic
server <- function(input, output, session) {
  
  # init global var.
  react <- reactiveValues(wMatrix = wMatrix)
  
  # event search button
  observeEvent(input$search,{
    req(input$keyword, input$age, input$gender,react$wMatrix)
    # search items.
    query = formQuery(input,session)
    react$wMatrix = searchByAll(wMatrix = react$wMatrix,gender = query$gender,age = query$age,term = query$term)
    # restart the value.
    renderTrials(react$wMatrix,session)
    # go to the trial tab when clicking the button
    updateTabsetPanel(session, inputId = "navbar", selected = "trials")
  })
  
  # event restart button
  observeEvent(input$restart,{
    renderInit(session)
  })
  
  # event continue button
  observeEvent(input$continue,{
    req(react$wMatrix)
    # optimize.
    common_concept_id = findConcept(wMatrix = react$wMatrix)
    # render the question.
    question = questionRender(wMatrix = react$wMatrix,common_concept_id = common_concept_id)
    renderQuestion(question,session)
    # standardize the answer.
    answer = formAnswer(input,session)
    # update the wMatrix.
    react$wMtrix = updateWMatrix(wMatrix = react$wMatrix,common_concept_id = common_concept_id,answer = answer)
    # restart the value.
    renderTrials(react$wMatrix,session)
  })
  
}

shinyApp(ui = ui, server = server)