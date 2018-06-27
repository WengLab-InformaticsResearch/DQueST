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


wMatrix = wMatrixInitByCsv(File = "../resource/mock_w_matrix.csv") # for test only.
titleDt = trialDtInitByCsv(File = '../resource/titleDt.csv')
demoDt = demoDtInitByCsv(File = "../resource/demoDt.csv")
#keywordDT = initByRd(rdata = "../resource/demoDt.rda")
#locationDT = initByRd(rdata = "../resource/demoDt.rda")
trialDt = titleDt # the information want to render


ui <- navbarPage(
  "eqacts",
  id = "navbar",
  header = tagList(
    img(src='gif/ajax-loader-bar.gif', align = "left"),
    useShinyjs(),
    extendShinyjs("www/js/app-shinyjs.js", functions = c("updateHistory"))
  ),
  
  tabPanel(
    "Search",
    value = "search",
    h2("You can search by condition and demographics"),
    wellPanel(
      textInput(
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
      ),
      radioButtons(
        inputId = "ctrl",
        label = "Looking for healthy volunteers",
        selected = "Yes",
        inline = TRUE,
        choiceNames = c("Yes", "No"),
        choiceValues = c("Yes", "No")
      )
    ), 

    actionButton(inputId = "search", label = "Search",class = "btn-primary"),
    bsTooltip(id = "search", title = "search among all trials", placement = "bottom", trigger = "hover")
  ),
  tabPanel(
    "Trials",
    value = "trials",
    
    h4("You can search by answering the questions"),
    wellPanel(fluidRow(column(12, tags$div(id = "uiInput1", tags$div(id = "placeholder1")))),
              fluidRow(column(12, tags$div(id = "uiInput2", tags$div(id = "placeholder2"))))),
    checkboxInput(inputId = "speed", label = "Fast Filtering", value = TRUE),
    # actionButton(inputId = "skip", label = "Skip", class = "btn-danger"),
    actionButton(
      inputId = "continue",
      label = "Continue",
      class = "btn-info"
    ),
    actionButton(inputId = "restart", label = "Restart",class = "btn-secondary"),
    bsTooltip(id = "restart", title = "refresh the app", placement = "bottom", trigger = "hover"),
    h4("Search results:"),
    wellPanel(DT::dataTableOutput(outputId = "trial_info"))
  ),
  tabPanel(
    "About",
    value = "about",
    "This a demo of eqatcs - a dynamic question generating system for trial filtering.
    User could either search by keyword or answer the questions to filter the trials.",
    tags$a("source code", href = "https://github.com/stormliucong/",target="_blank" )
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
            placement = "right", trigger = "hover"),
  bsTooltip(id = "speed", title = "check this box to filter out trials fast meanwhile losing some accuracy", 
            placement = "right", trigger = "hover")
)

# Define server logic
server <- function(input, output, session) {
  # init global var.
  react <- reactiveValues(
    wMatrix = wMatrix,
    wMatrix_tmp = wMatrix,
    trialSet = trialDt %>% pull(nct_id) %>% unique(),
    common_concept_id = NULL
  )
  
  # event search button
  observeEvent(input$search, {
    req(input$age, input$gender, input$ctrl, demoDt, trialDt)
    # search items.
    
    # always search from start.
    
    #query = formQuery(input, session)
    react$trialSet = searchByAll(
      demoDt = demoDt,
      gender = input$gender,
      age = input$age,
      term = input$keyword,
      ctrl = input$ctrl
    )
    # update search result.
    react$wMatrix_tmp = react$wMatrix %>% filter(nct_id %in% react$trialSet)
    # render trial table
    output$trial_info = renderTrialInfo(react$trialSet, trialDt, session)
    # go to the trial tab when clicking the button
    updateTabsetPanel(session, inputId = "navbar", selected = "trials")
    
  })
  
  # event restart button
  observeEvent(input$restart, {
    refreshAll(session)
  })
  
  # event continue button
  observeEvent(input$continue, {
    req(react$trialSet)
    if(dim(react$wMatrix_tmp)[1] > 0){
      # confirm the update or search results
      react$wMatrix = react$wMatrix_tmp
      # render the results
      # output$trial_info = renderTrialInfo(react$wMatrix, TrialDt, session)
      # optimize.
      react$common_concept_id = findConcept(wMatrix = react$wMatrix)
      # generate the question.
      question = questionGet(wMatrix = react$wMatrix,
                             idx = react$common_concept_id)
      #refresh question form.
      refreshQA(session)
      # store questions.
      # renderAnswer(session)
      # render question
      renderQuestion(question, session)
      # go to the search tab when clicking the button
      # updateTabsetPanel(session, inputId = "navbar", selected = "qa")
    } else{
      showNotification("All trials have been filtered out or all trial criteria has been asked")
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
      # update trial set
      nct1 = react$wMatrix_tmp %>% select(nct_id) %>% distinct()
      nct2 = react$wMatrix %>% select(nct_id) %>% distinct()
      nct3 = react$trialSet
      react$trialSet = setdiff(nct3,setdiff(nct2,nct1))
      # render trial table
      output$trial_info = renderTrialInfo(react$trialSet,trialDt, session)
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