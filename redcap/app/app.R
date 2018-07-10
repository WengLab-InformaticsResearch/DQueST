library(shiny)
library(shinyjs)
library(shinyBS)
library(R.utils)
sourceDirectory("util")


wMatrix = wMatrixInitByCsv(File = "model/mock_w_matrix.csv") # for test only.
# wMatrix = wMatrixInitByCsv(File = ".model//knowledgeBase.csv") # for test only.
titleDt = trialDtInitByCsv(File = 'model/titleDt.csv')
demoDt = demoDtInitByCsv(File = "model/demoDt.csv")
conditionDt = conditionDtInitByCsv(File = "model/conditionDf.csv")
geoDt = geoDtInitByCsv(File = "model/geoDf_py.csv")
countryName = geoDt %>% pull(country) %>% unique()
stateName = geoDt %>% pull(state) %>% unique()
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
    h4("You can search by condition, demographics and location"),
    wellPanel(
      textInput(
        inputId = "condition",
        label = "Enter condition to search",
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
        selected = "All",
        inline = TRUE,
        choices = c("All", "Male", "Female")
      ),
      selectizeInput(
        inputId = 'countrySelection',
        label = 'Select country',
        choices = countryName,
        multiple = TRUE
      ),
      selectizeInput(
        inputId = 'stateSelection',
        label = 'Select state',
        choices = stateName,
        multiple = TRUE
      ),
      checkboxInput(inputId = "ctrl",
                    label = "Looking for healthy volunteers",
                    value = FALSE)
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
  
  # update state selection
  observe({
    x <- input$countrySelection
    if (!is.null(x)) {
      updateSelectizeInput(
        session,
        inputId = "stateSelection",
        label = "Select state",
        choices = geoDt %>% filter(country %in% input$countrySelection) %>% pull(state) %>% unique()
      )
    }
  })
  # event search button
  observeEvent(input$search, {
    if (is.null(input$condition)) {
      # condition is required.
      showNotification(paste("condition is required for search"), duration = 0)
    } else{
      # always search from start.
      
      #query = formQuery(input, session)
      termTrial = searchByTerm(conditionDt = conditionDt,
                               term = input$condition)
      if(input$age > 0 | input$gender != 'All' | input$ctrl != FALSE){
        # patient made something other than default.
        demoTrial = searchByDemo(
          demoDt = demoDt,
          gender = input$gender,
          age = input$age,
          ctrl = input$ctrl
        )
      }else{
        demoTrial = termTrial
      }
      
      if(!is.null(input$countrySelection) | !is.null(input$stateSelection)){
        geoTrial = searchByGeo(
          geoDt = geoDt,
          country = input$countrySelection,
          state = input$stateSelection
        )
      }else{
        geoTrial = termTrial
      }
      
      react$trialSet = intersect(intersect(termTrial,demoTrial),geoTrial)
      # update search result.
      react$wMatrix_tmp = react$wMatrix %>% filter(nct_id %in% react$trialSet)
      # render trial table
      output$trial_info = renderTrialInfo(react$trialSet, trialDt, session)
      # go to the trial tab when clicking the button
      updateTabsetPanel(session, inputId = "navbar", selected = "trials")
    }
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