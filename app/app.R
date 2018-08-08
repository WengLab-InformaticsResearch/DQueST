# Sys.setenv(http_proxy="http://bcp3.cumc.columbia.edu:8080")
rm(list = ls())
# setwd('~/Projects/eqacts/app/')
library(shiny)
library(shinyjs)
library(shinyBS)
# library(devtools)
# devtools::install_github("AnalytixWare/ShinySky")
library(shinysky)
# library(R.utils)
# sourceDirectory("./util")
file.sources = list.files(
  c("./util"),
  pattern = "*.R$",
  full.names = TRUE,
  ignore.case = TRUE
)
sapply(file.sources, source, .GlobalEnv)
post_data = load_data()

ui <- navbarPage(
  "eqacts",
  id = "navbar",
  header = tagList(
    useShinyjs(),
    extendShinyjs("www/js/app-shinyjs.js", functions = c("updateHistory"))
  ),
  
  tabPanel(
    "Search",
    value = "search",
    h4("You can search by condition, demographics and location"),
    wellPanel(
      # textInput(
      #   inputId = "condition",
      #   label = "Enter condition to search",
      #   value = NULL,
      #   placeholder = 'cancer'
      # ),
      select2Input(
        "condition",
        label = "Enter condition to search",
        choices = c(post_data$conditionChoices),
        type = c("input", "select"),
        multiple=FALSE
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
        choices = post_data$countryName,
        multiple = TRUE
      ),
      selectizeInput(
        inputId = 'stateSelection',
        label = 'Select state',
        choices = post_data$stateName,
        multiple = TRUE
      ),
      checkboxInput(
        inputId = "ctrl",
        label = "Looking for healthy volunteers",
        value = FALSE
      )
    ),
    
    actionButton(
      inputId = "search",
      label = "Search",
      class = "btn-primary"
    )
  ),
  tabPanel(
    "Trials",
    value = "trials",
    
    h4("You can search by answering the questions"),
    wellPanel(fluidRow(column(
      12, tags$div(id = "uiInput1", tags$div(id = "placeholder1"))
    )),
    fluidRow(column(
      12, tags$div(id = "uiInput2", tags$div(id = "placeholder2"))
    ))),
    checkboxInput(
      inputId = "speed",
      label = "Fast Filtering",
      value = TRUE
    ),
    # actionButton(inputId = "skip", label = "Skip", class = "btn-danger"),
    actionButton(
      inputId = "continue",
      label = "Continue",
      class = "btn-info"
    ),
    actionButton(
      inputId = "restart",
      label = "Restart",
      class = "btn-secondary"
    ),
    br(),
    textOutput("count"),
    h4("Search results:"),
    br(),
    wellPanel(DT::dataTableOutput(outputId = "trial_info_removal")),
    wellPanel(DT::dataTableOutput(outputId = "trial_info"))
  ),
  tabPanel(
    "About",
    value = "about",
    "This a demo of eqatcs - a dynamic question generating system for trial filtering.
    User could either search by keyword or answer the questions to filter the trials.",
    tags$a("source code", href = "https://github.com/stormliucong/", target =
             "_blank")
  ),
  
  # javascript embedded.
  tags$head(tags$script(src = "js/app.js")),
  
  # add toolTip
  bsTooltip(
    id = "search",
    title = "search among all trials",
    placement = "bottom",
    trigger = "hover"
  ),
  bsTooltip(
    id = "speed",
    title = "check this box to filter out trials fast meanwhile losing some accuracy",
    placement = "right",
    trigger = "hover"
  ),
  bsTooltip(
    id = "continue",
    title = "Start question or continue to next question",
    placement = "right",
    trigger = "hover"
  ),
  bsTooltip(
    id = "restart",
    title = "refresh the app",
    placement = "bottom",
    trigger = "hover"
  )
  )

# Define server logic
server <- function(input, output, session) {
  # init global var.
  react <- reactiveValues(
    wMatrix = post_data$wMatrix,
    wMatrix_tmp = NULL,
    trialSet = NULL,
    trialSet_tmp = NULL,
    common_concept_id = NULL,
    asked_concept_id = NULL
  )
  
  #init question count.
  counter <- reactiveValues(countervalue = 0)
  
  # update state selection
  observe({
    x <- input$countrySelection
    if (!is.null(x)) {
      updateSelectizeInput(
        session,
        inputId = "stateSelection",
        label = "Select state",
        choices = post_data$geoDt %>% filter(country %in% input$countrySelection) %>% pull(state) %>% unique()
      )
    }
  })
  
  # event search button
  observeEvent(input$search, {
    if (is.null(input$condition)) {
      # condition is required.
      showNotification(paste("Condition is required for search"), duration = 0)
    } else{
      #query = formQuery(input, session)
      # termTrial = searchByTerm(conditionDt = conditionDt,
      #                          term = input$condition)
      # term search.
      termTrial = searchByApi(trialDt = post_data$trialDt,
                              term = input$condition)
      if(length(termTrial) < 1){
        termTrial = post_data$wMatrix %>% pull(nct_id) %>% unique()
      }
      
      # demo search.
      if ((!is.null(input$age) &
           input$age > 0) |
          (!is.null(input$gender) &
           input$gender != 'All') |
          (!is.null(input$ctrl) & 
           input$ctrl != FALSE)) {
        # patient made something other than default.
        demoTrial = searchByDemo(
          demoDt = post_data$demoDt,
          gender = input$gender,
          age = input$age,
          ctrl = input$ctrl
        )
        if(length(demoTrial) < 1){
          demoTrial = termTrial
        }
      } else{
        demoTrial = termTrial
      }
      
      # geo search.
      if (!is.null(input$countrySelection) |
          !is.null(input$stateSelection)) {
        geoTrial = searchByGeo(
          geoDt = post_data$geoDt,
          country = input$countrySelection,
          state = input$stateSelection
        )
        if(length(geoTrial) < 1){
          geoTrial = termTrial
        }
      } else{
        geoTrial = termTrial
      }
      
      react$trialSet_tmp = intersect(intersect(termTrial, demoTrial), geoTrial)
      # update search result.
      react$wMatrix_tmp = react$wMatrix %>% filter(nct_id %in% react$trialSet_tmp)
      # render trial table
      output$trial_info = renderTrialInfo(react$trialSet_tmp, post_data$trialDt, session)
      # go to the trial tab when clicking the button
      updateTabsetPanel(session, inputId = "navbar", selected = "trials")
    }
  })
  
  # event restart button
  observeEvent(input$restart, {
    refreshAll(session)
    counter$countervalue <- 0
  })
  
  # event continue button
  observeEvent(input$continue, {
    req(react$trialSet_tmp)
    if (dim(react$wMatrix_tmp)[1] > 0 &
        length(react$trialSet_tmp) > 0) {
      
      # confirm the update or search results
      react$wMatrix = react$wMatrix_tmp
      
      # print(paste0('trial after filtered:',input$trial_info_rows_all))
      
      react$trialSet = react$trialSet_tmp
      # optimize.
      react$common_concept_id = findConcept(wMatrix = react$wMatrix,asked_concept_id = react$asked_concept_id)
      react$asked_concept_id = c(react$asked_concept_id,react$common_concept_id)
      # generate the question.
      question = questionGet(wMatrix = react$wMatrix,
                             idx = react$common_concept_id)
      #refresh question form.
      refreshQA(session)
      # store questions.
      # renderAnswer(session)
      # render question
      renderQuestion(question, session)
      # update count.
      if (length(input$skip) > 0) {
        if(input$skip == FALSE){
          counter$countervalue <- counter$countervalue + 1
        }
      }
      
      # go to the search tab when clicking the button
      # updateTabsetPanel(session, inputId = "navbar", selected = "qa")
    } else{
      showNotification("All trials have been filtered out or all trial criteria has been asked")
    }
    
  })
  
  # event update button
  observeEvent(input$update, {
    req(react$wMatrix, react$common_concept_id)
    if (dim(react$wMatrix)[1] > 0) {
      if (input$skip == TRUE) {
        showNotification("You have selected skip the question.")
        answer = NULL
      } else{
        # standardize the answer.
        answer = formAnswer(input)
      }
      # update the wMatrix_tmp.
      react$wMatrix_tmp = updateWMatrix(
        wMatrix = react$wMatrix,
        common_concept_id = react$common_concept_id,
        answer = answer,
        speed = input$speed
      )
      # update trial set
      nct1 = react$wMatrix_tmp %>% pull(nct_id) %>% unique()
      nct2 = react$wMatrix %>% pull(nct_id) %>% unique()
      nct3 = react$trialSet
      # print(paste0('--trial updated #:',length(nct1)))
      # print(paste0('--trial original # in knowledgebase:',length(nct2)))
      # print(paste0('--trial updated # in knowledgebase + search results:',length(nct3)))
      # 
      react$trialSet_tmp = setdiff(nct3, setdiff(nct2, nct1))
      # render removed trial table
      output$trial_info_removal = renderTrialInfo(setdiff(nct2, nct1), post_data$trialDt, session)
      # render trial table
      output$trial_info = renderTrialInfo(react$trialSet_tmp, post_data$trialDt, session)
      # go to the trial tab when clicking the button
      # updateTabsetPanel(session, inputId = "navbar", selected = "trials")
    } else{
      showNotification("All trials have been filtered out.")
    }
  })
  
  # output count.
  output$count <- renderText({
    paste("question answered is ", counter$countervalue)   # print the latest value stored in the reactiveValues object
  })
}

shinyApp(ui = ui, server = server)