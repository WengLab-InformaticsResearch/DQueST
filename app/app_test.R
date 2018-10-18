### load config ###
source(file = './config.R')
### end ###

### load ie_result_raw ###
IE_RESULT = get(load(file = "./model/ie_result_raw.rda"))
### end ###

fieldsMandatory <- c("name")

### define UI ###
ui <- navbarPage(
  title = "DQeST",
  id = "navbar",
  
  ### panels ###
  source(file.path("ui", "SearchPanel.R"),  local = TRUE)$value,
  source(file.path("ui", "TrialPanel_test.R"),  local = TRUE)$value,
  source(file.path("ui", "AboutPanel_test.R"),  local = TRUE)$value,
  ### end ###
  
  ### add toolTip ###
  bsTooltip(
    id = "search",
    title = "search among all trials",
    placement = "bottom",
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
  ),
  ### end ###
  
  ### javascript embedded ###
  useShinyjs(),
  tags$head(tags$script(src = "js/app.js"))
  ### end ###
  
)

# Define server logic
server <- function(input, output, session) {
  
  
  ### init global var ###
  react <- reactiveValues(
    wMatrix = CRITERIA_LIB,
    # working lib
    wMatrix_tmp = NULL,
    trialSet = NULL,
    # candidate pool
    trialSet_tmp = NULL,
    common_concept_id = NULL,
    asked_concept_id = NULL
  )
  ### end ###
  
  ### render knowledgebase ###
  output$knowledge_base = DT::renderDataTable(
    CRITERIA_LIB,
    escape = FALSE,
    server = TRUE,
    filter = 'top',
    rownames = F,
    options = list(scrollX = TRUE)
  )
  ### end ###
  output$ie_results_raw = DT::renderDataTable(
    IE_RESULT,
    escape = FALSE,
    server = TRUE,
    filter = 'top',
    rownames = F,
    options = list(scrollX = TRUE)
  )
  ### end ###
  
  ### render IE_results ###
  
  
  ### init question count ###
  counter <- reactiveValues(countervalue = 0)
  ### end ###
  
  ### event search button ###
  source(file.path("server", "SearchEvent.R"),  local = TRUE)$value
  ### end ###
  
  #### event restart button ###
  source(file.path("server", "RestartEvent.R"),  local = TRUE)$value
  ### end ###
  
  ### event continue button ###
  source(file.path("server", "ContinueEvent.R"),  local = TRUE)$value
  ### end ###
  
  # event update button
  source(file.path("server", "UpdateEvent_test.R"),  local = TRUE)$value
  ### end ###
  
  ### output question count ###
  output$count <- renderText({
    paste("question answered is ", counter$countervalue)   # print the latest value stored in the reactiveValues object
  })
  ### end ###
  
  ### update state selection ###
  source(file.path("server", "DynamicStateUi.R"),  local = TRUE)$value
  ### end ###
  
  ### dynamic ui within questions ###
  source(file.path("server", "DynamicTimeUi.R"),  local = TRUE)$value
  ### end ###
  
}

shinyApp(ui = ui, server = server, onStop(function() {
  lapply(dbListConnections(drv = dbDriver("PostgreSQL")), function(x) {dbDisconnect(conn = x)})
}))
