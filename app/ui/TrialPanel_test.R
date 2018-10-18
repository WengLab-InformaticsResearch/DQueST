tabPanel(
  "Trials",
  value = "trials",
  
  h4("You can search by answering the questions"),
  wellPanel(fluidRow(column(
    12, tags$div(id = "questionbox", tags$div(id = "questionplaceholder"))
  )),
  fluidRow(column(
    12, tags$div(id = "question_box_anchor", tags$div(id = "placeholder2"))
  )),
  fluidRow(column(
    12, tags$div(id = "questionbuttonbox", tags$div(id = "buttonplaceholder"))
  ))),
  actionButton(
    inputId = "continue",
    label = "Continue",
    class = "btn-info"
  ),
  actionButton(
    inputId = "restart",
    label = "Restart",
    class = "btn-danger"
  ),
  br(),
  textOutput("count"),
  h4("Search results:"),
  br(),
  wellPanel(DT::dataTableOutput(outputId = "trial_info_removal")),
  wellPanel(DT::dataTableOutput(outputId = "trial_info"))
)