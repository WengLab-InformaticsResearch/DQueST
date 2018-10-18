tabPanel(
  "About",
  value = "about",
  "This a demo of DQeST - a dynamic question generating system for trial filtering.
  User could either search by keyword or answer the questions to filter the trials.",
  tags$a("source code", href = "https://github.com/stormliucong/", target =
           "_blank"),
  wellPanel(DT::dataTableOutput(outputId = "knowledge_base")),
  wellPanel(DT::dataTableOutput(outputId = "ie_results_raw"))
  
)