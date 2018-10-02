observeEvent(input$restart, {
  refreshAll(session)
  counter$countervalue <- 0
})