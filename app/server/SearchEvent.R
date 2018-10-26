observeEvent(input$search, {
  withBusyIndicatorServer("search",
                          if (is.null(input$condition)) {
                            # condition is required.
                            showNotification(paste("Condition is required for search"), duration = 0)
                          } else{
                            # get nct_id.
                            react$trialSet_tmp = getTrialsBySearch(
                              con = react$MY_CON,
                              condition = input$condition,
                              age = input$age,
                              gender = input$gender,
                              country = input$country,
                              state = input$state,
                              ctrl = input$ctrl
                            )
                            # update search result.
                            react$wMatrix_tmp = react$wMatrix[nct_id %in% react$trialSet_tmp]
                            # render trial table
                            output$trial_info = renderTrialInfo(react$trialSet_tmp, TRIAL_INFO, session)
                            # go to the trial tab when clicking the button
                            updateTabsetPanel(session, inputId = "navbar", selected = "trials")
                          })
  
  
})