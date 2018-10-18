observeEvent(input$update, {
  req(react$wMatrix, react$common_concept_id)
  if (dim(react$wMatrix)[1] > 0) {
    # standardize answer
    answer = input2answer(input)
    # update the wMatrix_tmp.
    react$wMatrix_tmp = updateWMatrix(
      wMatrix = react$wMatrix,
      common_concept_id = react$common_concept_id,
      answer = answer
    )
    # update trial set
    nct1 = react$wMatrix_tmp %>% pull(nct_id) %>% unique()
    nct2 = react$wMatrix %>% pull(nct_id) %>% unique()
    nct3 = react$trialSet
    react$trialSet_tmp = setdiff(nct3, setdiff(nct2, nct1))
    
    # render trial table
    output$trial_info = renderTrialInfo(react$trialSet_tmp, TRIAL_INFO, session)
    
    # render removed trial table
    output$trial_info_removal = renderTrialInfo(setdiff(nct2, nct1), TRIAL_INFO, session)

    # go to the trial tab when clicking the button
    # updateTabsetPanel(session, inputId = "navbar", selected = "trials")
  } else{
    showNotification("All trials have been filtered out.")
  }
})