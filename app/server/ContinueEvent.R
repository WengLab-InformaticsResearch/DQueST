observeEvent(input$continue, {
  req(react$trialSet_tmp)
  if (dim(react$wMatrix_tmp)[1] == 0 |
      length(react$trialSet_tmp) == 0) {
    showNotification("All trials have been filtered out or all trial criteria has been asked")
  } else{
    # confirm update
    react$wMatrix = react$wMatrix_tmp
    react$trialSet = react$trialSet_tmp
    # optimizing.
    react$common_concept_id = findConcept(wMatrix = react$wMatrix,
                                          asked_concept_id = react$asked_concept_id)
    react$asked_concept_id = c(react$asked_concept_id, react$common_concept_id)
    #refresh question form.
    refreshQA(session)
    # render question.
    renderQuestion(react$common_concept_id, react$wMatrix, session)
    
    # update count.
    if (length(input$skip) > 0) {
      if (input$skip == FALSE) {
        counter$countervalue <- counter$countervalue + 1
      }
    }
  }
})