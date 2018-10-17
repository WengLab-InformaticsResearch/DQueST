updateWMatrix = function(wMatrix,
                         common_concept_id,
                         answer) {
  # update wMatrix.
  wMatrix_new = wMatrix
  # print(paste0("--trials Origin:",wMatrix %>% pull(nct_id) %>% unique() %>% length()))
  
  relatedTrialsWMatrix = wMatrix_new %>% filter(common_omop_id == common_concept_id)
  # print(paste0("--trials Related:",relatedTrialsWMatrix %>% pull(nct_id) %>% unique() %>% length()))
  
  # only compute if patient answer the question.
  if (!is.null(answer)) {
    domain = relatedTrialsWMatrix %>%
      pull(domain) %>% unique()
    trialsRemoved = removeTrialsByDomain(relatedTrialsWMatrix, domain, answer)
    # print(paste0("--trials Removed:",length(trialsRemoved)))
    # print(paste0("--trials Removed examples:",head(trialsRemoved)))
    
    
    # remove unqualified trials.
    wMatrix_new = wMatrix_new %>% filter(!nct_id %in% trialsRemoved)
    wMatrix_new = wMatrix_new %>% filter(common_omop_id != common_concept_id) %>% as.data.table()
    # print(paste0("--trials Updated:",wMatrix_new %>% pull(nct_id) %>% unique() %>% length()))
  }
  # remove the criteria from the working matrix.
  # THIS IS A BUG: remove this operation
  # wMatrix_new = wMatrix_new %>% filter(common_omop_id != common_concept_id)
  # print(paste0("--trials Updated 2:",wMatrix_new %>% pull(nct_id) %>% unique() %>% length()))
  
  # tryCatch(
  #   expr = {
  #     1
  #   },
  #   error = function(e)
  #     e,
  #   finally = print("update wMatrix")
  # )
  return(wMatrix_new)
}

removeTrialsByDomain = function(relatedTrialsWMatrix,
                                domain,
                                answer
                                ) {
  trialsRemoved = switch(
    domain,
    "condition" = removeTrialsCondition(relatedTrialsWMatrix, answer),
    "observation" = removeTrialsObservation(relatedTrialsWMatrix, answer),
    "measurement" = removeTrialsMeasurement(relatedTrialsWMatrix, answer),
    "procedure" = removeTrialsProcedure(relatedTrialsWMatrix, answer),
    "drug" = removeTrialsDrug(relatedTrialsWMatrix, answer)
  )
  return(trialsRemoved)
}


removeTrial = function(relatedTrialsWMatrixAdd) {
  trialRemoved1 = relatedTrialsWMatrixAdd %>%
    filter(match == TRUE & ie_flag == 0)
  trialRemoved2 = relatedTrialsWMatrixAdd %>%
    filter(match == FALSE & ie_flag == 1)
  trialRemoved = trialRemoved1 %>%
    bind_rows(trialRemoved2) %>%
    pull(nct_id) %>% unique()
  return(trialRemoved)
}

removeTrialsCondition = function(relatedTrialsWMatrix, answer) {
  if (is.null(answer$start)) {
    # no temporal
    relatedTrialsWMatrixAdd = relatedTrialsWMatrix %>%
      mutate(match = (answer$present == TRUE))
  }
  
  if (!is.null(answer$start)) {
    relatedTrialsWMatrixAdd = relatedTrialsWMatrix %>%
      mutate(match = (
        answer$present == TRUE &
          (is.na(temporal_min) | temporal_min <= answer$end) &
          (is.na(temporal_max) | temporal_max >= answer$start)
      ))
  }
  
  trialsRemoved = removeTrial(relatedTrialsWMatrixAdd)
  return(trialsRemoved)
}

removeTrialsObservation = removeTrialsCondition



removeTrialsMeasurement = function(relatedTrialsWMatrix, answer) {
  if (!is.null(answer$value)) {
    relatedTrialsWMatrixAdd = relatedTrialsWMatrix %>%
      mutate(match = ((is.na(value_min) |
                         value_min <= answer$value) &
                        (is.na(value_max) |
                           value_max >= answer$value)
      ))
    trialsRemoved = removeTrial(relatedTrialsWMatrixAdd)
  }
  return(trialsRemoved)
}

removeTrialsProcedure = removeTrialsCondition

removeTrialsDrug = removeTrialsCondition