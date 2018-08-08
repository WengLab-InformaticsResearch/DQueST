updateWMatrix = function(wMatrix,
                         common_concept_id,
                         answer,
                         speed = TRUE) {
  # update wMatrix.
  wMatrix_new = wMatrix
  # print(paste0("--trials Origin:",wMatrix %>% pull(nct_id) %>% unique() %>% length()))
  
  relatedTrialsWMatrix = wMatrix_new %>% filter(common_omop_id == common_concept_id)
  # print(paste0("--trials Related:",relatedTrialsWMatrix %>% pull(nct_id) %>% unique() %>% length()))
  
  # only compute if patient answer the question.
  if (!is.null(answer)) {
    domain = relatedTrialsWMatrix %>%
      pull(domain) %>% unique()
    trialsRemoved = removeTrialsByDomain(relatedTrialsWMatrix, domain, answer, speed)
    # print(paste0("--trials Removed:",length(trialsRemoved)))
    # print(paste0("--trials Removed examples:",head(trialsRemoved)))
    
    
    # remove unqualified trials.
    wMatrix_new = wMatrix_new %>% filter(!nct_id %in% trialsRemoved) %>% as.data.table()
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

removeTrialsByDomain = function(relatedTrialsWMatrix, domain, answer, speed) {
  trialsRemoved = switch(
    domain,
    "demographic" = removeTrialsDemo(relatedTrialsWMatrix, answer, speed),
    "condition" = removeTrialsCondition(relatedTrialsWMatrix, answer, speed),
    "observation" = removeTrialsObservation(relatedTrialsWMatrix, answer, speed),
    "measurement" = removeTrialsMeasurement(relatedTrialsWMatrix, answer, speed),
    "procedure" = removeTrialsProcedure(relatedTrialsWMatrix, answer, speed),
    "drug" = removeTrialsDrug(relatedTrialsWMatrix, answer, speed)
  )
  return(trialsRemoved)
}

removeTrialsDemo = function(relatedTrialsWMatrix, answer, speed) {
  if (!is.null(answer$value)) {
    # age.
    trialsRemoved = relatedTrialsWMatrix %>%
      filter((
        value_min < answer$value &
          value_max > answer$value &
          ie_flag == 0
      ) |
        ((value_min > answer$value |
            value_max < answer$value) & ie_flag == 1
        )) %>%
      pull(nct_id) %>% unique()
  } else{
    # not age
    trialsRemoved = relatedTrialsWMatrix %>%
      filter((!status %in% answer$status &
                ie_flag == 1) |
               (status %in% answer$status & ie_flag == 0)) %>%
      pull(nct_id) %>% unique()
  }
  return(trialsRemoved)
}

removeTrial = function(relatedTrialsWMatrixAdd, speed) {
  if (speed == TRUE) {
    trialRemoved1 = relatedTrialsWMatrixAdd %>%
      filter(match == TRUE & ie_flag == 0)
    trialRemoved2 = relatedTrialsWMatrixAdd %>%
      filter(match == FALSE & ie_flag == 1)
    trialRemoved = trialRemoved1 %>%
      bind_rows(trialRemoved2) %>%
      pull(nct_id) %>% unique()
  } else{
    trialRemoved1 = relatedTrialsWMatrixAdd %>%
      filter(match == TRUE &
               omop_id == common_omop_id &
               mapping_score > 0.8 & ie_flag == 0)
    trialRemoved2 = relatedTrialsWMatrixAdd %>%
      filter(match == FALSE & mapping_score > 0.8 & ie_flag == 1)
    trialRemoved = trialRemoved1 %>%
      bind_rows(trialRemoved2) %>%
      pull(nct_id) %>% unique()
  }
  return(trialRemoved)
}

removeTrialsCondition = function(relatedTrialsWMatrix, answer, speed) {
  if (is.null(answer$status) &
      length(answer$time) == 0) {
    # no status or temporal
    relatedTrialsWMatrixAdd = relatedTrialsWMatrix %>%
      mutate(match = (answer$exist == "YES"))
  }
  
  if (is.null(answer$status) & length(answer$time) > 0) {
    # no status
    relatedTrialsWMatrixAdd = relatedTrialsWMatrix %>%
      mutate(match = (
        answer$exist == "YES" &
          (is.na(temporal_min) | temporal_min < answer$time$time2) &
          (is.na(temporal_max) | temporal_max > answer$time$time1)
      ))
  }
  
  if (length(answer$time) == 0 & !is.null(answer$status)) {
    # no temporal
    relatedTrialsWMatrixAdd = relatedTrialsWMatrix %>%
      mutate(match = (answer$exist == "YES" &
                        (
                          is.na(status) | status %in% answer$status
                        )))
  }
  
  if (length(answer$time) > 0 & !is.null(answer$status)) {
    relatedTrialsWMatrixAdd = relatedTrialsWMatrix %>%
      mutate(match = (
        answer$exist == "YES" &
          (is.na(temporal_min) | temporal_min < answer$time$time2) &
          (is.na(temporal_max) | temporal_max > answer$time$time1) &
          (is.na(status) | status %in% answer$status)
      ))
  }
  
  trialsRemoved = removeTrial(relatedTrialsWMatrixAdd, speed)
  return(trialsRemoved)
}

removeTrialsObservation = function(relatedTrialsWMatrix, answer, speed) {
  if(length(answer$time) == 0){
    # no temp.
    relatedTrialsWMatrixAdd = relatedTrialsWMatrix %>%
      mutate(match = (answer$exist == "YES"))
  }else{
    relatedTrialsWMatrixAdd = relatedTrialsWMatrix %>%
      mutate(match = (
        answer$exist == "YES" &
          (is.na(temporal_min) | temporal_min < answer$time$time2) &
          (is.na(temporal_max) | temporal_max > answer$time$time1)
      ))
  }
  trialsRemoved = removeTrial(relatedTrialsWMatrixAdd, speed)
  return(trialsRemoved)
}

removeTrialsMeasurement = function(relatedTrialsWMatrix, answer, speed) {
  if(!is.na(answer$value)){
    relatedTrialsWMatrixAdd = relatedTrialsWMatrix %>%
      mutate(match = ((is.na(value_min) | value_min < answer$value) &
                        (is.na(value_max) | value_max > answer$value)
      ))
    trialsRemoved = removeTrial(relatedTrialsWMatrixAdd, speed)
  }
  return(trialsRemoved)
}

removeTrialsProcedure = function(relatedTrialsWMatrix, answer, speed) {
  if(length(answer$time) == 0){
    # no temp.
    relatedTrialsWMatrixAdd = relatedTrialsWMatrix %>%
      mutate(match = (answer$exist == "YES"))
  }else{
    relatedTrialsWMatrixAdd = relatedTrialsWMatrix %>%
      mutate(match = (
        answer$exist == "YES" &
          (is.na(temporal_min) | temporal_min < answer$time$time1) &
          (is.na(temporal_max) | temporal_max > answer$time$time1)
      ))
  }
  trialsRemoved = removeTrial(relatedTrialsWMatrixAdd, speed)
  return(trialsRemoved)
}

removeTrialsDrug = function(relatedTrialsWMatrix, answer, speed) {
  if(length(answer$time) == 0){
    # no temp.
    relatedTrialsWMatrixAdd = relatedTrialsWMatrix %>%
      mutate(match = (answer$exist == "YES"))
  }else{
    relatedTrialsWMatrixAdd = relatedTrialsWMatrix %>%
      mutate(match = (
        answer$exist == "YES" &
          (is.na(temporal_min) | temporal_min < answer$time$time2) &
          (is.na(temporal_max) | temporal_max > answer$time$time1)
      ))
  }
  trialsRemoved = removeTrial(relatedTrialsWMatrixAdd, speed)
  return(trialsRemoved)
}