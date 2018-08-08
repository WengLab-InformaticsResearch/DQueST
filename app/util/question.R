questionGet = function(wMatrix,idx){
  # generate questions list
  question = NULL
  subWMatrix = wMatrix %>% filter(common_omop_id == idx)
  domain = subWMatrix %>% pull(domain) %>% unique()
  question = questionGetDomain(subWMatrix,domain)

  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("generate question list"))
  return(question)
}

questionGetDomain = function(subWMatrix,domain){
  question = NULL
  question = switch(domain,
                    "demographic" = questionGetDemo(subWMatrix),
                    "condition" = questionGetCondition(subWMatrix),
                    "observation" = questionGetObservation(subWMatrix),
                    "measurement" = questionGetMeasurement(subWMatrix),
                    "procedure" = questionGetProcedure(subWMatrix),
                    "drug" = questionGetDrug(subWMatrix))
  return(question)
}

questionGetDemo = function(subWMatrix){
  question = NULL
  question$domain = "demographic"
  question$name = subWMatrix %>% pull(common_omop_name) %>% unique()
  question$s = paste("What is your ",question$name)
  status = subWMatrix %>% filter(!is.na(status)) %>% pull(status) %>% unique()
  # return logical(0) if empty array in status.
  
  if(length(status)>0){
    question$s_choice = status
    question$s_numeric = FALSE
  }else{
    question$s_choice = NA
    question$s_numeric = TRUE
  }
  # no temporal for demographic
  return(question)
}

questionGetCondition = function(subWMatrix){
  question = NULL
  question$domain = "condition"
  question$name = subWMatrix %>% pull(common_omop_name) %>% unique()
  question$s1 = paste("Do you have condition: ",question$name)
  question$s1_choice = c("YES","NO")
  status = subWMatrix %>% filter(!is.na(status)) %>% pull(status) %>% unique()
  
  # temporal
  question$s2 = paste("How long since you latest diagnoisis of the condition ? (answer = 0 if you are experiencing it now.)")
  question$s2_numeric = TRUE
  
  question$s3 = paste("How long since your initial diagnosis of the condition ?")
  question$s3_numeric = TRUE
  
  # if(length(status)>0){
  #   question$s3_choice = c(status)
  #   question$s3 = paste("Check your condition status")
  # }else{
  #   question$s3 = NA
  #   question$s3_choice = NA
  # }
  # no numeric for condition
  return(question)
}

questionGetProcedure = function(subWMatrix){
  question = NULL
  question$domain = "procedure"
  question$name = subWMatrix %>% pull(common_omop_name) %>% unique()
  question$s1 = paste("Do you have procedure: ",question$name)
  question$s1_choice = c("YES","NO")

  # temporal
  question$s2 = paste("How long since you have had the latest procedure ?")
  question$s2_numeric = TRUE
  
  # no numeric and status for procedure.
  return(question)
}

questionGetMeasurement = function(subWMatrix){
  question = NULL
  question$domain = "measurement"
  question$name = subWMatrix %>% pull(common_omop_name) %>% unique()
  question$s1 = paste("What is the value of your measurement: ",question$name)
  question$s1_numeric = TRUE
  
  # no temporal and status for procedure.
  return(question)
}

questionGetDrug = function(subWMatrix){
  question = NULL
  question$domain = "drug"
  question$name = subWMatrix %>% pull(common_omop_name) %>% unique()
  question$s1 = paste("Do you have medicine: ",question$name)
  question$s1_choice = c("YES","NO")
  
  # temporal
  question$s2 = paste("How long since you latest taken of the medicine ? ")
  question$s2_numeric = TRUE
  
  question$s3 = paste("How long since your initial taken of the medicine ?")
  question$s3_numeric = TRUE
  
  # no numeric and status for procedure.
  return(question)
}

questionGetObservation = function(subWMatrix){
  # observation is very difficult to parse.
  # therefore, do not implement.
  question = NULL
  question$domain = "observation"
  question$name = subWMatrix %>% pull(common_omop_name) %>% unique()
  question$s1 = paste("Do you have observation: ",question$name)
  question$s1_choice = c("YES","NO")
  
  # temporal
  question$s2 = paste("How long since you latest observation of the event ? (answer = 0 if you are observing it now.)")
  question$s2_numeric = TRUE
  
  question$s3 = paste("How long since your initial observation of the event ?")
  question$s3_numeric = TRUE
  return(question)
}

