findConcept = function(wMatrix){
  # find best concept
  common_concept_id = NULL
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("find best concept"))
  return(common_concept_id)
}