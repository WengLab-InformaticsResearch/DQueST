library(dplyr)
findConcept = function(wMatrix){
  # find best concept
  idx = NULL
  # randomly pick a concept_id
  idx = wMatrix %>% pull(common_omop_id) %>% unique() %>% sample(size = 1)
  # for test only.
  idx = 4344899
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("find best concept"))
  return(idx)
}