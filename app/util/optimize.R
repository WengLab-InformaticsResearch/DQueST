library(dplyr)
findConcept = function(wMatrix){
  # find best concept
  idx = NULL
  # randomly pick a concept_id
  idx = wMatrix %>% pull(common_omop_id) %>% unique() %>% sample(size = 1)
  # for test only.
  # idx = 4344898
  idx = wMatrix %>% select(id,nct_id,domain,omop_id,common_omop_id,mapping_score,cofreq) %>%
    mutate(tmp_score = cofreq * mapping_score) %>%
    group_by(common_omop_id) %>%
    summarise(es = sum(tmp_score)) %>%
    arrange(-es) %>%
    head(n=1) %>%
    pull(common_omop_id)
    
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("find best concept"))
  return(idx)
}
