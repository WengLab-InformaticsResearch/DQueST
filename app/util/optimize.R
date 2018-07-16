library(dplyr)
findConcept = function(wMatrix){
  print(paste0('trial number: ',wMatrix %>% pull(nct_id) %>% unique() %>% length()))
  # find best concept
  idx = NULL
  # randomly pick a concept_id
  # idx = wMatrix %>% pull(common_omop_id) %>% unique() %>% sample(size = 1)
  # for test only.
  # idx = 4344898
  # cofreq = 1 # not implemented yet.
  
  idx_tbl = wMatrix %>% select(nct_id,domain,omop_id,common_omop_id,mapping_score) %>%
    filter(mapping_score > 0.7) %>%
    mutate(tmp_score = mapping_score) %>%
    group_by(common_omop_id) %>%
    summarise(es = sum(tmp_score)) %>%
    arrange(-es) %>%
    head(n=1)
  
  idx= idx_tbl %>% pull(common_omop_id)
  score = idx_tbl %>% pull(es)
  print(paste0('idx:',idx))
  print(paste0('score:',score))
  print(paste0('trial number:',wMatrix %>% filter(common_omop_id %in% idx) %>% pull(nct_id) %>% unique() %>% length()))
  
  
    
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("find best concept"))
  return(idx)
}
