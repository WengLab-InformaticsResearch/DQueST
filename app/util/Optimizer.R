findConcept = function(wMatrix,asked_concept_id){
  # print(paste0('--trial number: ',wMatrix %>% pull(nct_id) %>% unique() %>% length()))
  # find best concept
  idx = NULL
  # randomly pick a concept_id
  # idx = wMatrix %>% pull(common_omop_id) %>% unique() %>% sample(size = 1)
  # for test only.
  # idx = 4344898
  # cofreq = 1 # not implemented yet.
  
  wMatrix_new = wMatrix %>% filter(!common_omop_id %in% asked_concept_id)
  idx_tbl = wMatrix_new %>% select(nct_id,domain,omop_id,common_omop_id,mapping_score) %>%
    filter(mapping_score > 0.7) %>%
    mutate(tmp_score = mapping_score) %>%
    group_by(common_omop_id) %>%
    summarise(es = sum(tmp_score)) %>%
    arrange(-es) %>%
    head(n=1)
  
  idx= idx_tbl %>% pull(common_omop_id)
  score = idx_tbl %>% pull(es)
  return(idx)
}
