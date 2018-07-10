addBranchLogicDemographic = function(record_set, variable){
  subData = rawData %>% filter(record_it %in% record_set)
  if(variable == "age_min"){
    record_set =
  }
}


addBranchLogicConditions = function(record_set, variable){
  subData = rawData %>% filter(record_it %in% record_set)
  condition_pull = variable$condition_pull
  condition_severity = variable$condition_severity
  condition_duration = variable$condition_duration
  condition_incexc = variable$condition_incexc
  
  subDataRemove1 = subData %>% filter(condition_pull == variable$condition_pull & is.na(condition_severity) & condition_incexc == 0)
  subDataRemove1 = subData %>% filter(condition_pull == variable$condition_pull & is.na(condition_duration) & condition_incexc == 0)
  subDataRemove1 = subData %>% filter(condition_pull != variable$condition_pull & is.na(condition_severity) & condition_incexc == 0)
  
  subDataShow = subData %>%
}
