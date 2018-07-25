removeNonValueMeasurement = function(dt){
  dt = dt[domain %in% c('condition','observation','procedure','drug') | !is.na(value_min) | !is.na(value_max)] # remove na measurements.
  return(dt)
}

removeConflictCriteria = function(dt){
  dt = dt[dt[,.I[which.max(mapping_score)],by=list(nct_id,common_omop_id)]$V1] # return the first max.
  return(dt)
}

outputKnowledgeBase = function(knowledgeBase){
  fwrite(x = knowledgeBase, file = '../resource/knowledgeBase.csv',nThread = 4)
  save(knowledgeBase,file = '../resource/knowledgeBase.rda')
  knowledgeBase_small = knowledgeBase[,.(nct_id,ie_flag,domain,temporal_min,temporal_max,value_min,value_max,value_unit,mapping_term,mapping_score,omop_id,common_omop_id,common_omop_name)]
  fwrite(x = knowledgeBase_small, file = '../resource/knowledgeBase_small.csv',nThread = 4)
  save(knowledgeBase_small,file = '../resource/knowledgeBase_small.rda')
  return(0)
}