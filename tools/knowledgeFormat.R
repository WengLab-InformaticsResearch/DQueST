removeNonValueMeasurement = function(dt){
  dt = dt[domain %in% c('condition','observation','procedure','drug') | !is.na(value_min) | !is.na(value_max)] # remove na measurements.
  return(dt)
}

removeConflictCriteria = function(dt, aggressive = TRUE){
  if(aggressive){
    # remove all conflicts.
    dim(dt)
    nonConflictOnes = dt[,.N,.(nct_id,common_omop_id)][N==1]
    dim(nonConflictOnes)
    dt1 = nonConflictOnes[dt, nomatch=0L,on = c("nct_id", "common_omop_id")]
    dt1[,N:=NULL]
    dim(dt1)
    return(dt1)
  }else{
    dt = dt[dt[,.I[which.max(mapping_score)],by=list(nct_id,common_omop_id)]$V1] # return the first max.
    return(dt)
  }
}

outputKnowledgeBase = function(knowledgeBase, file_name = 'knowledgeBase'){
  fwrite(x = knowledgeBase, file = paste0('../resource/',file_name,'.csv'),nThread = 4)
  save(knowledgeBase,file = paste0('../resource/',file_name,'.rda'))
  knowledgeBase_small = knowledgeBase[,.(nct_id,ie_flag,domain,temporal_min,temporal_max,temporal_unit,value_min,value_max,value_unit,mapping_term,mapping_score,omop_id,common_omop_id,common_omop_name)]
  fwrite(x = knowledgeBase_small, file = paste0('../resource/',file_name,'_small.csv'),nThread = 4)
  save(knowledgeBase_small,file = paste0('../resource/',file_name,'_small.rda'))
  return(0)
}