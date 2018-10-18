library(data.table)
library(dplyr)
library(dbplyr)

filterLowQuality = function(cm_result, mapping_threshold = 0.7){
  colnames(cm_result) = c('term','domain','mapping_term','mapping_score','omop_id')
  conceptMapping = cm_result[mapping_score > mapping_threshold]
  return(conceptMapping)
}