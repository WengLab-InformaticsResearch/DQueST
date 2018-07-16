source("conceptClusterOmop.R")

addConceptMapping = function(dt){
  # import concept mapping table.
  conceptMapping = fread(file = '../resource/concept_mapping_result_v3.txt',sep = "\t",header = F,fill = T,stringsAsFactors = F,data.table = T,nThread = 4)
  colnames(conceptMapping) = c('term','domain','mapping_term','mapping_score','omop_id')
  # filter out some manually curated high level abstracted concept id.
  HighLevelConceptId = read.csv("../resource/high_level_id.csv",header = F)
  HighLevelConceptId = HighLevelConceptId$V1
  # concept clustering.
  conceptMappingAncestor = conceptCluster(conceptMapping = conceptMapping,
                                          mapping_threshold = 0.7,
                                          levels_of_separation = 1,
                                          low_count_threshold = 5,
                                          abstract_id = HighLevelConceptId)
  conceptMappingAncestor = as.data.table(conceptMappingAncestor)
  conceptMappingAncestorName = getConceptName(conceptIdTbl = conceptMappingAncestor)
  conceptMappingAncestorName = as.data.table(conceptMappingAncestorName)
  # join tables.
  setindexv(conceptMappingAncestorName, "omop_id")
  setindexv(conceptMapping, "omop_id")
  conceptMappingWithCluster = conceptMapping[conceptMappingAncestorName,on = 'omop_id',allow.cartesian = TRUE] # make sure to add allow.cartesian = TRUE
  conceptMappingWithCluster[,':='(domain=tolower(domain))]
  setindexv(conceptMappingWithCluster, c("term","domain"))
  setindexv(dt, c("term","domain"))
  knowledgeBase <- dt[conceptMappingWithCluster,on = c('term','domain'),allow.cartesian = TRUE] # make sure to add allow.cartesian = TRUE
  return(knowledgeBase)
}