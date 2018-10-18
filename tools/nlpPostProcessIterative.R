rm(list=ls())
setwd("~/Projects/DQueST//tools/")
library(data.table)
library(dplyr)
source("ieFormat.R")
source("conceptFormat.R")
source("knowledgeFormat.R")
source("filterLowQuality.R")


generateKnowledgeBase = function(cm_result, ie_result, mapping_threshold = 0.9, levels_of_separation = 2, low_count_threshold = 5, aggressive = TRUE){
  dim(cm_result)
  cm_result = filterLowQuality(cm_result = cm_result, mapping_threshold = mapping_threshold)
  dim(cm_result)
  ie_and_cm = addConceptMapping(ie_result = ie_result,cm_result = cm_result,levels_of_separation = levels_of_separation,low_count_threshold = low_count_threshold)
  knowledge_base = removeNonValueMeasurement(ie_and_cm)
  knowledge_base = removeConflictCriteria(knowledge_base,aggressive = aggressive)
  file_name = paste0('kb','_m_',mapping_threshold,'_ls_',levels_of_separation,'_c_',low_count_threshold,'_agg_',aggressive)
  outputKnowledgeBase(knowledge_base,file_name = file_name)
  print(paste0(file_name,'...finished'))
  return(0)
}

# step 0: parse value and temporal using python PyCode/ie_parser.py

# step 1: clean and format ie matrix.
ie_result = fread("../resource/information_retrieval_results_plus.txt",sep = "\t",header = F,stringsAsFactors = F,fill = T,showProgress = T,nThread = 4,na.strings = c("no_temporal","no_value",'NA'))
# save(ie_result,file = '../resource/ie_result_raw.rda')
dim(ie_result) # 5107074
ie_result = removeIncOnlyTrial(ie_result)
dim(ie_result) # 4482721
ie_result = reFormatParsedDt(ie_result)
dim(ie_result) # 4482721
ie_result = addStatus(ie_result)
dim(ie_result) # 4482721



# step 2: add omop concept.
# step 3: postprocess knowledgebase
# step 4: output knowledgebase.
cm_result = fread("../resource/concept_mapping_results.txt",sep = "\t",header = F,stringsAsFactors = F,fill = T,showProgress = T,quote = "")


mapping_threshold = c(0.9)
levels_of_separation = c(1)
low_count_threshold = c(5)
aggressive = c(TRUE)

for(m in mapping_threshold){
  for(l in levels_of_separation){
    for(c in low_count_threshold){
      for(a in aggressive){
        generateKnowledgeBase(cm_result = cm_result,ie_result = ie_result,mapping_threshold=m,levels_of_separation = l,low_count_threshold = c,aggressive = a)
      }
    }
  }
}







