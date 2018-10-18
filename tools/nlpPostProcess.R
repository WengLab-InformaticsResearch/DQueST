rm(list=ls())
setwd("~/Projects/DQueST/tools/")
library(data.table)
library(dplyr)
source("ieFormat.R")
source("conceptFormat.R")
source("knowledgeFormat.R")
source("filterLowQuality.R")


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
cm_result = fread("../resource/concept_mapping_results.txt",sep = "\t",header = F,stringsAsFactors = F,fill = T,showProgress = T,quote = "")
dim(cm_result)
cm_result = filterLowQuality(cm_result = cm_result, mapping_threshold = 0.9)
dim(cm_result)
ie_and_cm = addConceptMapping(ie_result = ie_result,cm_result = cm_result,levels_of_separation = 1,low_count_threshold = 20)
dim(ie_and_cm) # 3673845

# step 3: postprocess knowledgebase
knowledge_base = removeNonValueMeasurement(ie_and_cm)
dim(knowledge_base) # 3409715
knowledge_base = removeConflictCriteria(knowledge_base)
dim(knowledge_base) # 2864820

# step 4: output knowledgebase.
outputKnowledgeBase(knowledge_base,file_name = 'kb_2864820')




