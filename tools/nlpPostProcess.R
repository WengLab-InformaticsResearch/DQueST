rm(list=ls())
setwd("~/Projects/eqacts/tools/")
library(data.table)
library(dplyr)
source("ieFormat.R")
source("conceptFormat.R")
source("knowledgeFormat.R")


# step 0: parse value and temporal using python PyCode/numericParse.py

# step 1: clean and format ie matrix.
ie_result = fread("../resource/information_retrieval_results_plus.txt",sep = "\t",header = F,stringsAsFactors = F,fill = T,showProgress = T,nThread = 4,na.strings = c("no_temporal","no_value",'NA'))
dim(ie_result) # 5107074
ie_result = removeIncOnlyTrial(ie_result)
dim(ie_result) # 4482721
ie_result = reFormatParsedDt(ie_result)
dim(ie_result) # 4482721
ie_result = addStatus(ie_result)
dim(ie_result) # 4482721

# step 2: add omop concept.
cm_result = fread("../resource/concept_mapping_results.txt",sep = "\t",header = F,stringsAsFactors = F,fill = T,showProgress = T,nThread = 4)
dim(cm_result) # 616870
knowledge_base = addConceptMapping(ie_result = ie_result,cm_result = cm_result)
dim(knowledge_base) # 3673847

# step 3: postprocess knowledgebase
knowledge_base = removeNonValueMeasurement(knowledge_base)
dim(knowledge_base) # 3409717
knowledge_base = removeConflictCriteria(knowledge_base)
dim(knowledge_base) # 3105624

# step 4: output knowledgebase.
outputKnowledgeBase(knowledge_base)



# ie_result[V3=='pregnant' & V2=='INC' & V5==FALSE]
# ie_result[V3=='pregnant' & V2=='INC' & V5==TRUE]
# ie_result[V3=='pregnant' & V2=='EXC' & V5==FALSE]
# ie_result[V3=='pregnant' & V2=='EXC' & V5==TRUE]
# 
# 
# dt %>% group_by(V1) %>% filter(V2 %in% 'INC') %>% summarise(inclusion_n = n()) %>% pull(V1) %>% unique() %>% length()
# dt %>% group_by(V1) %>% filter(V2 %in% 'EXC') %>% summarise(inclusion_n = n()) %>% pull(V1) %>% unique() %>% length()


