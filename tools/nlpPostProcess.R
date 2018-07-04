library(data.table)
library(dplyr)
source("conceptClusterOmop.R")

# import structured ec table.
dt = fread(file = '../resource/ec_parsed_matrix.txt',sep = "\t",
           header = F,stringsAsFactors = F,
           fill = T,showProgress = T,
           na.strings = c("no_temporal","no_value"))
colnames(dt) = c('nct_id','ie_flag','term','domain','negation','temporal','value')
# change domain to lower case.
dt[,':='(domain=tolower(domain))]
# change term to lower case.
dt[,':='(term=tolower(term))]
# change ie_flag to 1/0
dt[,':='(ie_flag=(ie_flag=='INC'))]
dt[,':='(ie_flag=as.numeric(ie_flag))]
# change negation to 1/0
dt[,':='(negation=as.numeric(negation))]
dt[,':='(ie_flag=xor(ie_flag,negation))]
dt[,':='(ie_flag=as.numeric(ie_flag))]
dt[,'negation':=NULL] # drop negation.

# import concept mapping table.
conceptMapping = fread(file = '../resource/concept_mapping_result_v2.txt',sep = "\t",header = F,fill = T,stringsAsFactors = F,data.table = T,nThread = 4)
colnames(conceptMapping) = c('term','mapping_term','mapping_score','omop_id')
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
# join tables.
setindexv(conceptMappingAncestor, "omop_id")
setindexv(conceptMapping, "omop_id")
conceptMappingWithCluster = conceptMapping[conceptMappingAncestor,on = 'omop_id',allow.cartesian = TRUE] # make sure to add allow.cartesian = TRUE

setindexv(conceptMappingWithCluster, "term")
setindexv(dt, "term")
knowledgeBase <- dt[conceptMappingWithCluster,on = 'term',allow.cartesian = TRUE] # make sure to add allow.cartesian = TRUE
fwrite(x = knowledgeBase, file = '../resource/knowledgeBase.csv',nThread = 4)
