library(data.table)
library(dplyr)
source("conceptClusterOmop.R")
source("valueParser.R")

# import structured ec table.
dt = fread(file = '../resource/ec_parsed_matrix_parsed.csv',sep = "\t",
           header = T,stringsAsFactors = F,
           fill = T,showProgress = T,
           na.strings = c("no_temporal","no_value",'NA'))
colnames(dt) = c('cid','nct_id','ie_flag','term','domain','negation','temporal','value','temporal_min','temporal_max','temporal_unit','value_min','value_max','value_unit')
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

dt[,':='(temporal_min=as.numeric(temporal_min))]
dt[,':='(temporal_max=as.numeric(temporal_max))]
dt[,':='(value_min=as.numeric(value_min))]
dt[,':='(value_max=as.numeric(value_max))]


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
fwrite(x = knowledgeBase, file = '../resource/knowledgeBase.csv',nThread = 4)
save(knowledgeBase,file = '../resource/knowledgeBase.rda')
knowledgeBase_small = knowledgeBase[,.(nct_id,ie_flag,domain,temporal_min,temporal_max,value_min,value_max,value_unit,mapping_term,mapping_score,omop_id,common_omop_id,common_omop_name)]
fwrite(x = knowledgeBase_small, file = '../resource/knowledgeBase_small.csv',nThread = 4)
save(knowledgeBase_small,file = '../resource/knowledgeBase_small.rda')
