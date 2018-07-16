library(data.table)
library(dplyr)
source("removeConflictCriteria.R")
source("reFormatParsedDt.R")
source("removeIncOnlyTrial.R")
# import structured ec table.
# ec table is provided by Chi
# and then parsed by PyCode
dt = fread(file = '../resource/ec_parsed_matrix_v3.txt',sep = "\t",nThread = 4,
           header = F,stringsAsFactors = F,
           fill = T,showProgress = T,
           na.strings = c("no_temporal","no_value",'NA'))
# dt %>% pull(V1) %>% unique() %>% length()
# dt = removeIncOnlyTrial(dt)
# dt %>% pull(V1) %>% unique() %>% length()
dt[V3=='pregnant' & V2=='INC' & V5==FALSE]
dt[V3=='pregnant' & V2=='INC' & V5==TRUE]
dt[V3=='pregnant' & V2=='EXC' & V5==FALSE]
dt[V3=='pregnant' & V2=='EXC' & V5==TRUE]

# colnames(dt) = c('cid','nct_id','ie_flag','term','domain','negation','temporal','value','temporal_min','temporal_max','temporal_unit','value_min','value_max','value_unit')

dt %>% group_by(V1) %>% filter(V2 %in% 'INC') %>% summarise(inclusion_n = n()) %>% pull(V1) %>% unique() %>% length()
dt %>% group_by(V1) %>% filter(V2 %in% 'EXC') %>% summarise(inclusion_n = n()) %>% pull(V1) %>% unique() %>% length()

dt = removeIncOnlyTrial(dt)
dt = reFormatParsedDt(dt)
dt = addConceptMapping(dt)
dt = removeConflictCriteria(dt)

fwrite(x = knowledgeBase, file = '../resource/knowledgeBase.csv',nThread = 4)
save(knowledgeBase,file = '../resource/knowledgeBase.rda')
knowledgeBase_small = knowledgeBase[,.(nct_id,ie_flag,domain,temporal_min,temporal_max,value_min,value_max,value_unit,mapping_term,mapping_score,omop_id,common_omop_id,common_omop_name)]
fwrite(x = knowledgeBase_small, file = '../resource/knowledgeBase_small.csv',nThread = 4)
save(knowledgeBase_small,file = '../resource/knowledgeBase_small.rda')
