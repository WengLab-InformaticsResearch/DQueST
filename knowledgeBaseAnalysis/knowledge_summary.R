library(data.table)
library(dplyr)
knowledgeBase = get(load("../resource/knowledgeBase.rda"))
cdcf = knowledgeBase %>% group_by(mapping_term,omop_id,domain) %>% summarise(frequency = n_distinct(nct_id)) %>% arrange(-frequency)
stdName = cdcf %>% group_by(omop_id,domain) %>% top_n(1) %>% select(mapping_term,omop_id,domain)
cdcf2 = knowledgeBase %>% group_by(omop_id,domain) %>% summarise(frequency = n_distinct(nct_id)) %>% arrange(-frequency)
cdcf3 = cdcf2 %>% left_join(stdName,by = c("omop_id","domain")) %>% mutate(concept_name=mapping_term,concept_id =omop_id) %>% ungroup() %>%
  select(concept_name, domain, concept_id, frequency)
write.table(file = "cdcf.csv",cdcf,sep = "\t",row.names = F,quote = F)

ncdtv = knowledgeBase %>% mutate(concept_name = mapping_term, concept_id = omop_id) %>%
  select(nct_id,concept_name,concept_id, domain,temporal_min,temporal_max,temporal_unit,value_min,value_max,value_unit)
write.table(file = "../knowledgeBaseAnalysis/ncdtv.csv",ncdtv,sep = "\t",row.names = F,quote = F)

head(ncdtv)
