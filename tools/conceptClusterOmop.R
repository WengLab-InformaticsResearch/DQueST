library(data.table)
library(dplyr)
library(dbplyr)

conceptMapping = fread(file = '../resource/concept_mapping_result.txt',sep = "\t",header = F,fill = T,stringsAsFactors = F,data.table = T,nThread = 4)
colnames(conceptMapping) = c('term','mapping_term','mapping_score','omop_id')
# term is the original term
# mapping_term is the term mapped in concept_synonym table (could be non-standard)
# mapping_score is string based usagi mapping_score
# omop_id is the standard concept id

conceptCluster = function(conceptMapping,mapping_threshold = 0.8,levels_of_separation = 2,low_count_threshold = 1){
  conceptMappingHighQuality = conceptMapping[mapping_score > mapping_threshold]
  conceptMappingSum = conceptMappingHighQuality[,.(scoreSum=sum(mapping_score)),by=omop_id]
  highQualityOmopId = conceptMappingSum %>% pull(omop_id) %>% unique()
  source('ohdsiConnection.R')
  con = ohdsiConnection()
  conceptAncestor = tbl(con,'concept_ancestor')
  conceptAncestorHighQuality = conceptAncestor %>%
    filter(ancestor_concept_id %in% highQualityOmopId) %>%
    collect()
  
  conceptMappingAncestor = conceptMappingSum %>%
    left_join(conceptAncestorHighQuality,
              by = c("omop_id" = "descendant_concept_id")) %>%
    filter(max_levels_of_separation < levels_of_separation) %>%
    rename(mapping_score_sum_1 = scoreSum) %>%
    select(omop_id, mapping_score_sum_1, ancestor_concept_id) %>%
    left_join(conceptMappingSum, by = c("ancestor_concept_id" = "omop_id")) %>%
    rename(mapping_score_sum_2 = scoreSum) %>%
    select(omop_id, mapping_score_sum_1, ancestor_concept_id, mapping_score_sum_2) %>%
    group_by(omop_id) %>%
    filter((
      mapping_score_sum_1 < low_count_threshold &
        mapping_score_sum_2 == max(mapping_score_sum_2)
    ) |
      (
        mapping_score_sum_1 >= low_count_threshold &
          omop_id == ancestor_concept_id
      )
    ) %>%
    rename(common_omop_id = ancestor_concept_id) %>%
    rename(mapping_score_sum = mapping_score_sum_1) %>%
    select(omop_id, mapping_score_sum, common_omop_id)
  
  return(conceptMappingAncestor)
}

conceptMappingAncestor = conceptCluster(conceptMapping = conceptMapping)

