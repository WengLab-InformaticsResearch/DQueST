library(data.table)
library(dplyr)
library(dbplyr)

addConceptMapping = function(ie_result,cm_result,blacklist = "../resource/blacklist.csv"){
  ###
  # ie_result: post-processed ner results.
  # cm_results: concept_mapping results.
  # blacklist: a curated blacklist storing concept do not want to be used to generate questions.
  # return: 
  ###
  
  # import concept mapping table.
  conceptMapping = cm_result
  colnames(conceptMapping) = c('term','domain','mapping_term','mapping_score','omop_id')
  
  # filter out some manually curated high level abstracted concept id.
  HighLevelConceptId = read.csv(blacklist,header = F)
  HighLevelConceptId = HighLevelConceptId$V1
  
  # concept clustering.
  # conceptMappingAncestor = conceptCluster(conceptMapping = conceptMapping,
  #                                         mapping_threshold = 0.7,
  #                                         levels_of_separation = 1,
  #                                         low_count_threshold = 5,
  #                                         abstract_id = HighLevelConceptId)
  
  # do clustering iteratively. 
  # It takes reasonable time to finish.
  conceptMappingAncestor = conceptClusterIterative(conceptMapping = conceptMapping,
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
  conceptMappingWithCluster = conceptMapping[conceptMappingAncestorName,on = 'omop_id',allow.cartesian = TRUE,nomatch=0] # make sure to add allow.cartesian = TRUE
  conceptMappingWithCluster[,':='(domain=tolower(domain))]
  setindexv(conceptMappingWithCluster, c("term","domain"))
  setindexv(ie_result, c("term","domain"))
  # note: mapping_term could be different from common omop name even for the same omop_id because the mapping_term could be non-standard.
  knowledgeBase <- ie_result[conceptMappingWithCluster,on = c('term','domain'),allow.cartesian = TRUE, nomatch=0] # make sure to add allow.cartesian = TRUE
  return(knowledgeBase)
}



conceptCluster = function(conceptMapping,mapping_threshold = 0.7,levels_of_separation = 2,low_count_threshold = 5, abstract_id = HighLevelConceptId){
  ###
  # mapping_threshold: remove low quality mapping concepts.
  # levels_of_separation: min_level_sperate < levels_of_separation
  # low_count_threshold: if mapping score > low count threshold, no mapping. change low_count_threshold = 0 to avoid any clustering.
  # abstract_id: abstract id should be removed from question list. no clustering.
  # return: tibble(omop_id,mapping_score_sum,common_omop_id)
  # old version deprecated.
  ###
  
  conceptMappingHighQuality = conceptMapping[mapping_score > mapping_threshold]
  conceptMappingSum = conceptMappingHighQuality[,.(scoreSum=sum(mapping_score)),by=omop_id]
  highQualityOmopId = conceptMappingSum %>% pull(omop_id) %>% unique()
  highQualityOmopId = highQualityOmopId[!highQualityOmopId %in% abstract_id]
  
  
  source('../resource/ohdsiConnection.R')
  
  con = ohdsiConnection()
  conceptAncestor = tbl(con,'concept_ancestor')
  conceptAncestorHighQuality = conceptAncestor %>%
    filter(ancestor_concept_id %in% highQualityOmopId) %>%
    collect()

  conceptMappingAncestor = conceptMappingSum %>%
    left_join(conceptAncestorHighQuality,
              by = c("omop_id" = "descendant_concept_id")) %>%
    filter(min_levels_of_separation < levels_of_separation) %>%
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

getConceptName = function(conceptIdTbl){
  ###
  # conceptIdTbl: a data table with common_omop_id column.
  # return a data table with common_omop_name added.
  ##
  source('../resource/ohdsiConnection.R')
  con = ohdsiConnection()
  conceptName = tbl(con,'concept')
  conceptId = conceptIdTbl %>% pull(common_omop_id) %>% unique()
  conceptNameTbl = conceptName %>%
    filter(concept_id %in% conceptId) %>%
    rename(common_omop_name = concept_name) %>%
    select(concept_id,common_omop_name) %>%
    collect()
  conceptNameTbl = conceptIdTbl %>% left_join(conceptNameTbl,by=c('common_omop_id'='concept_id'))
  return(conceptNameTbl)
}

conceptClusterIterative = function(conceptMapping,mapping_threshold = 0.7,levels_of_separation = 2,low_count_threshold = 5, abstract_id = HighLevelConceptId){
  ###
  # mapping_threshold: remove low quality mapping concepts.
  # levels_of_separation: min_level_sperate < levels_of_separation
  # low_count_threshold: if mapping score > low count threshold, no mapping. change low_count_threshold = 0 to avoid any clustering.
  # abstract_id: abstract id should be removed from question list. no clustering.
  # return: tibble(omop_id,common_omop_id)
  ###
  
  conceptMappingHighQuality = conceptMapping[mapping_score > mapping_threshold]
  conceptMappingSum = conceptMappingHighQuality[,.(scoreSum=sum(mapping_score)),by=omop_id]
  highQualityOmopId = conceptMappingSum %>% pull(omop_id) %>% unique()
  highQualityOmopId = highQualityOmopId[!highQualityOmopId %in% abstract_id]
  
  
  source('../resource/ohdsiConnection.R')
  con = ohdsiConnection()
  conceptAncestor = tbl(con,'concept_ancestor')
  conceptAncestorHighQuality = conceptAncestor %>%
    filter(ancestor_concept_id %in% highQualityOmopId) %>%
    collect()
  
  # remove concept in blacklist
  conceptMappingSum = conceptMappingSum %>%
    filter(omop_id %in% conceptAncestorHighQuality$ancestor_concept_id)
  
  # sortted  by mapping score
  # d->a
  # two score for d, before and after for sortting purpose.
  # two score for a, before and after for target findding purpose
  conceptMappingSortted = conceptMappingSum %>%
    left_join(conceptAncestorHighQuality,
              by = c("omop_id" = "descendant_concept_id")) %>%
    filter(min_levels_of_separation < levels_of_separation) %>%
    rename(mapping_score_sum1_before = scoreSum) %>%
    mutate(mapping_score_sum1_after = mapping_score_sum1_before) %>%
    select(omop_id, mapping_score_sum1_before, mapping_score_sum1_after, ancestor_concept_id) %>%
    left_join(conceptMappingSum, by = c("ancestor_concept_id" = "omop_id")) %>%
    rename(mapping_score_sum2_before = scoreSum) %>%
    mutate(mapping_score_sum2_after = mapping_score_sum2_before) %>%
    select(omop_id, mapping_score_sum1_before, mapping_score_sum1_after, ancestor_concept_id, mapping_score_sum2_before, mapping_score_sum2_after) %>%
    arrange(mapping_score_sum1_after,omop_id)
  
  # working matrix init.
  conceptMappingSorttedWorking = conceptMappingSortted
  conceptMappingAncestor = tibble(omop_id = integer(), common_omop_id = integer()) 
  d_list = conceptMappingSorttedWorking$omop_id
  
  while(length(d_list) > 0){
    # select d with lowest mapping score
    d = d_list[1]
    
    #looking for clustering target for d.
    # if less than count, find target with maximal mapping score
    # o/w target is itself.
    target = conceptMappingSorttedWorking %>% filter(omop_id == d) %>%
      filter((
        mapping_score_sum1_before < low_count_threshold &
          mapping_score_sum2_after == max(mapping_score_sum2_after)
      ) |
        (
          mapping_score_sum1_before >= low_count_threshold &
            omop_id == ancestor_concept_id
        )
      ) %>%
      rename(common_omop_id = ancestor_concept_id)
    
    # if tie occurs, pick the first one. 
    # bug fixed by Cong Liu
    # 20181016
    if(dim(target)[1] > 1){
      target = target[1,]
    }
    
    
    
    # update conceptMappingSorttedWorking table 
    # remove remove d from conceptMappingSorttedWorking
    # resort omop_id

    if(target$omop_id != target$common_omop_id){
      # update sum 2 if d -> a
      conceptMappingSorttedWorking = conceptMappingSorttedWorking %>% 
        mutate(mapping_score_sum2_after = ifelse(ancestor_concept_id == target$common_omop_id,mapping_score_sum2_after+target$mapping_score_sum1_after,mapping_score_sum2_after)) %>%
        mutate(mapping_score_sum1_after = ifelse(omop_id == target$common_omop_id,mapping_score_sum1_after+target$mapping_score_sum1_after,mapping_score_sum1_after)) %>%
        filter(!omop_id %in% d) %>% 
        arrange(mapping_score_sum1_after,omop_id)
    }else{
      # only remove
      conceptMappingSorttedWorking = conceptMappingSorttedWorking %>% 
        filter(!omop_id %in% d)
    }

    # record mapping result for d.
    conceptMappingAncestor = bind_rows(conceptMappingAncestor, tibble(omop_id=d,common_omop_id=target$common_omop_id))
    
    # update d_list
    d_list = conceptMappingSorttedWorking$omop_id
  }
  return(conceptMappingAncestor)
}

