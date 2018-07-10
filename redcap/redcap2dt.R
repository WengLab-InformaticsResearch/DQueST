rm(list=ls())

library(dplyr)
library(tidyverse)

rawData = read.csv("RecruitmentOptimizat_DATA_2018-07-05_0832.csv",header = TRUE,na.strings = c("NA",""))
conditionMatrix = rawData %>% filter(redcap_repeat_instrument == "conditions") %>% 
  select(record_id,redcap_repeat_instrument,redcap_repeat_instance,condition_pull,condition_severity,condition_duration,condition_incexc)
medicationMatrix = rawData %>% filter(redcap_repeat_instrument == "medications") %>%
  select(record_id,redcap_repeat_instrument,redcap_repeat_instance,med_pull,med_frequency,med_allergy,med_incexc)
labTestMatrix = rawData %>% filter(redcap_repeat_instrument == "lab_measures") %>%
  select(record_id,redcap_repeat_instrument,redcap_repeat_instance,lab_test,lab_normal,lab_highlow,lab_incexc)
ProcedureMatrix = rawData %>% filter(redcap_repeat_instrument == "procedures") %>%
  select(record_id,redcap_repeat_instrument,redcap_repeat_instance,proc_pull,proc_status,proc_incexc)
noncodedMatrix = rawData %>% filter(redcap_repeat_instrument == "noncoded_eligibility_criteria") %>%
  select(record_id,redcap_repeat_instrument,redcap_repeat_instance,noncoded_instruct,noncoded_criteria,noncoded_incexc)
demoMatrix = rawData %>% filter(is.na(redcap_repeat_instrument)) %>%
  mutate(redcap_repeat_instrument == "demographics",redcap_repeat_instance = 1) %>%
  select(record_id,redcap_repeat_instrument,redcap_repeat_instance,age_min,age_max,gender_check___1,gender_check___2,gender_check___3,healthy_volunteer)
studyInfoMatrix = rawData %>% filter(is.na(redcap_repeat_instrument)) %>%
  mutate(redcap_repeat_instrument == "study_information",redcap_repeat_instance = 1) %>%
  select(record_id,redcap_repeat_instrument,redcap_repeat_instance,recruiting_now)

# test for condition
MatrixCondition = conditionMatrix %>% 
  select(record_id, condition_pull,condition_incexc,condition_severity,condition_duration)

# possible answers.  
conditionExistAnswer = c(0,1)
conditionSeverityAnswer = c(1,2,3)
conditionDurationAnswer = c(1,2,3,4,5,6)
conditionAnswer = as_tibble(data.frame(Exist=numeric(),Severity=numeric(),Duration=numeric()))
for(i in conditionExistAnswer){
  for(j in conditionSeverityAnswer){
    for(k in conditionDurationAnswer){
      row = as_tibble(data.frame(Exist=i,Severity=j,Duration=k))
      conditionAnswer = conditionAnswer %>% bind_rows(row)
    }
  }
}

# init term.and working matrix.
initWMatrixCondition = MatrixCondition
initCondition = initWMatrixCondition %>% group_by(condition_pull) %>% summarise(num_of_nct_id = n()) %>% arrange(-num_of_nct_id) %>% head(n=1) %>% pull(condition_pull)
initWTerm = initCondition
initSeq = 0
# iter possible answer to add logic
addBranchLogic = function(wTerm,
                          wMatrixCondition,
                          seq) {
  for (i in 1:dim(conditionAnswer)[1]) {
    # 1. wTerm appear in this nct.
    removed_nct = wMatrixCondition %>% filter(condition_pull == wTerm)
    # 2. ie_flag = 0 & match == TRUE
    removed_nct_0 = removed_nct %>% filter(condition_incexc == 0)
    removed_nct_01 = removed_nct_0 %>% filter(
      conditionAnswer$Exist[i] == 1 &
        is.na(condition_severity) & is.na(condition_duration)
    )
    removed_nct_02 = removed_nct_0 %>% filter(
      conditionAnswer$Exist[i] == 1 &
        is.na(condition_severity) &
        condition_duration == conditionAnswer$Duration[i]
    )
    removed_nct_03 = removed_nct_0 %>% filter(
      conditionAnswer$Exist[i] == 1 &
        condition_severity == conditionAnswer$Severity[i] &
        is.na(condition_duration)
    )
    removed_nct_04 = removed_nct_0 %>% filter(
      conditionAnswer$Exist[i] == 1 &
        condition_severity == conditionAnswer$Severity[i] &
        condition_duration == conditionAnswer$Duration[i]
    )
    # 3. ie_flag = 1 & match == FALSE
    removed_nct_1 = removed_nct %>% filter(condition_incexc == 1)
    removed_nct_11 = removed_nct_1 %>% filter(conditionAnswer$Exist[i] == 0)
    removed_nct_12 = removed_nct_1 %>% filter(conditionAnswer$Exist[i] == 1 &
                                                ((
                                                  !is.na(condition_severity) &
                                                    condition_severity != conditionAnswer$Severity[i]
                                                )
                                                |
                                                  (
                                                    !is.na(condition_duration) &
                                                      condition_duration != conditionAnswer$Duration[i]
                                                  )
                                                ))
    removeRecordId = bind_rows(
      removed_nct_01,
      removed_nct_02,
      removed_nct_03,
      removed_nct_04,
      removed_nct_11,
      removed_nct_12
    ) %>% pull(record_id) %>% unique()
    newWMatrixCondition = wMatrixCondition %>% filter(!record_id %in% removeRecordId)
    newWMatrixCondition = newWMatrixCondition %>% filter(!condition_pull %in% wTerm)
    newCondition = newWMatrixCondition %>% group_by(condition_pull) %>% summarise(num_of_nct_id = n()) %>% arrange(-num_of_nct_id) %>% head(n = 1) %>% pull(condition_pull)
    newTerm = newCondition
    newseq = seq + 1
    print(newseq)
    row = tibble(
      seq = newseq,
      descendant = newTerm,
      ancestor = wTerm,
      exist = conditionAnswer$Exist[i],
      severity = conditionAnswer$Severity[i],
      duration = conditionAnswer$Duration[i]
    )
    branch_logic <<- branch_logic %>% bind_rows(row)
    if (dim(newWMatrixCondition)[1] > 0) {
      addBranchLogic(wTerm = newTerm,
                     wMatrixCondition = newWMatrixCondition,
                     seq = newseq)
    } else{
      row = tibble(
        seq = newseq,
        descendant = "leaf",
        ancestor = wTerm,
        exist = conditionAnswer$Exist[i],
        severity = conditionAnswer$Severity[i],
        duration = conditionAnswer$Duration[i]
      )
      # print(seq)
      # print(wTerm)
      # print(conditionAnswer$Exist[i])
      # print(conditionAnswer$Severity[i])
      # print(conditionAnswer$Duration[i])
      # print(row)
      branch_logic <<- branch_logic %>% bind_rows(row)
    }
  }
  return(0)
}
# init global branch_logic tree.
branch_logic <<- tibble(seq = initSeq, descendant = initWTerm, ancestor = 'root', exist = NA, severity = NA, duration = NA)
addBranchLogic(wTerm = initWTerm,wMatrixCondition = initWMatrixCondition,seq = initSeq)
branch_logic %>% print(n = 1000)
