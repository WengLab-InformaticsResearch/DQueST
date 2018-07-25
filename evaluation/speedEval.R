# source R
rm(list = ls())
setwd('~/Projects/eqacts/app/')
file.sources = list.files(
  c("./util"),
  pattern = "*.R$",
  full.names = TRUE,
  ignore.case = TRUE
)
sapply(file.sources, source, .GlobalEnv)
# load data
post_data = load_data()
condition_fo_eval = c("Breast Cancer","Obesity","HIV Infections","Hypertension","Diabetes","Coronary Artery Disease","Depression","Alzheimer Disease","Asthma","Rheumatoid Arthritis")
condition_fo_eval_abb = c("BRCA","OBESE","HIV","HTN","DM","CAD","DEPR","AD","ATHM","RA")

# initialize
# define input
input = NULL
input$countrySelection = 'United States'
input$skip = NULL
input$radio_qa = NULL
input$status_qa = NULL
input$value_qa = NULL
input$time_qa = NULL
input$time_unit_qa = NULL
input$speed = TRUE

speed_stat_10condition = NULL
for(condition in condition_fo_eval){
  print(condition)
  input$condition = condition
  # search for initial trials.
  geoTrial = searchByGeo(geoDt = post_data$geoDt,
                         country = input$countrySelection)
  termTrial = searchByApi(trialDt = post_data$trialDt,
                          term = input$condition)
  demoTrial = termTrial
  trialSet_init = intersect(intersect(termTrial, demoTrial), geoTrial)
  
  speed_stat = tibble(seq = integer(), trial_length = integer(),iter = integer())
  
  # start qa process.
  for(iter in c(1:10)){
    print(iter)
    common_concept_id = NULL
    asked_concept_id = NULL
    wMatrix = post_data$wMatrix[nct_id %in% trialSet_init]
    trialSet = wMatrix %>% pull(nct_id) %>% unique()
    common_concept_id = findConcept(wMatrix = wMatrix,asked_concept_id = asked_concept_id)
    asked_concept_id = c(asked_concept_id,common_concept_id)
    seq = 0
    trial_length = length(trialSet)
    speed_stat_tmp = as.tbl(data.frame(seq = seq, trial_length = trial_length,iter = iter))
    speed_stat = speed_stat %>% bind_rows(speed_stat_tmp)
    # update
    while(1){
      
      if (seq < 50 & length(common_concept_id) > 0 & dim(wMatrix)[1] > 0) {
        # randomize input
        input$radio_qa = sample(c("YES","NO"),1)
        input$skip = FALSE
        if(input$radio_qa == "YES"){
          input$status_qa = NULL
          # sample value
          value_candidate_min = wMatrix[common_omop_id==common_concept_id & !is.na(value_min)] %>% pull(value_min) %>% min() %>% head(1)
          value_candidate_max = wMatrix[common_omop_id==common_concept_id & !is.na(value_max)] %>% pull(value_max) %>% max() %>% head(1)
          if(length(value_candidate_min) >0 & length(value_candidate_max) >0 & value_candidate_min < value_candidate_max){
            value_candidate_min = max(0,value_candidate_min)
            value_candidate_max = min(999,value_candidate_max)
            input$value_qa = runif(1,min = value_candidate_min,max = value_candidate_max)
          }else{
            
            input$value_qa = NULL
          }
          # sample time
          temporal_candidate_min = wMatrix[common_omop_id==common_concept_id & !is.na(temporal_min)] %>% pull(temporal_min) %>% min() %>% head(1)
          temporal_candidate_max = wMatrix[common_omop_id==common_concept_id & !is.na(temporal_max)] %>% pull(temporal_max) %>% max() %>% head(1)
          temporal_candidate_qa = wMatrix[common_omop_id==common_concept_id & !is.na(temporal_unit)] %>% pull(temporal_unit) %>% head(1)
          if(length(temporal_candidate_min) >0 & length(temporal_candidate_max) >0 & length(temporal_candidate_qa) >0 & temporal_candidate_min < temporal_candidate_max){
            temporal_candidate_min = max(0,temporal_candidate_min)
            temporal_candidate_max = min(999,temporal_candidate_max)
            input$time_qa = runif(1,min = temporal_candidate_min,max = temporal_candidate_max)
            input$time_unit_qa = temporal_candidate_qa
          }else{
            input$time_qa = NULL
            input$time_unit_qa = NULL
          }
        }else{
          input$radio_qa = NULL
          input$status_qa = NULL
          input$value_qa = NULL
          input$time_qa = NULL
          input$time_unit_qa = NULL
        }
        
        if (input$skip == TRUE) {
          answer = NULL
        } else{
          answer = formAnswer(input)
        }
        # update the wMatrix
        wMatrix_tmp = updateWMatrix(
          wMatrix = wMatrix,
          common_concept_id = common_concept_id,
          answer = answer,
          speed = input$speed
        )
        # update trial set
        nct1 = wMatrix_tmp %>% pull(nct_id) %>% unique()
        nct2 = wMatrix %>% pull(nct_id) %>% unique()
        nct3 = trialSet
        trialSet_tmp = setdiff(nct3, setdiff(nct2, nct1))
        trialSet = trialSet_tmp
        # update seq stat
        seq = seq + 1
        if(seq %% 5 == 0){
          trial_length = length(trialSet)
          speed_stat_tmp = as.tbl(data.frame(seq = seq, trial_length = trial_length,iter = iter))
          speed_stat = speed_stat %>% bind_rows(speed_stat_tmp)
          # update concept id
          wMatrix = wMatrix_tmp
          common_concept_id = findConcept(wMatrix = wMatrix,asked_concept_id = asked_concept_id)
          asked_concept_id = c(asked_concept_id,common_concept_id)
        }
        
        
      }else{
        break
      }
    }
  }
  speed_stat_10condition[[condition]] = speed_stat
}

library(ggplot2)
df = do.call("rbind", speed_stat_10condition)
df = df %>% mutate(condition = rep(condition_fo_eval_abb,each=dim(df)[1]/10))
df_plot = df %>% group_by(condition,iter) %>% mutate(orig_length = max(trial_length)) %>% mutate(remove_percentage = 1-(trial_length/orig_length))
df_plot = df_plot %>% group_by(condition,seq) %>% summarise(mean_remove_percentage = mean(remove_percentage))
p = df_plot %>% ggplot(aes(x = seq,y = mean_remove_percentage,group=factor(condition),color=factor(condition))) +
  geom_line()
p

            