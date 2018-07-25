# ie format
library(data.table)
library(dplyr)

removeIncOnlyTrial = function(dt){
  trialWithExc = dt[V2 %in% 'EXC'] %>% pull(V1) %>% unique()
  dt = dt[V1 %in% trialWithExc]
  return(dt)
}

reFormatParsedDt = function(dt){
  colnames(dt) = c('nct_id','ie_flag','term','domain','negation','temporal','value','temporal_min','temporal_max','temporal_unit','value_min','value_max','value_unit')
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
  dt[,'temporal':=NULL] # drop temporal
  dt[,'value':=NULL] # drop value
  
  dt[,':='(temporal_min=as.numeric(temporal_min))]
  dt[,':='(temporal_max=as.numeric(temporal_max))]
  dt[,':='(value_min=as.numeric(value_min))]
  dt[,':='(value_max=as.numeric(value_max))]
  return(dt)
}

addStatus = function(dt){
  dt[,'status':=NA]
  return(dt)
}