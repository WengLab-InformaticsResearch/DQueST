# restructure dt.

reFormatParsedDt = function(dt){
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
  return(dt)
}