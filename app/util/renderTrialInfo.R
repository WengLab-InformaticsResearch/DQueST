library(DT)
renderTrialInfo = function(trialSet,trialDt,session,number = 999999){
  ouput = NULL
  data = trialDt[nct_id %in% trialSet] %>% head(n=number)
  data[,nct_id:=paste0('<a target="_blank" href="https://clinicaltrials.gov/ct2/show/',nct_id,'">',nct_id,'</a>')]
  data[,V1:=NULL]

  # dt = DT::datatable(data = data,options = list(
  #   scrollY = 200,
  #   scroller = TRUE,
  #   escape = FALSE
  # ))
  # ouput = DT::renderDT({
  #   data
  # })
  
  # a bug: DT::datatable does not work to embed html.
  data$phase = as.factor(data$phase)
  data$status = as.factor(data$status)
  data = data[,c('nct_id','title','phase','status')]
  output = DT::renderDataTable(data, escape = FALSE,server = TRUE,filter = 'top',rownames = F,options = list(scrollX = TRUE))
  return(output)
}