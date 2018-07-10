library(DT)
renderTrialInfo = function(trialSet,trialDt,session){
  ouput = NULL
  data = trialDt[nct_id %in% trialSet]
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
  ouput = DT::renderDT(data, escape = FALSE,server = TRUE)
  return(ouput)
}