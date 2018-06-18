library(DT)
renderTrialInfo = function(wMatrix,session){
  ouput = NULL
  dt = DT::datatable(data = wMatrix,options = list(
    deferRender = TRUE,
    scrollY = 200,
    scroller = TRUE
  ))
  ouput = DT::renderDT({
    dt
  })
  return(ouput)
}