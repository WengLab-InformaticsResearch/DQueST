load_data = function() {
  initData = NULL
  tryCatch(
    expr = {
      # wMatrix = wMatrixInitByCsv(File = "model/mock_w_matrix.csv") # for test only.
      wMatrix = wMatrixInitByRd(rda = "model/knowledgeBase_small.rda")
      titleDt = trialDtInitByCsv(File = 'model/titleDt.csv')
      demoDt = demoDtInitByCsv(File = "model/demoDt.csv")
      conditionDt = conditionDtInitByCsv(File = "model/conditionDf.csv")
      conditionDt100 = conditionDt %>% group_by(condition) %>% dplyr::summarise(countN = n()) %>% arrange(-countN) %>% head(100)
      geoDt = geoDtInitByCsv(File = "model/geoDf_py.csv")
      countryName = geoDt %>% pull(country) %>% unique()
      stateName = geoDt %>% pull(state) %>% unique()
      trialDt = titleDt # the information want to render
      
      initData = list(
        wMatrix = wMatrix,
        trialDt = titleDt,
        demoDt = demoDt,
        geoDt = geoDt,
        countryName = countryName,
        stateName = stateName,
        conditionChoices = conditionDt100 %>% pull(condition) %>% unique()
      )
    },
    error = function(e)
      e,
    finally = print("load Data finished")
  )
  
  return(initData)
}
