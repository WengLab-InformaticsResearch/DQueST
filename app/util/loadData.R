load_data = function() {
  initData = NULL
  tryCatch(
    expr = {
      # wMatrix = wMatrixInitByCsv(File = "model/mock_w_matrix.csv") # for test only.
      wMatrix = wMatrixInitByRd(rda = "model/knowledgeBase_small.rda") # for test only.
      titleDt = trialDtInitByCsv(File = 'model/titleDt.csv')
      demoDt = demoDtInitByCsv(File = "model/demoDt.csv")
      # conditionDt = conditionDtInitByCsv(File = "model/conditionDf.csv")
      geoDt = geoDtInitByCsv(File = "model/geoDf_py.csv")
      countryName = geoDt %>% pull(country) %>% unique()
      stateName = geoDt %>% pull(state) %>% unique()
      trialDt = titleDt # the information want to render
    },
    error = function(e)
      e,
    finally = print("load Data finished")
  )
  initData = list(
    wMatrix = wMatrix,
    trialDt = titleDt,
    demoDt = demoDt,
    geoDt = geoDt,
    countryName = countryName,
    stateName = stateName
  )
  return(initData)
}
