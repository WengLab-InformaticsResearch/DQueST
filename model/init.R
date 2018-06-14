initByCsv = function(File){
  # init by read a csv file.
  dt = NULL
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("init by Csv finished"))
  return(dt)
}

initByRd = function(rdata){
  # init by read an rda.
  dt = NULL
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("init by Rda finished"))
  return(dt)
}

initByApi = function(api){
  # init by api.
  dt = NULL
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("init by Api finished"))
  return(dt)
}

