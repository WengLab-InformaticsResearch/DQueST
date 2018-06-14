searchByName = function(wMatrix,term){
  # search by free-text term
  wMatrix_new = NULL
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("search by free-text term finished"))
  return(wMatrix)
}

searchByAge = function(wMatrix,age){
  # search by free-text term.
  wMatrix_new = NULL
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("search by free-text term finished"))
  return(wMatrix)
}

searchByGender = function(wMatrix,gender){
  # search by gender.
  wMatrix_new = NULL
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("search by gender finished"))
  return(wMatrix)
}

searchByAll = function(wMatrix,gender,age,term){
  # search by multiple options.
  wMatrix_new = NULL
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("search by multiple options finished"))
  return(wMatrix)
}