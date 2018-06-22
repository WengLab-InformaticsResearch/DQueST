searchByLocation = function(localDt, state, country){
  localDt = NULL
  tryCatch(expr = {stop("currently search by location is not supported")},
           error = function(e) e,
           finally = print("search by location finished"))
  return(localDt)
}
searchByName = function(demoDt,term){
  # search by free-text term
  demoDt = demoDt # for test only.
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("search by free-text term finished"))
  return(demoDt)
}

searchByAge = function(demoDt,age){
  # search by free-text term.
  demoDt = demoDt[is.na(maximum_age1) | maximum_age1 >= age]
  demoDt = demoDt[is.na(minimum_age1) | minimum_age1 <= age]
  
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("search by free-text term finished"))
  return(demoDt)
}

searchByGender = function(demoDt,g){
  # search by gender.
  demoDt = demoDt[is.na(gender) | gender == g]
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("search by gender finished"))
  return(demoDt)
}

searchByCtrl = function(demoDt,ctrl){
  # search by healthy_volunteers
  demoDt = demoDt[is.na(healthy_volunteers) | healthy_volunteers == ctrl]
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("search by health volunteer finished"))
  return(demoDt)
}

searchByAll = function(demoDt,gender,age,term,ctrl){
  # search by multiple options.
  if(!ctrl %in% c("Yes","No")){
    stop("Ctrl should be Yes or No")
  }else{
    demoDt = searchByCtrl(demoDt,ctrl)
  }
  if(!is.numeric(age)){
    stop("Age should be numeric")
  }else{
    demoDt = searchByAge(demoDt,age)
  }
  if(!gender %in% c("Male","Female")){
    stop("Gender should be Male or Female")
  }else{
    demoDt = searchByGender(demoDt,gender)
  }
  if(!is.character(term)){
    stop("Term should be char")
  }
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("search by multiple options finished"))
  return(demoDt)
}