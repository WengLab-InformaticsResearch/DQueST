searchByAge = function(demoDt,age){
  if(dim(demoDt)[1] < 1){
    return(demoDt)
  }
  if(is.null(age)){
    return(demoDt)
  }
  if(age < 0 | age > 999){
    return(demoDt)
  }
  # search by free-text term.
  demoDt = demoDt[is.na(maximum_age1) | maximum_age1 >= age]
  demoDt = demoDt[is.na(minimum_age1) | minimum_age1 <= age]
  
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("search by age finished"))
  return(demoDt)
}

searchByGender = function(demoDt,g){
  if(dim(demoDt)[1] < 1){
    return(demoDt)
  }
  # search by gender.
  if(is.null(g)){
    return(demoDt)
  }
  if(g == 'All'){
    return(demoDt)
  }
  demoDt = demoDt[is.na(gender) | gender == g]
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("search by gender finished"))
  return(demoDt)
}

searchByCtrl = function(demoDt,ctrl){
  if(dim(demoDt)[1] < 1){
    return(demoDt)
  }
  # search by healthy_volunteers
  if(is.null(ctrl)){
    return(demoDt)
  }
  if(ctrl == FALSE){
    return(demoDt)
  }
  demoDt = demoDt[is.na(healthy_volunteers) | healthy_volunteers == "Yes"]
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("search by health volunteer finished"))
  return(demoDt)
}

searchByDemo = function(demoDt,gender,age,ctrl){
  if(dim(demoDt)[1] < 1){
    return(demoDt)
  }
  # search by multiple options.
  demoDt = searchByCtrl(demoDt,ctrl)
  demoDt = searchByGender(demoDt,gender)
  demoDt = searchByAge(demoDt,age)
  
  trialSet = demoDt %>%
    pull(nct_id) %>% unique()
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("search by demo finished"))
  trialSet = demoDt %>% pull(nct_id) %>% unique()
  return(trialSet)
}

searchByTerm = function(conditionDt,term){
  if(dim(conditionDt)[1] < 1){
    return(conditionDt)
  }
  # search by free-text term
  trialSet = conditionDt[condition %like% term] %>%
    pull(nct_id) %>% unique()
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("search by condition finished"))
  return(trialSet)
}

searchByCountry = function(geoDt,c){
  if(dim(geoDt)[1] < 1){
    return(geoDt)
  }
  if(is.null(c)){
    return(geoDt)
  }
  # search by free-text term.
  geoDt = geoDt[is.na(country) | country %in% c]

  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("search by country finished"))
  return(geoDt)
}
searchByState = function(geoDt,s){
  if(dim(geoDt)[1] < 1){
    return(geoDt)
  }
  if(is.null(s)){
    return(geoDt)
  }
  # search by free-text term.
  geoDt = geoDt[is.na(state) | state %in% s]
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("search by state finished"))
  return(geoDt)
}
searchByGeo = function(geoDt,country,state){
  if(dim(geoDt)[1] < 1){
    return(geoDt)
  }
  geoDt = searchByCountry(geoDt,country)
  geoDt = searchByState(geoDt,state)
  trialSet = geoDt %>% pull(nct_id) %>% unique()
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("search by geo finished"))
  return(trialSet)
}