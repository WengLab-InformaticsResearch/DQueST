getApi = function(dbname, host, port, user, password) {
  drv = dbDriver('PostgreSQL')
  con = dbConnect(
    drv = drv,
    dbname = dbname,
    host = host,
    port = port,
    user = user,
    password = password
  )
  return(con)
}

getCache = function(con) {
  aact_condition = dbGetQuery(con,
                              "SELECT name,COUNT(*) AS count FROM conditions GROUP BY name ORDER BY count DESC")
  condition_keyword = aact_condition$name[1:100]
  
  facilitity_search = dbGetQuery(con, "SELECT DISTINCT country, state FROM facilities")
  return(
    list(
      CONDITION_KEY_WORD_TOP100 = condition_keyword,
      COUNTRY_STATE_TABLE = data.table(facilitity_search)
    )
  )
}

getTrialsBySearch = function(con,
                             condition = c('breast cancer', 'liver cancer'),
                             age = 18,
                             gender = 'Male',
                             country = 'United States',
                             state = c('New York', 'New Jersey'),
                             ctrl = TRUE) {
  ### init trials ###
  final_trials = NULL
  condition_trials = NULL
  gender_trials = NULL
  age_trials = NULL
  country_trials = NULL
  state_trials = NULL
  ctrl_trials = NULL
  
  ### get condition defined trials ###
  # condition = c('breast cancer', 'liver cancer')
  if (!is.null(condition)) {
    condition = tolower(condition)
    condition = paste0("'", condition, "'")
    condition = paste0(condition, collapse = ",")
    condition = paste0("(", condition, ")")
    condition_trials = dbGetQuery(
      con,
      paste0(
        "SELECT DISTINCT nct_id FROM conditions WHERE downcase_name IN ",
        condition,
        ""
      )
    )
  }
  
  ### end ###
  
  ### get gender defined trials ###
  # gender = "male"
  if (!is.null(gender)) {
    if (tolower(gender) == 'all') {
      gender = paste0("('", gender, "','')")
    } else{
      if (tolower(gender) == 'male') {
        gender = paste0("('", gender, "','All','')")
      } else{
        gender = paste0("('", gender, "','All','')")
      }
    }
    gender_trials = dbGetQuery(con,
                               paste0(
                                 "SELECT DISTINCT nct_id FROM eligibilities WHERE gender IN ",
                                 gender,
                                 ""
                               ))
  }
  ### end ###
  
  ### get age defined trials ###
  # age = 1
  if (!is.null(age)) {
    all_age = dbGetQuery(
      con,
      paste0(
        "SELECT DISTINCT nct_id, minimum_age, maximum_age FROM eligibilities"
      )
    )
    all_age = data.table(all_age)
    # split table.
    all_age$minimum_age_as_number = as.character(lapply(strsplit(
      as.character(all_age$minimum_age), split = " "
    ), "[", 1))
    all_age$minimum_age_as_unit = as.character(lapply(strsplit(
      as.character(all_age$minimum_age), split = " "
    ), "[", 2))
    all_age$maximum_age_as_number = as.character(lapply(strsplit(
      as.character(all_age$maximum_age), split = " "
    ), "[", 1))
    all_age$maximum_age_as_unit = as.character(lapply(strsplit(
      as.character(all_age$maximum_age), split = " "
    ), "[", 2))
    
    age_nct = all_age[((
      minimum_age_as_number <= age &
        minimum_age_as_unit == 'Years'
    ) |
      (
        is.na(minimum_age_as_unit) |
          minimum_age_as_unit != 'Years'
      )
    ) &
      ((
        maximum_age_as_number >= age &
          maximum_age_as_unit == 'Years'
      ) | is.na(maximum_age_as_unit)
      ), nct_id]
    age_trials = data.table(nct_id = age_nct)
  }
  ### end ###
  
  
  ### get contry defined trials ###
  # country = c('United States', 'liver cancer')
  if (!is.null(country)) {
    country = paste0("'", country, "'")
    country = paste0(country, collapse = ",")
    country = paste0("(", country, ")")
    country_trials = dbGetQuery(con,
                                paste0(
                                  "SELECT DISTINCT nct_id FROM facilities WHERE country IN ",
                                  country,
                                  ""
                                ))
  }
  ### end ###
  
  ### get state defined trials ###
  # state = c('Illinois', 'New York')
  if (!is.null(state)) {
    state = paste0("'", state, "'")
    state = paste0(state, collapse = ",")
    state = paste0("(", state, ")")
    state_trials = dbGetQuery(con,
                              paste0(
                                "SELECT DISTINCT nct_id FROM facilities WHERE country IN ",
                                state,
                                ""
                              ))
  }
  ### end ###
  
  ### get health defined trials ###
  ctrl = TRUE
  if (ctrl == TRUE) {
    ctrl = "('Accepts Healthy Volunteers')"
  } else{
    ctrl = "('','NO')"
  }
  ctrl_trials = dbGetQuery(
    con,
    paste0(
      "SELECT DISTINCT nct_id FROM eligibilities WHERE healthy_volunteers IN ",
      ctrl,
      ""
    )
  )
  
  ### get final trials ###
  var_list = list(
    condition_trials$nct_id,
    gender_trials$nct_id,
    age_trials$nct_id,
    country_trials$nct_id,
    state_trials$nct_id,
    ctrl_trials$nct_id
  )
  
  union_trials = Reduce(
    union,
    var_list
  )
  
  intesect_trials = union_trials
  
  for(single_trials in var_list)
  if(!is.null(single_trials)){
    intesect_trials = intersect(single_trials,intesect_trials)
  }
  return(intesect_trials)
}

packrat::init('/Users/cl3720/Projects/DQeST/')
packrat::bundle(project = "DQeST.Rproj",file = "~/Projects/DQeST/",overwrite = T)

