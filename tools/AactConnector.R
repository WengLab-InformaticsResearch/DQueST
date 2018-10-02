source('../resource/AACT_LOGIN.cfg.R')
library(RPostgreSQL)
library(data.table)


getApi = function(dbname,host,port,user,password){
  drv = dbDriver('PostgreSQL')
  con = dbConnect(drv = drv, 
                  dbname=dbname,
                  host=host, 
                  port=port, 
                  user=user, 
                  password=password)
  return(list(CON=con))
}


getCache = function(con){
  aact_sample = dbGetQuery(con, "select distinct study_type from studies")
  
  aact_condition = dbGetQuery(con, "SELECT name,COUNT(*) AS count FROM conditions GROUP BY name ORDER BY count DESC")
  condition_keyword = aact_condition$name[1:100]
  
  eligibility_search = dbGetQuery(con, "SELECT distinct gender FROM eligibilities")
  
  facilitity_search = dbGetQuery(con,"SELECT DISTINCT country, state FROM facilities where country = 'United States'")
  us_country_state = facilitity_search$state
  
  return(list(CONDITION_KEY_WORD_TOP100 = condition_keyword,US_STATES = us_country_state))
}

