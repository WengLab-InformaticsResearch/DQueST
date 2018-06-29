library(data.table)
wMatrixInitByCsv = function(File){
  # init by read a csv file.
  dt = NULL
  dt = fread(file = File,sep = ",",
        header = T,
        na.strings = c("NA","NULL",""),
        stringsAsFactors = F,
        strip.white = T,
        fill = T,
        data.table = T)
  dt = formatDt(dt)
  checkFormat(dt)
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("init by Csv finished"))
  return(dt)
}

trialDtInitByCsv = function(File){
  # init by read a csv file.
  dt = NULL
  dt = fread(file = File,sep = ",",
             header = T,
             na.strings = c("NA","NULL","","N/A"),
             stringsAsFactors = F,
             strip.white = T,
             fill = T,
             data.table = T)
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("init by Csv finished"))
  return(dt)
}

demoDtInitByCsv = function(File){
  dt = NULL
  dt = fread(file = File,sep = ",",
             header = T,
             na.strings = c("NA","NULL","","N/A"),
             stringsAsFactors = F,
             strip.white = T,
             fill = T,
             data.table = T)
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("init by Csv finished"))
  return(dt)
}

geoDtInitByCsv = function(File){
  dt = NULL
  dt = fread(file = File,sep = ",",
             header = T,
             na.strings = c("NA","NULL","","N/A"),
             stringsAsFactors = F,
             strip.white = T,
             fill = T,
             data.table = T)
  tryCatch(expr = {1},
           error = function(e) e,
           finally = print("init by Csv finished"))
  return(dt)
} 

initByRd = function(rdata){
  # init by read an rda.
  dt = get(load(rdata))
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

formatDt = function(dt) {
  dt = dt %>%
    mutate(temporal_min = case_when(
      is.na(temporal_min) &
        domain %in% c("condition", "procedure", "drug") ~ -Inf,
      TRUE ~ as.numeric(temporal_min)
    )) %>%
    mutate(temporal_max = case_when(
      is.na(temporal_max) &
        domain %in% c("condition", "procedure", "drug") ~ Inf,
      TRUE ~ as.numeric(temporal_max)
    )) %>%
    mutate(value_min = case_when(
      is.na(value_min) &
        domain %in% c("measurement") ~ -Inf,
      TRUE ~ as.numeric(value_min)
    )) %>%
    mutate(value_max = case_when(
      is.na(value_max) &
        domain %in% c("measurement") ~ Inf,
      TRUE ~ as.numeric(value_max)
    ))
  return(dt)
}

checkFormat = function(dt){
  # check colnames.
  if(dim(dt)[2] != 16){
    stop("colname is not compatible.")
  }
  if(sum(colnames(dt) == c("id","nct_id","term","ie_flag",
                       "domain","value_min","value_max",
                       "status","temporal_min","temporal_max",
                       "omop_id","omop_name","mapping_score",
                       "common_omop_id","common_omop_name","cofreq")) != 16){
    stop("colname is not compatible.")
  }
  if(dim(dt)[1] < 5){
    stop("please input more trials.")
  }
  return(NULL)
}

