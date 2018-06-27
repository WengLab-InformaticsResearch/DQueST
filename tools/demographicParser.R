library(data.table)
library(dplyr)
library(stringr)
library(tidyr)

ageParser = function(age){
  if(is.na(age)){
    return(as.numeric(NA))
  }
  ageSplit = unlist(strsplit(age,split = " "))
  value = as.numeric(ageSplit[1])
  unit = ageSplit[2]
  if(str_detect(unit,'Month')){
    value = 0.0833334 * value
    return(round(value,digits = 1))
  }
  if(str_detect(unit,'Week')){
    value = 0.0191781 * value
    return(round(value,digits = 1))
  }
  if(str_detect(unit,'Day')){
    value = 0.00273973 * value
    return(round(value,digits = 1))
  }
  if(str_detect(unit,'Hour')){
    return(round(value,digits = 1))
  }
  if(str_detect(unit,'Minute')){
    value = 1.9025833333e-6 * value
    return(round(value,digits = 1))
  }
  return(value)
}

df = fread(file = "../resource/xmlParsed.csv",header = T,na.strings = c("N/A","","NA","NULL","All"),
           stringsAsFactors = F,
           fill = T,showProgress = T)
df[healthy_volunteers=='Accepts Healthy Volunteers',healthy_volunteers:='Yes']
#df_tmp = df %>% mutate(maximum_age = ageParser(maximum_age))
#df %>% pull(maximum_age) %>% unique()
df[!is.na(maximum_age),maximum_age1:= unlist(lapply(maximum_age,ageParser))]
df[is.na(maximum_age),maximum_age1:=as.numeric(NA)]
df[!is.na(minimum_age),minimum_age1:= unlist(lapply(minimum_age,ageParser))]
df[is.na(minimum_age),minimum_age1:=as.numeric(NA)]
df[,c("V1","city","country","state"):=NULL]


demoDt = df
setkey(demoDt,nct_id,gender,healthy_volunteers,minimum_age,maximum_age)
write.csv(x = demoDt, file = "../resource/demoDt.csv",row.names = F)
# save(demoDt,file = "../resource/demoDt.csv")


