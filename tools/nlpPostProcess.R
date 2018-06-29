library(data.table)
library(dplyr)
dt = fread(file = 'resource/alltresults_c3.txt',sep = "\t",header = F,stringsAsFactors = F,fill = T,showProgress = T)
colnames(dt) = c('nct_id','ie_flag','term','domain','negation','temporal','value')
# change domain to lower case.
dt[,':='(domain=tolower(domain))]
# change term to lower case.
dt[,':='(term=tolower(term))]
# change ie_flag to 1/0
dt[,':='(ie_flag=(ie_flag=='INC'))]
dt[,':='(ie_flag=as.numeric(ie_flag))]
# change negation to 1/0
dt[,':='(negation=as.numeric(negation))]
dt[,':='(ie_flag=xor(ie_flag,negation))]
dt[,':='(ie_flag=as.numeric(ie_flag))]
dt[,'negation':=NULL] # drop negation.
# parse demographic
demo = dt[domain=='demographic']
demo[,c('term','')] %>% pull() %>% unique()
