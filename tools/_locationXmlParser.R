library(data.table)
library(dplyr)
library(stringr)
library(tidyr)
library(xml2)
library(purrr)
# xml1 = '../resource/AllPublicXML/NCT0325xxxx/NCT03254537.xml'
# xml2 = '../resource/AllPublicXML/NCT0325xxxx/NCT03254563.xml'
# xml3 = '../resource/AllPublicXML/NCT0325xxxx/NCT03254017.xml'
# xml4 = '../resource/AllPublicXML/NCT0325xxxx/NCT03253991.xml'
# xml5 = '../resource/AllPublicXML/NCT0000xxxx/NCT00000102.xml'
# xml = c(xml1,xml2,xml3,xml4,xml5)

nct_dir = list.dirs(path="../resource/AllPublicXML/", recursive = F,full.names = T)
for(dir in nct_dir[1]){
  xmlFiles <- list.files(path=nct_dir, pattern="*.xml", full.names=T, recursive=TRUE)
  if(length(xmlFiles) != 252330){
    stop("File missing")
  }
  print("Read xml FilesNames")
  xmlSet = xmlFiles
  baseName = as.list(basename(xmlSet))
  nct_id = baseName %>% map(str_split,pattern="\\.") %>% map_chr(c(1,1))
  print("Read nct_id nct id")
  
  xmldf = map(xmlSet, read_xml) %>%
  print("read_xml")
  xmldf = xmldf %>% map(xml_find_all,"//address") %>%
    at_depth(2, as_list) %>% 
    set_names(nct_id) %>%
    map_df(map_df,flatten,.id = 'nct_id')
  print("convert to df")
}



geoDt = data.table(xmldf)
setkey(geoDt,nct_id,city,state,country,zip)
write.csv(x = geoDt, file = "../resource/geoDt.csv",row.names = F)
# save(demoDt,file = "../resource/demoDt.csv")


