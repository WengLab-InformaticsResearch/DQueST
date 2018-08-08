# code to interact with ctgov
library(httr)
library(xml2)

get_nct_by_advanced = function(param) {
  nct_id_list = NULL
  tryCatch({1})
  return(nct_id_list)
}

get_nct_by_term = function(term) {
  nct_id_list = NULL
  if(length(term) > 1){
    term = paste(x,collapse=" ")
  }
  tryCatch({
    url = paste0('https://clinicaltrials.gov/ct2/results?cond=',
                 term,
                 '&displayxml=true')
    nct_count = get_nct_count(url)
    if (is.null(nct_count)) {
      return(NULL)
    }
    url = paste0(url, '&count=', nct_count)
    nct_id_list = get_nct_list(url)
  })
  return(nct_id_list)
}

# get the total nct count
get_nct_count = function(url) {
  count = NULL
  tryCatch({
    url = paste0(url, '&count=1')
    req_url = modify_url(url)
    res_xml = GET(req_url)
    xml_data = read_xml(res_xml)
    count = xml_attr(xml_data, 'count')
  })
  return(count)
}

# get nct id list
get_nct_list = function(url) {
  nct_id_list = NULL
  tryCatch({
    req_url = modify_url(url)
    res_xml = GET(req_url)
    xml_data = read_xml(res_xml)
    nct_id_node = xml2::xml_find_all(xml_data, './/nct_id')
    nct_id_list = unique(xml_text(nct_id_node))
  })
  return(nct_id_list)
}
