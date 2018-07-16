valueNormalize <- function(has_value){
  if(is.na(has_value)){
    return(NA)
  }
  has_value = tolower(has_value)
  bigger_words = c("\\>","\\+","more","big","great","more","excess","high","better","exceed","old","long","over","at least")
  smaller_words = c("\\<","\\-","less","small","low","worse","young","short")
  and_words = c("and","to","\\-")
  
  # scan two number first.
  and_match = str_match(has_value, paste0("([\\d,\\.]+).*?",and_words,".*?([\\d,\\.]+)"))
  if(sum(!is.na(and_match[,2])) > 0){
    # at least one match.
    and_match = and_match[!is.na(and_match[,2]),c(2,3)]
    if(!is.null(dim(and_match))){
      # more than one match. select the first match.
      and_match = and_match[1,]
    }
    # require the first one is smaller than second one.
    min = and_match[1]
    max = and_match[2]
    return(paste(min,":",max,sep = ""))
  }
  
  # scan big number.
  big_match_suffix = str_match(has_value, paste0("([\\d,\\.]+).*?",bigger_words))
  big_match_prefix = str_match(has_value, paste0(bigger_words,".*?([\\d,\\.]+)"))
  big_match = rbind(big_match_suffix,big_match_prefix)
  if(sum(!is.na(big_match[,2])) > 0){
    # at least one match.
    big_match = big_match[!is.na(big_match[,2]),c(2)]
    if(length(big_match) > 1){
      # more than one match. select the first match.
      big_match = big_match[1]
    }
    # require the first one is smaller than second one.
    min = big_match[1]
    max = Inf
    return(paste(min,":",max,sep = ""))
  }
  
  # scan small number.
  small_match_suffix = str_match(has_value, paste0("([\\d,\\.]+).*?",smaller_words))
  small_match_prefix = str_match(has_value, paste0(smaller_words,".*?([\\d,\\.]+)"))
  small_match = rbind(small_match_suffix,small_match_prefix)
  if(sum(!is.na(small_match[,2])) > 0){
    # at least one match.
    small_match = small_match[!is.na(small_match[,2]),c(2)]
    if(length(small_match) > 1){
      # more than one match. select the first match.
      small_match = small_match[1]
    }
    # require the first one is smaller than second one.
    min = -Inf
    max = small_match[1]
    return(paste(min,":",max,sep = ""))
  }
  
  # if not return before. then return NA
  return(NA)
}
