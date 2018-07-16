removeIncOnlyTrial = function(dt){
  trialWithExc = dt[V2 %in% 'EXC'] %>% pull(V1) %>% unique()
  dt = dt[V1 %in% trialWithExc]
  return(dt)
}