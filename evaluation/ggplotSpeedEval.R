rm(list=ls())
setwd('~/Projects/eqacts/evaluation/')
load('speed_stat_10condition.rda')
library(ggplot2)
library(dplyr)
df = do.call("rbind", speed_stat_10condition)
condition_fo_eval_abb = c("BRCA","OBESE","HIV","HTN","DM","CAD","DEPR","AD","ATHM","RA")
df = df %>% mutate(condition = rep(condition_fo_eval_abb,each=dim(df)[1]/10))
df_plot = df %>% group_by(condition,iter) %>% mutate(orig_length = max(trial_length)) %>% mutate(remove_percentage = 1-(trial_length/orig_length))
df_plot = df_plot %>% ungroup() %>% mutate(condition = as.factor(paste0(condition," (",orig_length,")")))

df_plot = df_plot %>% group_by(condition,seq) %>% summarise(mean_remove_percentage = mean(remove_percentage))

p = df_plot %>% ggplot(aes(x = seq,y = mean_remove_percentage,group=factor(condition),color=condition)) +
  geom_line() + 
  xlab('number of questions answered') +
  ylab('average percent of trials filtered out') +
  scale_y_continuous(limits = c(0, 0.9))
p = p + theme_bw()
p