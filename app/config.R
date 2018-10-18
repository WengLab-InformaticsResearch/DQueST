rm(list = ls())
### set proxy ###
Sys.unsetenv("http_proxy")
Sys.unsetenv("https_proxy")
### remove this two lines if not running on a CUMC server. ###
# Sys.setenv(http_proxy="http://bcp3.cumc.columbia.edu:8080")
# Sys.setenv(https_proxy="https://bcp3.cumc.columbia.edu:8080")
### end of proxy ###

### load library ###
library(shiny)
library(shinyjs)
library(shinyBS)
library(devtools)
# devtools::install_github("AnalytixWare/ShinySky")
# library(shinysky)
# library(R.utils)
library(RPostgreSQL)
library(data.table)
library(dplyr)
library(DT)
### end of loading library ###

### load username and password ###
source('../resource/AACT_LOGIN.cfg.R')
### end ###

### load util functions one by one ###
source('./util/AactApi.gov.R')
source('./util/UiUtils.R')
source('./util/Render.R')
source('./util/Searcher.R')
source('./util/Optimizer.R')
source('./util/Refresher.R')
source('./util/Updater.R')
source('./util/Helpers.R') 
### end ###


### load models ###
CON = getApi(
  dbname = dbname,
  host = host,
  port = port,
  user = user,
  password = password
)
CACHE = getCache(CON)
COUNTRY_NAME = unique(CACHE$COUNTRY_STATE_TABLE$country)
TRIAL_INFO = get(load(file = "model/TRIAL_INFO.rda"))
CRITERIA_LIB = get(load(file = "model/kb_m_0.9_ls_1_c_5_agg_TRUE_small.rda"))
### end ###
