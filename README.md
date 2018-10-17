# DQueST (Dynamic Questionnaire for Personalized Search of Clinical Trials )
DQeST project includes an R package for question sequence optimization based on the frequency and accuracy of eligibility criteria to OMOP concept mapping.

## Prerequisites
1. packrat_0.4.9-2 (http://rstudio.github.io/packrat/) is used for external R package management. 
```
packrat::unbundle(bundle="testproject-2014-07-15.tar.gz", where="/home/bob/projects")
```
2. pre-generated model app/model/knowledgedBase.rda (optional, app/model/TRIAL_INFO.rda and app/model/titleDt.csv if connect to CT.gov on fly)
-- The model files are too big (>20mb) to upload to github
3. resource/AACT_LOGIN contains connection information to AACT (https://www.ctti-clinicaltrials.org/aact-database) database
-- registration required

## Usage 
1. Download XML files from clinicaltrials.gov (https://clinicaltrials.gov/ct2/resources/download#DownloadAllData)
2. Using *tools* to generate criteria library (off-line module)
3. run app/app.R (real-time module)

## Deployment
Deploy it on R shinny server

## Next step
1. wrap off-line module in a better pipeline

## Versioning
v_0.0.1

## Authors
Cong Liu, Chi Yuan, and Chunhua Weng
