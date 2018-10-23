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
1. create an AWS ubuntu 16.04 instance https://www.charlesbordet.com/en/shiny-aws-2/
2. config R shiny server on AWS VM https://tm3.ghost.io/2017/12/31/deploying-an-r-shiny-app-to-aws/. It is important to notice the default security setting in AWS should be changed to open port 3838 or change nginx setting to redict 80 request to 127.0.0.1:3838 and change shiny-server setting as well
3. copy `app/` to `/srv/shiny-server/sample-apps/` by `scp -v -i "aws-key.pem" -r /your/path/to/app ubuntu@aws-dns:`
4. Install required package as `sudo -i` http://freigeist.devmag.net/r/773-deploying-shiny-server-on-amazon-some-troubleshoots-and-solutions.html. It is important to notice that `curl` should be installed for `devtools`, which requires package installed in ubuntu. https://stackoverflow.com/questions/20671814/non-zero-exit-status-r-3-0-1-xml-and-rcurl. 
`postgresql` should be installed in ubuntu. https://stackoverflow.com/questions/22202141/installing-rpostgresql-on-linux. Install `digest` package in R 
5. start shiny server by `sudo service shiny-server start`
6. view logs at `/var/log/shiny-server`
7. make sure ../resource/ACT_login is loaded

## Next step
1. wrap off-line module in a better pipeline

## Versioning
v_0.0.1

## Authors
Cong Liu, Chi Yuan, and Chunhua Weng
