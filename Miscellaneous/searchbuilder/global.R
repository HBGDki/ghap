toInstall=c('RMySQL','shiny','DT','plyr','reshape2','queryBuildR','jsTree','vcs','dplyr')

if(!'queryBuildR'%in%rownames(installed.packages())) devtools::install_github('yonicd/queryBuildR')
if(!'jsTree'%in%rownames(installed.packages())) devtools::install_github('metrumresearchgroup/jsTree')
if(!'vcs'%in%rownames(installed.packages())) devtools::install_github('metrumresearchgroup/vcs')

sapply(toInstall[!toInstall%in%rownames(installed.packages())],function(x) install.packages(x))

require(RMySQL)
library(shiny)
library(DT)
library(plyr)
library(reshape2)
library(queryBuildR)
library(vcs)
library(jsTree)
library(ghap)
library(dplyr)

#forked from https://github.com/Yannael/shinyQueryBuildR
load('filters.Rdata')
load('meta_ghap.Rdata')

createDB<-function() {
  load('filters.Rdata')
  nm=names(meta_ghap)
  meta_ghap=meta_ghap[,c('STUDY_TYPE',nm[nm!='STUDY_TYPE'])]
  filters<-getFiltersFromTable(meta_ghap)
  save(file='filters.Rdata',filters)
  datadb<-dbConnect(RSQLite::SQLite(), "data/data.db")
  dbWriteTable(datadb,"datatable",meta_ghap,row.names=F,overwrite=T)
  dbDisconnect(datadb)
}

loadData<-function(sql) {
  if (sql!="") sql<-paste0("where ",sql)
  datadb<-dbConnect(RSQLite::SQLite(), "data/data.db")
  datacontent<-dbGetQuery(datadb,paste0("select * from datatable ",sql))
  dbDisconnect(datadb)
  datacontent
}

createDB()


get_study_n<-function(current_query){

  n_summ<-current_query%>%
    select(STUDY_TYPE,DOMAIN,STUDY_ID,VARIABLE=STUDY_VARIABLE)%>%distinct%>%
    group_by(STUDY_TYPE,DOMAIN,STUDY_ID)%>%
    summarise_at(funs(paste0(sprintf('%s IS NOT NULL',.),collapse=' AND ')),.vars=vars(VARIABLE))%>%
    group_by(STUDY_TYPE,DOMAIN,VARIABLE)%>%
    summarise_at(funs(paste0(sprintf("'%s'",.),collapse=',')),.vars=vars(STUDY_ID))


  if(file.exists('../data/ghap_longitudinal.sqlite3')) long_db<-dbConnect(RSQLite::SQLite(), "../data/ghap_longitudinal.sqlite3")
  if(file.exists('../data/ghap_cross_sectional.sqlite3')) cross_db<-dbConnect(RSQLite::SQLite(), "../data/ghap_cross_sectional.sqlite3")
    

  get_n<-n_summ%>%ddply(.(STUDY_TYPE,DOMAIN,VARIABLE),.fun=function(x){
    q <- sprintf("select STUDYID as STUDY_ID, count(DISTINCT SUBJID) as SUBJID_N from %s WHERE %s AND STUDY_ID IN (%s) GROUP BY STUDY_ID",x$DOMAIN,x$VARIABLE,x$STUDY_ID)

    if(x$STUDY_TYPE=='Longitudinal'){
      DBI::dbGetQuery(conn=long_db,q)
    }else{
      DBI::dbGetQuery(conn=cross_db,q)
    }

  },.progress = 'text')
  
  dbDisconnect(long_db)
  dbDisconnect(cross_db)
  
  get_n%>%select(-VARIABLE)
}
