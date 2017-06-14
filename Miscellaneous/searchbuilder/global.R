toInstall=c('RMySQL','shiny','DT','plyr','reshape2','queryBuildR','dplyr')

if(!'queryBuildR'%in%rownames(installed.packages())) devtools::install_github('Yannael/queryBuildR')

sapply(toInstall[!toInstall%in%rownames(installed.packages())],function(x) install.packages(x))

require(RMySQL)
library(shiny)
library(DT)
library(plyr)
library(reshape2)
library(queryBuildR)
library(dplyr)

#forked from https://github.com/Yannael/shinyQueryBuildR
load('filters.Rdata')

createDB<-function() {
  load('filters.Rdata')
  names(names(meta_ghap))=gsub('[.]','_',names(meta_ghap))
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

f<-list()
