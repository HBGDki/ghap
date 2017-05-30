require("RMySQL")
library(ghap)

createDB<-function() {
  data(meta_ghap)
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

