library(ghap)
library(reshape2)
library(plyr)
library(dplyr)
library(XLConnect)

git_base=normalizePath(get_git_base_path(),winslash = '/')
dataDirIDX<-file.path(git_base,"HBGD/studyaccounting")
dataDirCommon<-file.path(git_base,"HBGD/common/meta")
sqlDir<-"H:/GitHub/ghap/Miscellaneous/data"

f=data.frame(f=list.files(file.path(dataDirIDX,'StudyExplorer/jobs'),pattern = 'IDX',full.names = TRUE),stringsAsFactors = FALSE)
IDX<-plyr::ddply(f,c('f'),function(x) read.csv(x$f,stringsAsFactors = FALSE)%>%reshape2::melt(.,'STUDYID')%>%select(-value)%>%unique,.progress='text')

w<-loadWorkbook(file.path(dataDirCommon,'HBGD-codelists.xlsx'))
code.list<-readWorksheet(w,getSheets(w))


code.list.super=vector('list',3)
names(code.list.super)=c('continuous','discrete','geo')

plyr::l_ply(names(code.list),function(x){
  df=code.list[[x]]
  names(df)=toupper(names(df))
  if(any(grepl('CODEN',names(df)))){
    code.list.super$continuous[[x]]<<-df 
  }else{
    if(any(grepl('RMAP',names(df)))){
      code.list.super$geo[[x]]<<-df  
    }else{
      code.list.super$discrete[[x]]<<-df
    } 
  }
  }
  )


code.list.super=plyr::llply(code.list.super,function(x) plyr::ldply(x,.id = 'DOMAIN'))



study.info<-read.csv("H:/GHAP/QuantSci/git/HBGD/studyaccounting/StudyExplorer/jobs/studyinfo.csv",stringsAsFactors = FALSE)


w<-loadWorkbook(file.path(dataDirCommon,"HBGD-dataspecs.xlsx"))
data.specs<-readWorksheet(w,getSheets(w))


ghap_long <- src_sqlite(path = file.path(sqlDir, "ghap_longitudinal.sqlite3"), create = F)
ghap_cross <- src_sqlite(path = file.path(sqlDir, "ghap_cross_sectional.sqlite3"), create = F)

create_search_list=function(ghap_db){
x<-data.frame(DOMAIN=head(dplyr::db_list_tables(ghap_db$con),-1),stringsAsFactors = FALSE)%>%
  plyr::ddply(.variables = c('DOMAIN'),.fun=function(df0){ 
    df=tbl(ghap_db,df0$DOMAIN)%>%group_by(STUDYID)%>%summarise_all(funs(sum(is.na(.))<n()))%>%collect(n=Inf)
    reshape2::melt(df,id='STUDYID')%>%filter(value==1)%>%select(-value)%>%distinct()
      },.progress = 'text')


x%>%
  mutate_if(is.factor,as.character)%>%
  left_join(plyr::ldply(data.specs[-c(1:2)],
                        .fun=function(df) df%>%
                          select(variable=NAME,LABEL)%>%
                          distinct,.id='DOMAIN')%>%
                          mutate_if(is.factor,as.character),
            by=c('DOMAIN','variable'))%>%
  left_join(data.specs$DATASETS%>%
              select(DOMAIN=MEMNAME,DOMAIN.LBL=LABEL,DOMAIN.STRUCTURE=Structur),
            by='DOMAIN')%>%
  left_join(study.info,by='STUDYID')
}


combined_list_long=create_search_list(ghap_long)
combined_list_cross=create_search_list(ghap_cross)


meta_ghap=rbind(combined_list_long,combined_list_cross)




