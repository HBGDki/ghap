studs<-f%>%group_by(Study_Type,DOMAIN)%>%summarise(studs=paste0(sprintf("'%s'",STUDYID),collapse=','))
studs$cols<-paste0(gsub('^(.*?)\\[|\\]','',tail(names(f),-3)),collapse=',')
studs$wherecols<-paste0(gsub('^(.*?)\\[|\\]','',tail(names(f),-3)),' IS NOT NULL',collapse=' AND ')
studs$Study_Type<-ifelse(studs$Study_Type=='Longitudinal','long','cross')
queries<-sprintf("SELECT STUDYID, count(SUBJID) as subj_count from (SELECT DISTINCT STUDYID, SUBJID,%s FROM %s WHERE STUDYID in (%s) AND %s) GROUP BY STUDYID",studs$cols,studs$DOMAIN,studs$studs,studs$wherecols)

longdb<-dbConnect(RSQLite::SQLite(), "../data/ghap_longitudinal.sqlite3")
crossdb<-dbConnect(RSQLite::SQLite(), "../data/ghap_cross_sectional.sqlite3")

x<-dbGetQuery(longdb,queries[2])

###########

show_ghap <- function(path=getwd()){
  this_wd <- getwd()
  setwd(tools::file_path_as_absolute(path))
  path <- system('git ls-tree -r HEAD --name-only',intern=TRUE)
  x <- lapply(strsplit(path, "/"), function(z) as.data.frame(t(z)))
  x <- plyr::rbind.fill(x)
  setwd(this_wd)
  x
}

x1<-plyr::mdply(list.dirs(recursive = FALSE),show_ghap)
x1$X1=as.character(factor(x1$X1,labels=gsub('./','',list.dirs(recursive = FALSE))))


x1$depth <- apply(x1,1,function(y) sum(!is.na(y)))
d3Tree::d3tree(list(root = d3Tree::df2tree(rootname='archive',struct=x1),layout = 'collapse'))

x<-ghap::get_study_list()



plyr::d_ply(y[grepl('^ki',y$grant_folder),]%>%slice(1:10),
            .variables = c('grant_folder'),
            .fun=function(x){
              sparse_ghap(repo_url = sprintf('https://git.ghap.io/stash/scm/hbgd/%s.git',head(x$grant_folder,1)),repo = sprintf('HBGD2/%s',head(x$study_id,1)),queries = '*.txt') 
            },
            .progress='txt')

###########

preview_study=function(id, rules=c('*.txt','*.rtf','.docx')){
  path <- get_git_base_path()
  
  studies <- get_study_list()
  idx <- which(studies$study_id == id | studies$fstudy_id == id)
  if (length(idx) == 0) {
    idx <- which(studies$short_id == id)
    if (length(idx) == 0)
      stop_nice("A study with id '", id, "' could not be found.")
  }
  
  grant_folder <- studies$grant_folder[idx]
  
  if (!dir.exists(repo_path)) {
    dir.create(repo_path, recursive = TRUE, showWarnings = FALSE)
    message("Minimal check out of 'hbgd/", grant_folder, "' repo...")
    nm <- paste0("https://git.ghap.io/stash/scm/hbgd/", grant_folder, ".git")
    sparse_ghap(repo_url = nm,repo = file.path("hbgd", grant_folder),dirs = rules,create = TRUE)
  }
  }
  
###########

  studies=ghap::get_study_list()
  
  tips.folders=c(adam='Analysis-Ready data derived form SDTM data sets (USERS: ALL)',
                 docs='Study documentation (USERS: ALL)',
                 fmt='SAS format files (USERS: Data Management)',
                 import='Raw metadata submitted by Prinicipal Investigators (USERS: Data Management)',
                 jobs='SAS programs for creating SDTM datasets',
                 raw='Data submitted by Prinicipal Investigators (USERS: Data Management)',
                 sdtm='Data in HBGDki Standardized format (USERS: Data Scientists)'
  )
  
  fields=c('study_description','subject_count','grant_folder','study_type','intervention_type','country','population','study_url')
  
  fields.title=gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", gsub('_',' ',fields), perl=TRUE)
  
  tips.studies=t(apply(studies[,fields[c(1)]],1,function(x){
    paste(fields.title[c(1)],x,sep=': ')
  }))
  
  
  names(tips.studies)=studies[['study_id']]
  
  tips=c(tips.folders,tips.studies)
  
  tree.list<-unlist(sapply(dir('../../GHAP/QuantSci/git/HBGD1'),function(i) file.path(i,vcs::ls_remote(sprintf('../../GHAP/QuantSci/git/HBGD1/%s',i),vcs='git'))))
  
  jsTree::jsTree(x,tooltips = tips)
  
  vcs::navigate_remote('../../GHAP/QuantSci/git/HBGD1/GUSTO',vcs = 'git',output.opts=list(tooltips=tips))
  
#################
  
  
  setwd('../../ghap/QuantSci/git/HBGD1/AAP-NSECH/')
  readLines('.git/config')
  
  path='AAP-NSECH'
  branch='master'
  
  uri_bit <- sprintf('https://git.ghap.io/!api/1.0/repositories/%s/directory/%s',path,branch)
  httr::http_error(uri_bit)
  unlist(httr::content(httr::GET(uri_bit))$value)[-1]
  