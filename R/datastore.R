#' @title Retrieve git hbgd repository addresses from data store explorer
#' @description Copy URL of current data store explorer state on clipboard and function returns list of
#' checked studies with their respective clone paths on the git repository.
#' @return data.frame
#' @importFrom rvest html_nodes html_attr html_text
#' @importFrom utils sessionInfo read.table
#' @importFrom xml2 read_html
#' @export
paste_repos=function(){
  url=ifelse(grepl('apple',utils::sessionInfo()[[1]]$platform),
             suppressWarnings(utils::read.table(pipe('pbpaste'),stringsAsFactors = FALSE)[,1]),
             suppressWarnings(utils::read.table('clipboard',stringsAsFactors = FALSE)[,1]))
  
  x<-xml2::read_html(url)
  
  x1<-x%>%rvest::html_nodes(xpath='//*[starts-with(@id,"id_study")]')%>%rvest::html_attr('checked')
  x2<-x%>%rvest::html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "switch-label", " " ))]')%>%rvest::html_text()
  
  x3<-x2[!is.na(x1)]
  
  datOut=ghap::get_study_list()
  datOut=datOut[datOut$studyid%in%x3,c('study_id','grant_folder')]
  datOut$git_path=paste0("https://git.ghap.io/stash/scm/hbgd/", datOut$grant_folder, ".git")

  datOut
  
}
