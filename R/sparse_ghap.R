#' @title Create a sparse ghap repository
#' @description Create a ghap repository that fetches only specified subdirectories and file types
#' @param repo_url character, repository url path
#' @param repo character, path on local disk that repository is cloned to
#' @param dirs character, vector of repository subdirectories to fetch
#' @param create boolean, create a new git clone?, Default: TRUE
#' @param append boolean, append new lines to sparse-checkout file, Default: TRUE
#' @param remote character, alias of the remote, Default: 'origin'
#' @param branch character, alias of the branch, Default: 'master'
#' @return nothing
#' @examples
#' \donttest{
#' \dontrun{
#' repo_url='https://git.ghap.io/stash/scm/hbgd/allcrosssectional.git'
#' repo='HBGD/allcrosssectional-sparse'
#' dirs=sprintf('Main/%s',c('jobs/*.sas','adam/*.rtf','sdtm/*.rtf')
#' #create new sparse clone
#' sparse_ghap(repo_url,repo,dirs)
#' 
#' #update sparse-checkout definitions (appends to current list)
#' sparse_ghap(repo_url,repo,dirs=c('Main/jobs/*.log'),create=FALSE)
#' }
#' }
#' @export
sparse_ghap <- function(repo_url, repo, dirs, create = TRUE, append = TRUE, remote = "origin", branch = "master") {
  
  ghap_base <- normalizePath(get_git_base_path(), winslash = "/")
  repo_dir <- file.path(ghap_base, repo)
  
  thisDir <- getwd()
  if (!dir.exists(repo_dir) & !dir.exists(file.path(dirname(getwd()), repo_dir))) 
    dir.create(repo_dir)
  if (thisDir != file.path(dirname(getwd()), repo_dir)) 
    setwd(repo_dir)
  
  if (create) {
    
    # New repository
    
    system("git init")
    system(sprintf("git remote add -f %s %s", remote, repo_url))
    system("git config core.sparsecheckout true")
    cat(dirs, file = ".git/info/sparse-checkout", sep = "\n")
    system(sprintf("git pull %s %s", remote, branch))
    system(sprintf("git branch --set-upstream-to=origin/%s master", branch))
    
  } else {
    
    # Existing repository
    
    system("git config core.sparsecheckout true")
    
    if (append) {
      
      dirs_append <- dirs[which(!dirs %in% readLines(".git/info/sparse-checkout"))]
      if (length(dirs_append) > 0) 
        cat(dirs_append, file = ".git/info/sparse-checkout", sep = "\n", append = append)
      
    } else {
      
      cat(dirs, file = ".git/info/sparse-checkout", sep = "\n")
      
    }
    
    system("git read-tree -mu HEAD")
    
  }
  
  setwd(thisDir)
}