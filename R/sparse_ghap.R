#' @title Create a sparse ghap repository
#' @description Create a ghap repository that fetches only specified subdirectories and file types
#' @param repo_url character, repository url path
#' @param repo character, path on local disk that repository is cloned to
#' @param dirs character, vector of repository subdirectories to fetch
#' @param create boolean, create a new git clone?, Default: TRUE
#' @param remote character, alias of the remote Default: 'origin'
#' @param branch character, alias of the branch, Default: 'master'
#' @return nothing
#' @export
sparse_ghap<-function(repo_url,repo,dirs,create=TRUE,remote='origin',branch='master'){
  if(create){
    # New repository
    ghap_base=normalizePath(ghap::get_git_base_path(),winslash = '/')
    repo_dir=file.path(ghap_base,repo)
    if(!dir.exists(repo_dir)) dir.create(repo_dir)
    setwd(repo_dir)
    system('git init')
    system(sprintf('git remote add -f %s %s',remote,repo_url))
    system('git config core.sparsecheckout true')
    cat(dirs,file = '.git/info/sparse-checkout',sep = '\n')
    system(sprintf('git pull %s %s',remote,branch))
    system(sprintf('git branch --set-upstream-to=origin/%s master',branch))
  }else{
    # Existing repository
    system('git config core.sparsecheckout true')
    cat(dirs,file = '.git/info/sparse-checkout',sep = '\n',append = TRUE)
    system('git read-tree -mu HEAD')
    
    # If you later decide to change which directories you would like checked out, 
    # simply edit the sparse-checkout file and run git read-tree again as above  
  }
  
}