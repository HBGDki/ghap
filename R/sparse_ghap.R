#' @title Create a sparse ghap repository
#' @description Create a ghap repository that fetches only specified subdirectories and file types
#' @param repo_url character, repository url path
#' @param repo character, path on local disk that repository is cloned to
#' @param dirs character, vector of repository subdirectories to fetch
#' @param new boolean, is this is new clone, Default: TRUE
#' @param remote character, alias of the remote Default: 'origin'
#' @param branch character, alias of the branch, Default: 'master'
#' @return nothing
#' @export
sparse_ghap<-function(repo_url,repo,dirs,new=TRUE,remote='origin',branch='master'){
  if(new){
    # New repository
    if(!dir.exists(repo)) dir.create(repo)
    setwd(repo)
    system('git init')
    system(sprintf('git remote add -f %s %s'),remote,repo_url)
    system('git config core.sparsecheckout true')
    system(sprintf('echo "%s" >> .git/info/sparse-checkout'),dirs)
    system(sprintf('git branch --set-upstream-to=origin/%s master',branch))
    system(sprintf('git pull %s %s'),remote,branch)
  }else{
    # Existing repository
    system('git config core.sparsecheckout true')
    system(sprintf('echo "%s" >> .git/info/sparse-checkout'),dirs)
    system('git read-tree -mu HEAD')
    
    # If you later decide to change which directories you would like checked out, 
    # simply edit the sparse-checkout file and run git read-tree again as above  
  }
  
}