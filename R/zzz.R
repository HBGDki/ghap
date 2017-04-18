.onLoad <- function(libname, pkgname){
  options(GHAP_GIT_BASE_PATH = Sys.getenv("GHAP_GIT_BASE_PATH"))
}
