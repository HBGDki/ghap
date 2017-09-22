#' Set the base path for all repositories checked out from git
#'
#' @param path a directory in which to place all checked out repositories (should be an empty directory)
#'
#' @details All GHAP repositories are automatically organized and stored in a directory structure under a base git path which is specified by this function. This function will set an R environment variable that will be present in all subsequent sessions such that you only need to specify the base path once.
#'
#' @export
set_git_base_path <- function(path) {
  if (!dir.exists(path))
    dir.create(path, recursive = TRUE)

  path <- normalizePath(path)
  if (!file.exists("~/.Renviron")) {
    a <- NULL
  } else {
    a <- readLines("~/.Renviron")
  }
  idx <- which(grepl("GHAP_GIT_BASE_PATH", a))
  if (length(idx) == 0)
    idx <- length(a) + 1

  a[idx] <- paste0("GHAP_GIT_BASE_PATH=", gsub("\\\\", "\\\\\\\\", path))
  cat(paste(a, collapse = "\n"), "\n", file = "~/.Renviron")
  options(GHAP_GIT_BASE_PATH = path)
}

#' Get a data frame of meta data about studies to be used
#'
#' @note Unlike \code{\link{get_study_list}}, this function returns a subset of studies that are guaranteed to return data in a standard format that includes anthropometry when used with \code{\link{use_study}}. Also note that there very well may be studies in the larger list returned by \code{\link{get_study_list}} that have the same kind of structure too but for some reason are not included in the results of this function.
#'
#' @export
#' @importFrom readr read_csv
#' @examples
#' \dontrun{
#' studies <- get_study_list_anthro()
#' wsb <- use_study("wsb")
#' }
get_study_list_anthro <- function() {
  path <- get_git_base_path()

  common_path <- file.path(path, "hbgd/common")

  if (!dir.exists(common_path))
    git_setup()

  res <- system(sprintf("git -C %s pull", common_path))
  if (res == 128) {
    check_git_credentials()
    stop("Problem with credentials or couldn't find git server.", call. = FALSE)
  }

  tmp <- suppressMessages(
    readr::read_csv(file.path(path, "hbgd/common/VisApps/VisApps.csv")))
  names(tmp) <- tolower(names(tmp))

  tmp2 <- get_study_list() %>%
    filter(status != "Obsolete")
  # tmp$study_id %in% tmp2$study_id
  tmp2 <- dplyr::select(tmp2, study_id, short_id, short_description,
    intervention_type, country, fstudy_id)

  suppressMessages(dplyr::left_join(tmp, tmp2))
}

#' Get a data frame of meta data about all "hbgd" studies
#'
#' @note Only studies are returned that have non-NA grant_folder
#'
#' @export
#' @examples
#' \dontrun{
#' studies <- get_study_list()
#' wsb <- use_study("wsb")
#' }
get_study_list <- function() {
  path <- get_git_base_path()

  common_path <- file.path(path, "common")

  if (!dir.exists(common_path))
    git_setup()

  res <- system(sprintf("git -C %s pull", common_path))
  if (res == 128) {
    check_git_credentials()
    stop("Problem with credentials or couldn't find git server.", call. = FALSE)
  }

  studies <- suppressMessages(
    readr::read_csv(file.path(path, "common/meta/StudyInfo.csv")))
  names(studies) <- tolower(names(studies))
  studies <- studies %>%
    dplyr::filter(program_folder == "HBGD" & !is.na(grant_folder)) %>%
    dplyr::mutate(short_id = tolower(short_id))

  # select(studies, grant_folder, study_id, short_id)

  nms <- names(studies)
  nms <- setdiff(nms, c("program_folder", "child_count", "mpc", "clinical"))
  nms <- nms[!grepl("inactive", nms)]
  studies <- dplyr::select(studies, dplyr::one_of(nms)) %>%
    dplyr::filter(status != "Obsolete")
  studies$fstudy_id <- fix_study_id(studies$study_id)
  studies
}

#' Get data for a given study ID
#'
#' @param id the study ID - this can either be a short ID or long ID which are found in the ID columns of the result of \code{\link{get_study_list}} or \code{\link{get_study_list_anthro}}
#' @param defin boolean specifying whether to read in the data definition instead of the data - default is FALSE
#' @param guess_max parameter passed on to \code{\link[readr]{read_csv}}
#'
#' @details This function takes a study ID and if there is not a git repository checked out for it, it will check out the respository and read and return the appropriate data file. If the repository is checked out, it will pull any updates to the data and then read and return the appropriate data file. In the case of study IDs associated with \code{\link{get_study_list_anthro}}, the correct data will be returned. In the case of study IDs not in this list but in \code{\link{get_study_list}}, a guess will be made as to which data file is appropriate.
#'
#' @importFrom utils tail
#' @importFrom R.utils countLines
#' @export
#' @examples
#' \dontrun{
#' studies <- get_study_list_anthro()
#' wsb <- use_study("wsb")
#' }
use_study <- function(id, defin = FALSE, guess_max = -1) {
  path <- get_git_base_path()

  studies <- get_study_list()
  idx <- which(studies$study_id == id | studies$fstudy_id == id)
  if (length(idx) == 0) {
    idx <- which(studies$short_id == id)
    if (length(idx) == 0)
      stop_nice("A study with id '", id, "' could not be found.")
  } else if (length(idx) > 1) {
    message("More than one study entry was found with id ", id, ". Using the last one (please check to make sure this is correct.)")
    idx <- utils::tail(idx, 1)
  }

  grant_folder <- studies$grant_folder[idx]
  update_repo(grant_folder)

  visapps <- get_study_list_anthro()
  va_idx <- which(visapps$study_id == studies$study_id[idx])

  if (defin) {
    defdat <- NULL
    if (length(va_idx) > 0) {
      def_path <- file.path(path, "hbgd", grant_folder,
        gsub("\\\\", "/", visapps$defin_path[va_idx]))
      if (file.exists(def_path)) {
        message("Reading ", def_path)
        defdat <- suppressMessages(readr::read_csv(def_path))
        names(defdat) <- tolower(names(defdat))
        defdat$name <- tolower(defdat$name)
      }
    }

    if (is.null(defdat))
      message("Definition data was not read in...")

    return(defdat)
  }

  if (length(va_idx) > 0) {
    dat_path <- file.path(path, "hbgd", grant_folder,
      gsub("\\\\", "/", visapps$data_path[va_idx]))
    if (!file.exists(dat_path)) {
      tmp <- list.files(dirname(dat_path), full.names = TRUE)
      tmpidx <- which(tolower(tmp) == tolower(dat_path))
      if (length(tmpidx) == 1) {
        dat_path <- tmp[tmpidx]
      } else {
        dat_path <- NULL
      }
    }
  } else {
    analysis_folder <- studies$analysis_folder[idx]

    study_path <- file.path(path, "hbgd", grant_folder, analysis_folder, "adam")
    ff <- list.files(study_path, pattern = "csv$")
    ffl <- tolower(ff)
    idx <- which(grepl("_defn", ffl))

    if (length(idx) == 1) {
      newf <- gsub("_defn", "", ffl[idx])
      if (newf %in% ffl) {
        dat_path <- file.path(study_path, ff[which(ffl == newf)])
      } else {
        idx <- which.max(file.info(file.path(study_path, ff))$size)
        message("Guessing which csv file to read: ", ff[idx])
        message("It is advised to look around the repository to see ",
          "if a different file is more appropriate.")
        dat_path <- file.path(study_path, ff[idx])
      }
    }
  }

  message("Reading ", dat_path)
  if (identical(guess_max, -1)) {
    guess_max <- R.utils::countLines(dat_path)
  }
  d <- suppressMessages(readr::read_csv(dat_path, guess_max = guess_max))
  names(d) <- tolower(names(d))

  # some studies (tanzaniachild2) have this issue:
  d$sex[d$sex == "male"] <- "Male"
  d$sex[d$sex == "female"] <- "Female"

  # some studies (tdc) have this issue:
  d <- dplyr::filter(d, !is.na(subjid))

  # some studies (bogalusa) have this issue: (some NA studyid)
  d$studyid <- d$studyid[1]

  d
}

#' Get the path for the Vis-AdHocs repository
#'
#' @param pull should a pull be made on the repository while getting the path?
#'
#' @export
get_adhocs_path <- function(pull = FALSE) {
  path <- get_git_base_path()

  adhocs_path <- file.path(path, "Vis-AdHocs")

  if (!dir.exists(adhocs_path))
    git_adhocs_setup()

  if (pull) {
    res <- system(sprintf("git -C %s pull", adhocs_path))
    if (res == 128) {
      check_git_credentials()
      stop("Problem with credentials or couldn't find git server.", call. = FALSE)
    }
  }

  adhocs_path
}

#' Check out the Vis-AdHocs repository in the correct location in the git directory
#'
#' @export
git_adhocs_setup <- function() {
  path <- get_git_base_path()

  adhocs_path <- file.path(path, "Vis-AdHocs")
  if (!dir.exists(adhocs_path)) {
    dir.create(adhocs_path, showWarnings = FALSE)
    message("Checking out 'Vis-AdHocs' repo...")
    res <- system(sprintf("git clone https://git.ghap.io/stash/scm/hbgd-teams/Vis-AdHocs.git %s",
      adhocs_path))
    if (res == 128) {
      check_git_credentials()
      stop("Problem with credentials or couldn't find git server.", call. = FALSE)
    }
  }
}

#' Find the local base git path
#'
#' @export
get_git_base_path <- function() {
  path <- getOption("GHAP_GIT_BASE_PATH")
  if (is.null(path) || path == "") {
    path <- Sys.getenv("GHAP_GIT_BASE_PATH")
    if (is.null(path) || path == "")
      stop_nice("The environment variable 'GHAP_GIT_BASE_PATH' couldn't ",
        "be found. If you want to use git sources on GHAP, please set this using ",
        "set_git_base_path(). This is where all the git data repos will be checked out.")
  }
  path
}

#' Clean up a study ID that may contain non-standard characters
#'
#' @param x a character or character vector to "fix"
#'
#' @details This simply converts any non-alphanumeric character to "_", which is a useful conversion for file names, etc.
#'
#' @export
fix_study_id <- function(x) {
  x <- tolower(x)
  gsub("[^a-z0-9]", "_", x)
}

#' Set git credentials
#'
#' @note This function is usually called by other functions so it will rarely have need to be called explicitly.
#'
#' @export
set_git_credentials <- function() {
  # message("* Note: Your password will be stored in files ~/.netrc and ~/.git-credentials")
  # message("and will be stored as plain text. Since you are the only user")
  # message("of this machine, that should be fine. Just be aware.")

  # user <- readline(prompt = "Enter username: ")
  # pass <- getPass::getPass(msg = "Enter password: ", forcemask = FALSE)

  # res <- sprintf("machine git.ghap.io\nlogin %s\npassword %s\nprotocol https\n",
  #   user, pass)
  # cat(res, file = "~/.netrc")

  # res2 <- sprintf("https://%s:%s@git.ghap.io\n", user, pass)
  # cat(res2, file = "~/.git-credentials")
  system("git config --global credential.helper 'cache --timeout 43200'")
  system("chmod 0700 ~/.git-credential-cache")
}

#' Find the local base git path and clone it if it isn't there
#'
#' @note This function is usually called by other functions so it will rarely have need to be called explicitly.
#'
#' @export
git_setup <- function() {
  path <- get_git_base_path()

  common_path <- file.path(path, "common")
  if (!dir.exists(common_path)) {
    dir.create(common_path, showWarnings = FALSE)
    message("Checking out 'common' repo...")
    res <- system(sprintf("git clone https://git.ghap.io/stash/scm/common/common.git %s",
      common_path))
    if (res == 128) {
      check_git_credentials()
      stop("Problem with credentials or couldn't find git server.", call. = FALSE)
    }
  }

  hbgdc_path <- file.path(path, "hbgd/common")
  if (!dir.exists(hbgdc_path)) {
    dir.create(hbgdc_path, recursive = TRUE, showWarnings = FALSE)
    message("Checking out 'hbgd/common' repo...")
    res <- system(sprintf("git clone https://git.ghap.io/stash/scm/hbgd/common.git %s",
      hbgdc_path))
    if (res == 128) {
      check_git_credentials()
      stop("Problem with credentials or couldn't find git server.", call. = FALSE)
    }
  }

  message("Git directory is ready to go!")
  message("To start checking out some data repos, try get_study_list() ",
    " to find what repo you are interested in, and then pass the corresponding ",
    "'study_id' or 'short_id' into use_study().")
}


#### internal ####

update_repo <- function(grant_folder) {
  path <- get_git_base_path()

  repo_path <- file.path(path, "hbgd", grant_folder)
  if (!dir.exists(repo_path)) {
    dir.create(repo_path, recursive = TRUE, showWarnings = FALSE)
    message("Checking out 'hbgd/", grant_folder, "' repo...")
    nm <- paste0("https://git.ghap.io/stash/scm/hbgd/", grant_folder, ".git")
    res <- system(sprintf("git clone %s %s", nm, repo_path))
    if (res == 128) {
      check_git_credentials()
      unlink(repo_path, recursive = TRUE)
      return (FALSE)
    }
  } else {
    message("Checking for updates to hbgd/", grant_folder, "'")
    res <- system(sprintf("git -C %s pull", repo_path))
    if (res == 128)
      check_git_credentials()
  }
  TRUE
}

# called when there is an error with a git command
check_git_credentials <- function() {
  if (is_linux()) {
    has_creds <- TRUE
    if (!file.exists("~/.netrc") || !file.exists("~/.git-credentials")) {
      has_creds <- FALSE
    } else {
      tmp <- readLines("~/.netrc")
      if (!any(grepl("machine git\\.ghap\\.io", tmp)))
        has_creds <- FALSE
    }
    if (!has_creds)
      set_git_credentials()
  } else {
    msg <- strwrap(paste(
      "There was an error running the git command.",
      "It could have to do with credentials.",
      "To set up credentials on Windows, you may need to do a one-time manual checkout",
      "of a ghap repository."))
    message(paste(msg, collapse = "\n"))
  }
}

stop_nice <- function(...) {
  stop(paste(strwrap(paste(...), exdent = 7), collapse = "\n"), call. = FALSE)
}

is_linux <- function() Sys.info()["sysname"] == "Linux"
