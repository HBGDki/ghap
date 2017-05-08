#' Save a plot
#'
#' @param p specification (lattice object, ggplot2 object, or expression of arbitrary R code)
#' @param filename where to save the file
#' @param width (in pixels)
#' @param height (in pixels)
#' @param type type of file ("png" or "pdf")
#' @param \ldots additional parameters passed to graphics device
#'
#' @details All GHAP repositories are automatically organized and stored in a directory structure under a base git path which is specified by this function. This function will set an R environment variable that will be present in all subsequent sessions such that you only need to specify the base path once.
#'
#' @export
make_plot <- function(p, filename, width = 800, height = 800, type = "png", ...) {
  if (!inherits(p, c("trellis", "ggplot", "expression")))
    stop("Input must be of class 'trellis' or 'ggplot'")
  if (type == "png") {
    grDevices::png(filename, width = width * 2, height = height * 2, res = 150, ...)
  } else if (type == "pdf") {
    grDevices::pdf(filename, width = width / 72, height = height / 72, ...)
  } else {
    stop("type must 'pdf' or 'png'")
  }
  if (is.expression(p)) {
    eval(p)
  } else {
    print(p)
  }
  grDevices::dev.off()
}

#' Save a plot and upload it to a project on OSF (osf.io)
#'
#' @param p specification (lattice object, ggplot2 object, or expression of arbitrary R code)
#' @param filename where to save the file
#' @param width (in pixels)
#' @param height (in pixels)
#' @param type type of file ("png" or "pdf")
#' @param osf_id id of the project on OSF (osf.io/xxxx)
#' @param osf_folder folder within the project to save the file (file will have same base name as \code{filename})
#' @param \ldots additional parameters passed to graphics device
#'
#' @details All GHAP repositories are automatically organized and stored in a directory structure under a base git path which is specified by this function. This function will set an R environment variable that will be present in all subsequent sessions such that you only need to specify the base path once.
#'
#' @export
#' @importFrom osfr upload_file
save_and_upload <- function(p, filename, width = 800, height = 800, type = "png",
  osf_id, osf_folder = "plots", ...) {
  if (basename(dirname(filename)) != "plots")
    stop("Expecting the file to go in a 'plots' directory")
  make_plot(p, filename, width = width, height = height, type = "png", ...)
  message("Plot ", basename(filename), " created")
  osfr::upload_file(osf_id, filename, paste0(osf_folder, "/", basename(filename)))
  message("Plot ", basename(filename), " added to OSF")
}
