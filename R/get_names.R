#' Get path and file names for PortfolioTracker
#'
#' @usage get_names(path)
#' @param path A single character string. A directory where all data are stored.
#'
#' @export
get_names <- function(path) {

  get_path_names(path)
  get_file_names()

}
