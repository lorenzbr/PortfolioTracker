#' Get path and file names for PortfolioTracker
#'
#' @usage get_names(path)
#' @param path A single character string. A directory where all data are stored.
#' @return \code{get_names} returns a list with path and file names.
#'
#' @export
get_names <- function(path) {

  list.paths <- get_path_names(path)
  list.files <- get_file_names(path)

  output.list <- c(list.paths, list.files)

  return(output.list)

}
