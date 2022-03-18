#' Get path and file names for main directory
#'
#' @usage get_db_names(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
get_db_names <- function(path) {

  get_db_path_names(path)
  get_db_file_names()

}

#' Get user-specific path and file names
#'
#' @usage get_user_names(path)
#' @param path A single character string. Path where user-specific data are stored.
#'
#' @export
get_user_names <- function(path) {

  get_user_path_names(path)
  get_user_file_names()

}
