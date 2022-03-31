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
#' @usage get_user_names(path, portfolio_name = "portfolio_1")
#' @param path A single character string. Path where user-specific data are stored.
#' @param portfolio_name A single character string with the name of the
#' portfolio. All portfolio-specific data for the focal user are stored in
#' a folder with this name. Default is \emph{portfolio_1}.
#'
#' @export
get_user_names <- function(path, portfolio_name = "portfolio_1") {

  get_user_path_names(path, portfolio_name)
  get_user_file_names()

}
