#' Initiate Portfolio Tracker
#'
#' @description Initiate Portfolio Tracker (global variables of path names
#' and file names are loaded, directory with required folders are created)
#'
#' @usage init_portfolio_tracker(db_path = ".")
#' @param db_path A single character string containing the directory of the database.
#' Default is the current working directory.
#'
#' @export
init_portfolio_tracker <- function(db_path = ".") {

  get_db_names(path = db_path)
  create_main_dir(path = db_path)
  create_main_files(path = db_path)

}
