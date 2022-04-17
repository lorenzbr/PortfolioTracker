#' Get portfolio statistics
#'
#' @usage get_portfolio_stats(path)
#' @param path A single character string. Directory of the data.
#'
#' @return A data frame containing basic statistics of the portfolio.
#'
#' @export
get_portfolio_stats <- function(path) {

  PortfolioTracker::get_user_names(path)

  file_path_stats <- file.path(path.data, file.stats)

  if (file.exists(file_path_stats)) {

    df_stats <- data.table::fread(file_path_stats)

  } else {

    df_stats <- NULL

  }

  return(df_stats)

}
