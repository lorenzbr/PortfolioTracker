#' Get default target shares
#'
#' @usage get_default_target_shares(path)
#' @param path A single character string. Folder where all data are stored.
#'
#' @return A data frame containing a column for \code{isin} (character)
#' and \code{target_share} (numeric)
#'
#' @export
get_default_target_shares <- function(path) {

  PortfolioTracker::get_user_names(path)

  file_path_target_shares <- file.path(path.rebalance, file.target.shares)

  if (file.exists(file_path_target_shares)) {

    df_target_shares <- data.table::fread(file_path_target_shares)

  } else {

    df_target_shares <- NULL

  }

  return(df_target_shares)

}
