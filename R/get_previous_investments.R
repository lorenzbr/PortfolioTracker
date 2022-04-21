#' Get previous investments
#'
#' @usage get_previous_investments(path)
#' @param path A single character string. Directory of your data.
#'
#' @return A data frame containing current portfolio.
#'
#' @export
get_previous_investments <- function(path) {

  PortfolioTracker::get_user_names(path)

  path.data <- gsub("/$", "", path.data)
  path.data <- gsub("//", "/", path.data)

  if (file.exists(file.path(path.data, file.previous))) {

    df_previous <- data.table::fread(file.path(path.data, file.previous))

    df_previous$name <- PortfolioTracker::clean_investment_names(df_previous$name)

    return(df_previous)

  }

}
