#' Get current portfolio
#'
#' @usage get_current_portfolio(path)
#' @param path A single character string. Directory of your data.
#'
#' @return A data frame containing the current investments.
#'
#' @export
get_current_portfolio <- function(path) {

  PortfolioTracker::get_user_names(path)

  path_data <- gsub("/$", "", path.data)
  path_data <- gsub("//", "/", path_data)

  file_path_current <- file.path(path_data, file.current)

  if (file.exists(file_path_current)) {

    df_all <- data.table::fread(file_path_current)

    df_all$name <- PortfolioTracker::clean_investment_names(df_all$name)

    df_all$weight <- as.numeric(df_all$weight)

    return(df_all)

  }

}


