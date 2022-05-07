#' Get previous stock splits
#'
#' @usage get_previous_stock_splits(user_path)
#' @param user_path A single character string. Directory of user data.
#'
#' @return A data frame containing the previous stock splits
#'
#' @export
get_previous_stock_splits <- function(user_path) {

  PortfolioTracker::get_user_names(user_path)

  path_data <- gsub("/$", "", path.data)
  path_data <- gsub("//", "/", path_data)

  file_path_previous_stock_splits <- file.path(path_data, file.stock.splits.previous)

  if (file.exists(file_path_previous_stock_splits)) {

    df_splits <- data.table::fread(file_path_previous_stock_splits)

    # df_splits$name <- PortfolioTracker::clean_investment_names(df_splits$name)

    df_splits$stock_split <- as.numeric(df_splits$stock_split)

    return(df_splits)

  }

}


