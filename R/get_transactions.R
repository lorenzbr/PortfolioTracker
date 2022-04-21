#' Get transaction history
#'
#' @usage get_transaction_history(path)
#' @param path A single character string. Directory of your data.
#' @return A data frame containing current portfolio.
#'
#' @export
get_transaction_history <- function(path) {

  PortfolioTracker::get_user_names(path)

  file_path_transactions <- file.path(path.transactions, file.transactions)

  if (file.exists(file_path_transactions)) {

    try({

      df_transactions <- data.table::fread(file_path_transactions)

      df_transactions <- as.data.frame(df_transactions)

      df_transactions$name <- PortfolioTracker::clean_investment_names(df_transactions$name)

      df_transactions$transaction_date <- as.Date(
        df_transactions$transaction_date, "%d-%m-%Y")

      ## Order by date (most recent date at the top)
      df_transactions <- df_transactions[rev(order(df_transactions$transaction_date)), ]

      rownames(df_transactions) <- 1:nrow(df_transactions)

      df_transactions$transaction_price <- as.numeric(
        formatC(df_transactions$transaction_price, digits = 2, format = "f"))
      df_transactions$transaction_value <- as.numeric(
        formatC(df_transactions$transaction_value, digits = 2, format = "f"))
      df_transactions$transaction_fee <- as.numeric(
        formatC(df_transactions$transaction_fee, digits = 2, format = "f"))

      df_transactions <- df_transactions[, names(df_transactions) != "wkn"]
      df_transactions <- df_transactions[, names(df_transactions) != "document_page"]
      df_transactions <- df_transactions[, names(df_transactions) != "document_name"]

      df_transactions <- PortfolioTracker::clean_column_names(df_transactions)

      return(df_transactions)

    })

  }

}
