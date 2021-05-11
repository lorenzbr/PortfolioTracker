#' Get ISINs with missing tickers
#'
#' @usage get_isins_missing_tickers(path)
#' @param path A single character string. Directory of your data.
#' @return df.isins.missing.tickers A data frame with ISINS for which a ticker was not found.
#'
#' @export
get_isins_missing_tickers <- function(path){

  file.ticker = "isin_ticker.csv"
  file.transactions = "transaction_fullhistory.csv"
  list.paths <- PortfolioTracker::create_portfoliotracker_dir(path)
  path.tickers <- list.paths$path.tickers
  path.transactions <- list.paths$path.transactions

  ## get table that converts ISIN to ticker (which is needed by Yahoo Finance)
  df.isin.ticker <- data.table::fread(paste0(path.tickers, file.ticker))
  df.transaction.history <- data.table::fread(paste0(path.transactions, file.transactions))

  ## missing tickers
  df.missings <- dplyr::anti_join(df.transaction.history, df.isin.ticker, by = "isin")

  df.missings <- df.missings[, c("isin", "wkn", "name")]

  return(df.missings)

} ## end of function get_isins_missing_tickers