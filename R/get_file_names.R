#' Get file names for PortfolioTracker
#'
#' @usage get_file_names(path)
#' @param path A single character string. A directory where all data are stored.
#' @return \code{get_file_names} returns a list with file names.
#'
#' @export
get_file_names <- function(path){

  file.transactions <- "transaction_fullhistory.csv"
  file.current <- "current_portfolio.csv"
  file.previous = "previous_investments.csv"
  file.tickers <- "isin_ticker.csv"
  file.dividend.history <- "dividends_fullhistory.csv"
  file.target.shares <- "default_target_shares.csv"
  file.stats <- "portfolio_stats.csv"


  output.list <- list(
    file.transactions = file.transactions,
    file.current = file.current,
    file.previous = file.previous,
    file.tickers = file.tickers,
    file.dividend.history = file.dividend.history,
    file.target.shares = file.target.shares,
    file.stats = file.stats
  )

  return(output.list)

} ## end of function get_file_names
