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
  file.dividend.year <- "dividends_by_year.csv"
  file.dividend.month <- "dividends_by_month.csv"
  file.target.shares <- "default_target_shares.csv"
  file.stats <- "portfolio_stats.csv"
  file.returns.daily <- "daily_returns.csv"
  file.returns.monthly <- "monthly_returns.csv"
  file.returns.annual <- "annual_returns.csv"
  file.returns.annualized <- "annualized_returns.csv"
  file.return.portfolio.daily <- "daily_portfolio_return.csv"

  output.list <- list(
    file.transactions = file.transactions,
    file.current = file.current,
    file.previous = file.previous,
    file.tickers = file.tickers,
    file.dividend.history = file.dividend.history,
    file.dividend.year = file.dividend.year,
    file.dividend.month = file.dividend.month,
    file.target.shares = file.target.shares,
    file.stats = file.stats,
    file.returns.annual = file.returns.annual,
    file.returns.annualized = file.returns.annualized,
    file.return.portfolio.daily = file.return.portfolio.daily
  )

  return(output.list)

}
