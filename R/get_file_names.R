#' Get file names for PortfolioTracker
#'
#' @usage get_file_names()
#'
#' @export
get_file_names <- function() {

  # file.transactions <- "transaction_fullhistory.csv"
  assign("file.transactions", "transaction_fullhistory.csv", envir = .GlobalEnv)
  # file.current <<- "current_portfolio.csv"
  assign("file.current", "current_portfolio.csv", envir = .GlobalEnv)
  assign("file.previous", "previous_investments.csv", envir = .GlobalEnv)
  assign("file.tickers", "isin_ticker.csv", envir = .GlobalEnv)
  assign("file.dividend.history", "dividends_fullhistory.csv", envir = .GlobalEnv)
  assign("file.dividend.year", "dividends_by_year.csv", envir = .GlobalEnv)
  assign("file.dividend.month", "dividends_by_month.csv", envir = .GlobalEnv)
  assign("file.target.shares", "default_target_shares.csv", envir = .GlobalEnv)
  assign("file.stats", "portfolio_stats.csv", envir = .GlobalEnv)
  assign("file.returns.daily", "daily_returns.csv", envir = .GlobalEnv)
  assign("file.returns.monthly", "monthly_returns.csv", envir = .GlobalEnv)
  assign("file.returns.annual", "annual_returns.csv", envir = .GlobalEnv)
  assign("file.returns.annualized", "annualized_returns.csv", envir = .GlobalEnv)
  assign("file.return.portfolio.daily", "daily_portfolio_return.csv", envir = .GlobalEnv)

}
