#' Get file names for PortfolioTracker
#'
#' @usage get_file_names()
#'
#' @export
get_file_names <- function() {

  ## Solution from https://github.com/josephguillaume/hydromad/issues/73
  pos <- 1
  envir = as.environment(pos)

  # df <- data.frame(var_name = c("file.transactions",
  #                               "file.current"),
  #                  file_name = c("transaction_fullhistory.csv",
  #                                "current_portfolio.csv")
  #                  )
  #
  # for(row in df) {
  #
  #   assign(row[1], row[2], envir = envir)
  #
  # }

  # file.transactions <- "transaction_fullhistory.csv"
  assign("file.transactions", "transaction_fullhistory.csv", envir = envir)
  # file.current <<- "current_portfolio.csv"
  assign("file.current", "current_portfolio.csv", envir = envir)
  assign("file.previous", "previous_investments.csv", envir = envir)
  assign("file.tickers", "isin_ticker.csv", envir = envir)
  assign("file.ticker.exchange", "ticker_exchange.csv", envir = envir)
  assign("file.dividend.history", "dividends_fullhistory.csv", envir = envir)
  assign("file.dividend.year", "dividends_by_year.csv", envir = envir)
  assign("file.dividend.month", "dividends_by_month.csv", envir = envir)
  assign("file.target.shares", "default_target_shares.csv", envir = envir)
  assign("file.stats", "portfolio_stats.csv", envir = envir)
  assign("file.returns.daily", "daily_returns.csv", envir = envir)
  assign("file.returns.monthly", "monthly_returns.csv", envir = envir)
  assign("file.returns.annual", "annual_returns.csv", envir = envir)
  assign("file.returns.annualized", "annualized_returns.csv", envir = envir)
  assign("file.return.portfolio.daily", "daily_portfolio_return.csv",
         envir = envir)
  assign("file.returns.twr.daily", "twr_portfolio_factors_daily.csv",
         envir = envir)

}
