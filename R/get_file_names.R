#' Get file names for PortfolioTracker
#'
#' @usage get_file_names()
#'
#' @export
get_file_names <- function() {

  ## Solution from https://github.com/josephguillaume/hydromad/issues/73
  pos <- 1
  envir <- as.environment(pos)

  df <- data.frame(var_name = c("file.transactions",
                                "file.current",
                                "file.previous",
                                "file.tickers",
                                "file.ticker.exchange",
                                "file.dividend.history",
                                "file.dividend.year",
                                "file.dividend.month",
                                "file.target.shares",
                                "file.stats",
                                "file.returns.daily",
                                "file.returns.monthly",
                                "file.returns.annual",
                                "file.returns.annualized",
                                "file.return.portfolio.daily",
                                "file.returns.twr.daily",
                                "file.returns.irr"
                                ),
                   file_name = c("transaction_fullhistory.csv",
                                 "current_portfolio.csv",
                                 "previous_investments.csv",
                                 "isin_ticker.csv",
                                 "ticker_exchange.csv",
                                 "dividends_fullhistory.csv",
                                 "dividends_by_year.csv",
                                 "dividends_by_month.csv",
                                 "default_target_shares.csv",
                                 "portfolio_stats.csv",
                                 "daily_returns.csv",
                                 "monthly_returns.csv",
                                 "annual_returns.csv",
                                 "annualized_returns.csv",
                                 "daily_portfolio_return.csv",
                                 "twr_portfolio_factors_daily.csv",
                                 "investment_irr.csv"
                                 )
                   )

  for( i in 1:nrow(df) ) {

    assign(df[i, 1], df[i, 2], envir = envir)

  }

}
