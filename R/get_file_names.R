#' Get file names for files in shared database
#'
#' @usage get_db_file_names()
#'
#' @export
get_db_file_names <- function() {

  ## Solution from https://github.com/josephguillaume/hydromad/issues/73
  pos <- 1
  envir <- as.environment(pos)

  var_name <- c(
    "file.user.db",
    "file.cookies",
    "file.tickers.db",
    "file.ticker.exchange.db",
    "file.stock.splits.db",
    "file.ticker.price.available.db",
    "file.update.log",
    "file.transactions.log",
    "file.tickers.db.log",
    "file.stock.splits.log"
  )

  file_name <- c(
    "user_database_encoded.csv",
    "user_cookies.csv",
    "isin_ticker_db.csv",
    "ticker_exchange_db.csv",
    "stock_splits_db.csv",
    "ticker_price_available_db.csv",
    "user_update_log.csv",
    "transaction_history_log.csv",
    "isin_ticker_db_log.csv",
    "stock_splits_db_log.csv"
  )

  for (i in 1:length(var_name))
    assign(var_name[i], file_name[i], envir = envir)

}


#' Get file names for files in user directory
#'
#' @usage get_user_file_names()
#'
#' @export
get_user_file_names <- function() {

  ## Solution from https://github.com/josephguillaume/hydromad/issues/73
  pos <- 1
  envir <- as.environment(pos)

  var_name <- c("file.transactions",
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
                "file.returns.irr",
                "file.stock.splits.previous",
                "file.stock.splits.current"
  )

  file_name <- c("transaction_fullhistory.csv",
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
                 "investment_irr.csv",
                 "stock_splits_previous.csv",
                 "stock_splits_current.csv"
  )

  for (i in 1:length(var_name))
    assign(var_name[i], file_name[i], envir = envir)

}
