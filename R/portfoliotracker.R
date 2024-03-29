#' \code{PortfolioTracker} package
#'
#' Track Your Investment Portfolio
#'
#' See the README on
#' \href{https://github.com/lorenzbr/PortfolioTracker#readme}{GitHub}
#'
#' @docType package
#' @name PortfolioTracker
NULL

if (getRversion() >= "2.15.1") {

  utils::globalVariables(
    c(
     ## Database paths
     "path.root",
     "path.user.credentials",
     "path.user.data",
     "path.database",
     "path.logs",
     "path.prices.db",
     "path.tickers.db",
     ## User-specific paths
     "path.data",
     "path.tickers",
     "path.prices.raw",
     "path.quantity.panel",
     "path.price.panel",
     "path.complete.panel",
     "path.pricequantity.panel",
     "path.dividends",
     "path.transactions",
     "path.rebalance",
     "path.returns",
     "path.returns.roi",
     "path.cash",
     "path.crypto",
     "path.value.panel",
     ## User meta data file names
     "file.cookies",
     "file.user.db",
     "file.update.log",
     "file.transactions.log",
     "file.tickers.db.log",
     "file.stock.splits.log",
     ## Database file names
     "file.tickers.db",
     "file.ticker.exchange.db",
     "file.stock.splits.db",
     "file.ticker.price.available.db",
     ## User-specific file names
     "file.transactions",
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
     "file.stock.splits.current",
     ## Other variables
     "transaction_date"
     )
   )

}
