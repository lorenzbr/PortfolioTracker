#' Create main directory for the Portfolio Tracker
#'
#' @usage create_main_dir(path)
#' @param path A single character string. Path where data are stored.
#' @return Returns a list with path names for data storage
#'
#' @export
create_main_dir <- function(path) {

  path.root <- file.path(path, "data")
  path.user.credentials <- file.path(path.root, "user_credentials")
  path.user.data <- file.path(path.root, "user_data")
  path.database <- file.path(path.root, "database")
  path.prices.db <- file.path(path.database, "prices")

  ## Create folders for tickers and prices (if not yet exists)
  folders <- c(path.root, path.user.credentials, path.user.data,
               path.database, path.prices.db)
  for (folder in folders)
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

}

#' Create directory for user
#'
#' @usage create_user_dir(path, portfolio_name = "portfolio_1")
#' @param path A single character string. Path where user data are stored.
#' @param portfolio_name A single character string indicating the name of the
#' portfolio. Currently the default is \emph{portfolio_1}.
#' @return Returns a list with path names for user-specific data storage
#'
#' @export
create_user_dir <- function(path, portfolio_name = "portfolio_1") {

  ## Name of user's portfolio
  ## Currently the name is fixed to "portfolio_1" (currently cannot be seen in UI)

  path <- file.path(path, portfolio_name)

  path.data <- path
  path.tickers <- file.path(path, "tickers")
  path.prices.raw <- file.path(path, "prices_raw")
  path.quantity.panel <- file.path(path, "quantity_panel")
  path.price.panel <- file.path(path, "price_panel")
  path.pricequantity.panel <- file.path(path, "price_quantity_panel")
  path.dividends <- file.path(path, "dividends")
  path.transactions <- file.path(path, "transactions")
  path.rebalance <- file.path(path, "rebalance")
  path.returns <- file.path(path, "returns")
  path.returns.roi <- file.path(path.returns, "roi")
  path.cash <- file.path(path, "cash/")
  path.crypto <- file.path(path, "crypto")
  path.value.panel <- file.path(path, "value_panel")
  path.complete.panel <- file.path(path, "complete_panel")

  ## Create folders for tickers and prices (if not yet exists)
  folders <- c(path.data, path.tickers, path.prices.raw, path.quantity.panel,
               path.price.panel, path.pricequantity.panel, path.dividends,
               path.transactions, path.rebalance, path.returns, path.returns.roi,
               path.cash, path.crypto, path.value.panel, path.complete.panel)
  for (folder in folders)
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

}
