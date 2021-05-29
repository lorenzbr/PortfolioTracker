#' Create directory for PortfolioTracker
#'
#' @usage create_portfoliotracker_dir(path)
#' @param path A single character string. Path where data are stored.
#' @return create_portfoliotracker_dir returns a list with path names for data storage
#'
#' @export
create_portfoliotracker_dir <- function(path) {

  path.data <- paste0(path, "/data/")
  path.tickers <- paste0(path, "/data/tickers/")
  path.prices.raw <- paste0(path, "/data/prices_raw/")
  path.quantity.panel <- paste0(path, "/data/quantity_panel/")
  path.price.panel <- paste0(path, "/data/price_panel/")
  path.pricequantity.panel <- paste0(path, "/data/price_quantity_panel/")
  path.dividends <- paste0(path, "/data/dividends/")
  path.transactions <- paste0(path, "/data/transactions/")
  path.rebalance <- paste0(path, "/data/rebalance/")
  path.returns <- paste0(path, "/data/returns/")
  path.returns.roi <- paste0(path.returns, "/roi/")
  path.fiat <- paste0(path, "/data/currencies/fiat/")
  path.crypto <- paste0(path, "/data/currencies/crypto")
  path.value.panel <- paste0(path, "/data/value_panel/")
  path.complete.panel <- paste0(path, "/data/complete_panel/")

  ## create folders for tickers and prices (if not yet exists)
  if (!dir.exists(path.data)) dir.create(path.data, recursive = TRUE)
  if (!dir.exists(path.tickers)) dir.create(path.tickers, recursive = TRUE)
  if (!dir.exists(path.prices.raw)) dir.create(path.prices.raw, recursive = TRUE)
  if (!dir.exists(path.quantity.panel)) dir.create(path.quantity.panel, recursive = TRUE)
  if (!dir.exists(path.price.panel)) dir.create(path.price.panel, recursive = TRUE)
  if (!dir.exists(path.pricequantity.panel)) dir.create(path.pricequantity.panel, recursive = TRUE)
  if (!dir.exists(path.dividends)) dir.create(path.dividends, recursive = TRUE)
  if (!dir.exists(path.transactions)) dir.create(path.transactions, recursive = TRUE)
  if (!dir.exists(path.rebalance)) dir.create(path.rebalance, recursive = TRUE)
  if (!dir.exists(path.returns)) dir.create(path.returns, recursive = TRUE)
  if (!dir.exists(path.returns.roi)) dir.create(path.returns.roi, recursive = TRUE)
  if (!dir.exists(path.fiat)) dir.create(path.fiat, recursive = TRUE)
  if (!dir.exists(path.crypto)) dir.create(path.crypto, recursive = TRUE)
  if (!dir.exists(path.value.panel)) dir.create(path.value.panel, recursive = TRUE)
  if (!dir.exists(path.complete.panel)) dir.create(path.complete.panel, recursive = TRUE)

}
