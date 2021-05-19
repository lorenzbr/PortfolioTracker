#' Create directory for PortfolioTracker
#'
#' @usage create_portfoliotracker_dir(path)
#' @param path A single character string. Folder where all data are stored.
#' @return create_portfoliotracker_dir returns a list with path names for data storage
#'
#' @export
create_portfoliotracker_dir <- function(path) {

  path.data <- paste0(path, "/data/")
  path.tickers <- paste0(path, "/data/tickers/")
  path.prices.raw <- paste0(path, "/data/prices_raw/")
  path.quantitypanel <- paste0(path, "/data/quantity_panel/")
  path.pricepanel <- paste0(path, "/data/price_panel/")
  path.pricequantitypanel <- paste0(path, "/data/price_quantity_panel/")
  path.dividends <- paste0(path, "/data/dividends/")
  path.transactions <- paste0(path, "/data/transactions/")
  path.rebalance <- paste0(path, "/data/rebalance/")
  path.returns <- paste0(path, "/data/returns/")
  path.fiat <- paste0(path, "/data/currencies/fiat/")
  path.crypto <- paste0(path, "/data/currencies/crypto")
  path.value.panel <- paste0(path, "/data/value_panel/")

  ## create folders for tickers and prices (if not exists)
  if (!dir.exists(path.data)) dir.create(path.data, recursive = TRUE)
  if (!dir.exists(path.tickers)) dir.create(path.tickers, recursive = TRUE)
  if (!dir.exists(path.prices.raw)) dir.create(path.prices.raw, recursive = TRUE)
  if (!dir.exists(path.quantitypanel)) dir.create(path.quantitypanel, recursive = TRUE)
  if (!dir.exists(path.pricepanel)) dir.create(path.pricepanel, recursive = TRUE)
  if (!dir.exists(path.pricequantitypanel)) dir.create(path.pricequantitypanel, recursive = TRUE)
  if (!dir.exists(path.dividends)) dir.create(path.dividends, recursive = TRUE)
  if (!dir.exists(path.transactions)) dir.create(path.transactions, recursive = TRUE)
  if (!dir.exists(path.rebalance)) dir.create(path.rebalance, recursive = TRUE)
  if (!dir.exists(path.returns)) dir.create(path.returns, recursive = TRUE)
  if (!dir.exists(path.fiat)) dir.create(path.fiat, recursive = TRUE)
  if (!dir.exists(path.crypto)) dir.create(path.crypto, recursive = TRUE)
  if (!dir.exists(path.value.panel)) dir.create(path.value.panel, recursive = TRUE)

  output.list <- list(
    path.data = path.data,
    path.tickers = path.tickers,
    path.prices.raw = path.prices.raw,
    path.quantitypanel = path.quantitypanel,
    path.pricepanel = path.pricepanel,
    path.pricequantitypanel = path.pricequantitypanel,
    path.dividends = path.dividends,
    path.transactions = path.transactions,
    path.rebalance = path.rebalance,
    path.returns = path.returns,
    path.fiat = path.fiat,
    path.crypto = path.crypto,
    path.value.panel = path.value.panel
  )

  return(output.list)

}
