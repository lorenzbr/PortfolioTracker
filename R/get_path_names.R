#' Get path names for PortfolioTracker
#'
#' @usage get_path_names(path)
#' @param path A single character string. A directory where all data are stored.
#' @return \code{get_path_names} returns a list with path names.
#'
#' @export
get_path_names <- function(path){

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
    path.crypto = path.crypto
  )

  return(output.list)

} ## end of function get_path_names
