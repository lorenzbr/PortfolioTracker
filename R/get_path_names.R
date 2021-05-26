#' Get path names for PortfolioTracker
#'
#' @usage get_path_names(path)
#' @param path A single character string. A directory where all data are stored.
#'
#' @export
get_path_names <- function(path) {

  assign("path.data", paste0(path, "/data/"), envir = .GlobalEnv)
  assign("path.tickers", paste0(path, "/data/tickers/"), envir = .GlobalEnv)
  assign("path.prices.raw", paste0(path, "/data/prices_raw/"), envir = .GlobalEnv)
  assign("path.quantity.panel", paste0(path, "/data/quantity_panel/"), envir = .GlobalEnv)
  assign("path.price.panel", paste0(path, "/data/price_panel/"), envir = .GlobalEnv)
  assign("path.pricequantity.panel", paste0(path, "/data/price_quantity_panel/"), envir = .GlobalEnv)
  assign("path.dividends", paste0(path, "/data/dividends/"), envir = .GlobalEnv)
  assign("path.transactions", paste0(path, "/data/transactions/"), envir = .GlobalEnv)
  assign("path.rebalance", paste0(path, "/data/rebalance/"), envir = .GlobalEnv)
  assign("path.returns", paste0(path, "/data/returns/"), envir = .GlobalEnv)
  assign("path.returns.roi", paste0(path.returns, "/roi/"), envir = .GlobalEnv)
  assign("path.fiat", paste0(path, "/data/currencies/fiat/"), envir = .GlobalEnv)
  assign("path.crypto", paste0(path, "/data/currencies/crypto/"), envir = .GlobalEnv)
  assign("path.value.panel", paste0(path, "/data/value_panel/"), envir = .GlobalEnv)
  assign("path.complete.panel", paste0(path, "/data/complete_panel/"), envir = .GlobalEnv)

}
