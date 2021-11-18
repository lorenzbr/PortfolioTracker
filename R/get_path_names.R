#' Get path names for PortfolioTracker
#'
#' @usage get_path_names(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
get_path_names <- function(path) {

  ## Solution from https://github.com/josephguillaume/hydromad/issues/73
  pos <- 1
  envir = as.environment(pos)

  # df <- data.frame(var_name = c("path.data",
  #                               "path.tickers"),
  #                  file_name = c("",
  #                                "tickers")
  #                  )
  #
  # for(row in df) {
  #
  #   assign(row[1], file.path(path, "data", row[2]), envir = envir)
  #
  # }

  assign("path.data", paste0(path, "/data/"), envir = envir)
  assign("path.tickers", paste0(path, "/data/tickers/"), envir = envir)
  assign("path.prices.raw", paste0(path, "/data/prices_raw/"), envir = envir)
  assign("path.quantity.panel", paste0(path, "/data/quantity_panel/"),
         envir = envir)
  assign("path.price.panel", paste0(path, "/data/price_panel/"), envir = envir)
  assign("path.pricequantity.panel", paste0(path, "/data/price_quantity_panel/"),
         envir = envir)
  assign("path.dividends", paste0(path, "/data/dividends/"), envir = envir)
  assign("path.transactions", paste0(path, "/data/transactions/"), envir = envir)
  assign("path.rebalance", paste0(path, "/data/rebalance/"), envir = envir)
  assign("path.returns", paste0(path, "/data/returns/"), envir = envir)
  assign("path.returns.roi", paste0(path.returns, "/roi/"), envir = envir)
  assign("path.fiat", paste0(path, "/data/currencies/fiat/"), envir = envir)
  assign("path.crypto", paste0(path, "/data/currencies/crypto/"), envir = envir)
  assign("path.value.panel", paste0(path, "/data/value_panel/"), envir = envir)
  assign("path.complete.panel", paste0(path, "/data/complete_panel/"),
         envir = envir)

}
