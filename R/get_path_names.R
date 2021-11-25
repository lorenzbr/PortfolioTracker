#' Get path names for PortfolioTracker
#'
#' @usage get_path_names(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
get_path_names <- function(path) {

  before <- Sys.time()

  ## Solution from https://github.com/josephguillaume/hydromad/issues/73
  pos <- 1
  envir <- as.environment(pos)

  df <- data.frame(var_name = c("path.data",
                                "path.tickers",
                                "path.prices.raw",
                                "path.quantity.panel",
                                "path.price.panel",
                                "path.pricequantity.panel",
                                "path.dividends",
                                "path.transactions",
                                "path.rebalance",
                                "path.returns",
                                "path.returns.roi",
                                "path.cash",
                                "path.crypto",
                                "path.value.panel",
                                "path.complete.panel"
                                ),
                   file_name = c("/",
                                 "tickers",
                                 "prices_raw",
                                 "quantity_panel",
                                 "price_panel",
                                 "price_quantity_panel",
                                 "dividends",
                                 "transactions",
                                 "rebalance",
                                 "returns",
                                 "returns/roi",
                                 "cash",
                                 "crypto",
                                 "value_panel",
                                 "complete_panel"
                                 )
                   )

  for( i in 1:nrow(df) ) {

    assign(df[i, 1], file.path(path, "data", df[i, 2]), envir = envir)

  }

  after <- Sys.time()
  after - before

}
