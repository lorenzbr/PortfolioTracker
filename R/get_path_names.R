#' Get path names for main directory
#'
#' @usage get_db_path_names(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
get_db_path_names <- function(path) {

  ## Solution from https://github.com/josephguillaume/hydromad/issues/73
  pos <- 1
  envir <- as.environment(pos)

  var_name <- c("path.root",
                "path.user.credentials",
                "path.user.data",
                "path.database",
                "path.prices.db"
  )

  file_name <- c("",
                 "user_credentials",
                 "user_data",
                 "database",
                 "database/prices"
  )

  for (i in 1:length(var_name))
    assign(var_name[i], file.path(path, "data", file_name[i]), envir = envir)

}

#' Get path names for user directory
#'
#' @usage get_user_path_names(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
get_user_path_names <- function(path) {

  ## Solution from https://github.com/josephguillaume/hydromad/issues/73
  pos <- 1
  envir <- as.environment(pos)

  var_name <- c("path.data",
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
  )

  file_name <- c("",
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

  for (i in 1:length(var_name))
    assign(var_name[i], file.path(path, "data", file_name[i]), envir = envir)

}
