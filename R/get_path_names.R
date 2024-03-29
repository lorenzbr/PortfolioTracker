#' Get database path for main directory
#'
#' @usage get_db_main_path(db_path = ".")
#' @param db_path A single character string. Path where database is stored.
#'
#' @export
get_db_main_path <- function(db_path = ".") {
  return(db_path)
}

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
                "path.logs",
                "path.prices.db"
  )

  file_name <- c("",
                 "user_credentials",
                 "user_data",
                 "database",
                 "logs",
                 "database/prices"
  )

  for (i in 1:length(var_name))
    assign(var_name[i], file.path(path, "data", file_name[i]), envir = envir)

}

#' Get path names for user directory
#'
#' @usage get_user_path_names(path, portfolio_name = "portfolio_1")
#' @param path A single character string. Path where data are stored.
#' @param portfolio_name A single character string with the name of the
#' portfolio. All portfolio-specific data for the focal user are stored in
#' a folder with this name. Default is \emph{portfolio_1}.
#'
#' @export
get_user_path_names <- function(path, portfolio_name = "portfolio_1") {

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
    assign(var_name[i], file.path(path, portfolio_name,
                                  file_name[i]), envir = envir)

}
