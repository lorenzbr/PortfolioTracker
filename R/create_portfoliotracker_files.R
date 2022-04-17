#' Create main files of the Portfolio Tracker
#'
#' Create the main files of the Portfolio Tracker. Create files for cookies,
#' user database,
#'
#' @usage create_main_files(path)
#' @param path A single character string. Path where files are created.
#'
#' @export
create_main_files <- function(path) {

  get_db_names(path = path)

  file_path_cookies <- file.path(path.user.credentials, file.cookies)
  if (!file.exists(file_path_cookies)) {
    df_cookies <- data.frame(
      matrix(nrow = 0, ncol = 2,
             dimnames = list(NULL, c("user_name", "sessionid")))
    )
    data.table::fwrite(df_cookies, file_path_cookies)
  }

  file_path_user_db <- file.path(path.user.credentials, file.user.db)
  if (!file.exists(file_path_user_db)) {
    df_user_base <- data.frame(
      matrix(nrow = 0, ncol = 2,
             dimnames = list(NULL, c("user_name", "password")))
    )
    data.table::fwrite(df_user_base, file_path_user_db)
  }

  file_path_update_log <- file.path(path.user.credentials, file.update.log)
  if (!file.exists(file_path_update_log)) {
    df_update_log <- data.frame(
      matrix(nrow = 0, ncol = 2,
             dimnames = list(NULL, c("user_name", "pt_update_date")))
    )
    data.table::fwrite(df_update_log, file_path_update_log)
  }

  file_path_tickers <- file.path(path.database, file.tickers.db)
  if (!file.exists(file_path_tickers)) {
    df_isin_ticker <- data.frame(
      matrix(nrow = 0, ncol = 2,
             dimnames = list(NULL, c("isin", "ticker")))
    )
    data.table::fwrite(df_isin_ticker, file_path_tickers)
  }

  file_path_ticker_exchange <- file.path(path.database, file.ticker.exchange.db)
  if (!file.exists(file_path_ticker_exchange)) {
    df_ticker_exchanges <- data.frame(
      matrix(nrow = 0, ncol = 2,
             dimnames = list(NULL, c("ticker", "exchange")))
    )
    data.table::fwrite(df_ticker_exchanges, file_path_ticker_exchange)
  }

  file_path_price_available <- file.path(path.database,
                                         file.ticker.price.available.db)
  if (!file.exists(file_path_price_available)) {
    df_price_range <- data.frame(
      matrix(nrow = 0, ncol = 3,
             dimnames = list(NULL, c("ticker", "first_date", "last_date")))
    )
    data.table::fwrite(df_price_range, file_path_price_available)
  }

  ## To do: create files for stock splits

}
