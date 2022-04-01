#' Get ISINs with missing tickers
#'
#' @usage get_isins_missing_tickers(path)
#' @param path A single character string. Directory of your data.
#'
#' @return A data frame with ISINS for which a ticker was not found.
#'
#' @export
get_isins_missing_tickers <- function(path){

  get_user_names(path)

  transaction_history_exists <- file.exists(file.path(path.transactions,
                                                      file.transactions))
  isin_ticker_exists <- file.exists(file.path(path.tickers, file.tickers))

  if (transaction_history_exists && isin_ticker_exists) {

    ## Get table that converts ISIN to ticker (which is needed by Yahoo Finance)
    df_isin_ticker <- data.table::fread(file.path(path.tickers, file.tickers))
    df_isin_ticker <- df_isin_ticker[df_isin_ticker$ticker != "", ]
    df_transaction_history <- data.table::fread(file.path(path.transactions,
                                                          file.transactions))

    df_missings <- dplyr::anti_join(df_transaction_history,
                                    df_isin_ticker,
                                    by = "isin")

    df_missings <- df_missings[, c("isin", "wkn", "name")]

    df_missings <- unique(df_missings)

    df_missings$wkn[is.na(df_missings$wkn)] <- "-"

    return(df_missings)

  }

}
