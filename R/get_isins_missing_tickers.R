#' Get ISINs with missing tickers
#'
#' @usage get_isins_missing_tickers(path)
#' @param path A single character string. Directory of your data.
#' @return A data frame with ISINS for which a ticker was not found.
#'
#' @export
get_isins_missing_tickers <- function(path){

  get_user_names(path)

  transaction.history.exists <- file.exists(file.path(path.transactions, file.transactions))
  isin.ticker.exists <- file.exists(file.path(path.tickers, file.tickers))

  if (transaction.history.exists && isin.ticker.exists) {

    ## Get table that converts ISIN to ticker (which is needed by Yahoo Finance)
    df.isin.ticker <- data.table::fread(file.path(path.tickers, file.tickers))
    df.isin.ticker <- df.isin.ticker[df.isin.ticker$ticker != "", ]
    df.transaction.history <- data.table::fread(file.path(path.transactions, file.transactions))

    df.missings <- dplyr::anti_join(df.transaction.history, df.isin.ticker, by = "isin")

    df.missings <- df.missings[, c("isin", "wkn", "name")]

    df.missings <- unique(df.missings)

    df.missings$wkn[is.na(df.missings$wkn)] <- "-"

    return(df.missings)

  }

}
