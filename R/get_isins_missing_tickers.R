#' Get ISINs with missing tickers
#'
#' @usage get_isins_missing_tickers(user_path, db_path)
#' @param user_path A single character string containing the directory of the user.
#' @param db_path A single character string containing the directory of the database.
#'
#' @return A data frame with ISIN-ticker pairs for which a ticker was not found.
#'
#' @export
get_isins_missing_tickers <- function(user_path, db_path){

  get_user_names(user_path)
  get_db_names(db_path)

  file_path_transactions <- file.path(path.transactions, file.transactions)

  if (file.exists(file_path_transactions)) {

    ## Get table that converts ISIN to ticker (which is needed by Yahoo Finance)
    df_isin_ticker <- data.table::fread(file.path(path.database, file.tickers.db))
    df_isin_ticker <- df_isin_ticker[df_isin_ticker$ticker != "", ]

    df_transactions <- data.table::fread(file_path_transactions)

    if (nrow(df_transactions) && nrow(df_isin_ticker)) {

      df_missings <- dplyr::anti_join(df_transactions,
                                      df_isin_ticker,
                                      by = "isin")

      df_missings <- df_missings[, c("isin", "wkn", "name")]

      df_missings <- unique(df_missings)

      df_missings$wkn[is.na(df_missings$wkn)] <- "-"

    } else {

      df_missings <- as.data.frame(
        matrix(nrow = 0, ncol = 3,
               dimnames = list(NULL, c("isin", "wkn", "name"))))

    }

    return(df_missings)

  }

}
