#' Write current portfolio investments to a csv file
#'
#' @usage write_current_portfolio(user_path, db_path)
#' @param user_path A single character string containing the directory of the user.
#' @param db_path A single character string containing the directory of the database.
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
write_current_portfolio <- function(user_path, db_path) {

  get_user_names(user_path)
  get_db_names(db_path)

  files_pricequantity_panel <- list.files(path.pricequantity.panel)

  if (length(files_pricequantity_panel) > 0) {

    files <- file.path(path.pricequantity.panel, files_pricequantity_panel)
    list_dfs <- lapply(files, data.table::fread)

    file_path_transactions <- file.path(path.transactions, file.transactions)

    if (file.exists(file_path_transactions)) {

      ## Get table that converts ISIN to ticker (which is needed by Yahoo Finance)
      df_isin_ticker <- data.table::fread(file.path(path.database, file.tickers.db))

      df_transactions <- data.table::fread(file_path_transactions)
      df_ticker_names <- unique(df_transactions[, c("isin",
                                                    "name",
                                                    "transaction_date")])

      ## Use name from most recent transaction
      df_ticker_names$date <- as.Date(
        df_ticker_names$transaction_date, format = "%d-%m-%Y")
      df_ticker_names <- df_ticker_names %>%
        dplyr::group_by(.data$isin) %>%
        dplyr::filter(date == max(.data$date))
      df_ticker_names <- df_ticker_names %>%
        dplyr::group_by(.data$isin) %>%
        dplyr::sample_n(size = 1)

      df_ticker_names <- merge(df_ticker_names, df_isin_ticker, by = "isin")
      df_ticker_names <- unique(df_ticker_names[, c("ticker", "name")])

      #### Get most recent entry in each price-quantity panel

      df_all <- do.call(rbind, list_dfs)

      df_current <- stats::aggregate(date ~ ticker, data = df_all, max)

      df_current <- merge(df_all, df_current, by = c("ticker", "date"))

      df_current <- df_current[df_current$cum_quantity > 0, ]

      df_current <- unique(df_current)

      df_current <- merge(df_current, df_ticker_names, by = "ticker")
      df_current <- merge(df_current, df_isin_ticker, by = "ticker")

      df_current <- df_current[, c("name", "isin", "ticker", "adjusted",
                                   "cum_quantity", "value")]

      total_portfolio_value <- sum(df_current$value)

      df_current$weight <- df_current$value / total_portfolio_value

      data.table::fwrite(df_current, file.path(path.data, file.current))

    }

  } else {

    ## If no price quantity panels exist, delete file with current investments
    unlink(file.path(path.data, file.current))

  }

}
