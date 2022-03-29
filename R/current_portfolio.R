#' Write current portfolio investments to a csv file
#'
#' @usage write_current_portfolio(path)
#' @param path A single character string. Directory of your data.
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
write_current_portfolio <- function(path) {

  get_user_names(path)

  if (length(list.files(path.pricequantity.panel)) > 0) {

    files <- file.path(path.pricequantity.panel, list.files(path.pricequantity.panel))
    list_dfs <- lapply(files, data.table::fread)

    transaction_history_exists <- file.exists(file.path(path.transactions,
                                                        file.transactions))
    isin_ticker_exists <- file.exists(file.path(path.tickers, file.tickers))

    if (transaction_history_exists && isin_ticker_exists) {

      ## Get table that converts ISIN to ticker (which is needed by Yahoo Finance)
      df_isin_ticker <- data.table::fread(file.path(path.tickers, file.tickers))

      df_transaction_history <- data.table::fread(file.path(path.transactions,
                                                            file.transactions))
      df_ticker_investmentnames <- unique(df_transaction_history[, c("isin", "name",
                                                                     "transaction_date")])

      ## Use name from most recent transaction
      df_ticker_investmentnames$date <- as.Date(df_ticker_investmentnames$transaction_date,
                                                format = "%d-%m-%Y")
      df_ticker_investmentnames <- df_ticker_investmentnames %>%
        dplyr::group_by(.data$isin) %>%
        dplyr::filter(date == max(.data$date))
      df_ticker_investmentnames <- df_ticker_investmentnames %>%
        dplyr::group_by(.data$isin) %>%
        dplyr::sample_n(size = 1)

      df_ticker_investmentnames <- merge(df_ticker_investmentnames,
                                         df_isin_ticker,
                                         by = "isin")
      df_ticker_investmentnames <- unique(df_ticker_investmentnames[, c("ticker",
                                                                        "name")])

      #### Get most recent entry in each price-quantity panel

      df_all <- do.call(rbind, list_dfs)

      df_current <- stats::aggregate(date ~ ticker, data = df_all, max)

      df_current <- merge(df_all, df_current, by = c("ticker", "date"))

      df_current <- df_current[df_current$cum_quantity > 0, ]

      df_current <- unique(df_current)

      df_current <- merge(df_current, df_ticker_investmentnames, by = "ticker")
      df_current <- merge(df_current, df_isin_ticker, by = "ticker")

      df_current <- df_current[, c("name", "isin", "ticker", "adjusted",
                                   "cum_quantity", "value")]

      total_portfolio_value <- sum(df_current$value)

      df_current$weight <- df_current$value / total_portfolio_value

      data.table::fwrite(df_current, file.path(path.data, file.current))

    }

  }

}
