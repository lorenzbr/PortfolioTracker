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
    list.dfs <- lapply(files, data.table::fread)

    transaction.history.exists <- file.exists(file.path(path.transactions,
                                                        file.transactions))
    isin.ticker.exists <- file.exists(file.path(path.tickers, file.tickers))

    if (transaction.history.exists && isin.ticker.exists) {

      ## Get table that converts ISIN to ticker (which is needed by Yahoo Finance)
      df.isin.ticker <- data.table::fread(file.path(path.tickers, file.tickers))

      df.transaction.history <- data.table::fread(file.path(path.transactions,
                                                            file.transactions))
      df.ticker.investmentnames <- unique(df.transaction.history[, c("isin", "name",
                                                                     "transaction_date")])

      ## Use name from most recent transaction
      df.ticker.investmentnames$date <- as.Date(df.ticker.investmentnames$transaction_date,
                                                format = "%d-%m-%Y")
      df.ticker.investmentnames <- df.ticker.investmentnames %>%
        dplyr::group_by(.data$isin) %>%
        dplyr::filter(date == max(.data$date))
      df.ticker.investmentnames <- df.ticker.investmentnames %>%
        dplyr::group_by(.data$isin) %>%
        dplyr::sample_n(size = 1)

      df.ticker.investmentnames <- merge(df.ticker.investmentnames,
                                         df.isin.ticker, by = "isin")
      df.ticker.investmentnames <- unique(df.ticker.investmentnames[, c("ticker",
                                                                        "name")])

      #### Get most recent entry in each price-quantity panel

      df.all <- do.call(rbind, list.dfs)

      df.current <- stats::aggregate(date ~ ticker, data = df.all, max)

      df.current <- merge(df.all, df.current, by = c("ticker", "date"))

      df.current <- df.current[df.current$cum_quantity > 0, ]

      df.current <- unique(df.current)

      df.current <- merge(df.current, df.ticker.investmentnames, by = "ticker")
      df.current <- merge(df.current, df.isin.ticker, by = "ticker")

      df.current <- df.current[, c("name", "isin", "ticker", "adjusted",
                                   "cum_quantity", "value")]

      total.portfolio.value <- sum(df.current$value)

      df.current$weight <- df.current$value / total.portfolio.value

      data.table::fwrite(df.current, file.path(path.data, file.current))

    }

  }

}
