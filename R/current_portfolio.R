#' Write current portfolio investments to a csv file
#'
#' @usage write_current_portfolio(path)
#' @param path A single character string. Directory of your data.
#'
#' @export
write_current_portfolio <- function(path) {

  get_names(path)

  ## load price quantity panels if exists
  if ( !rlang::is_empty(list.files(path.pricequantity.panel)) ) {

    files <- paste0(path.pricequantity.panel, list.files(path.pricequantity.panel))
    list.dfs <- lapply(files, data.table::fread)

    transaction.history.exists <- file.exists(file.path(path.transactions, file.transactions))
    isin.ticker.exists <- file.exists(file.path(path.tickers, file.tickers))

    if (transaction.history.exists && isin.ticker.exists) {

      ## get table that converts ISIN to ticker (which is needed by Yahoo Finance)
      df.isin.ticker <- data.table::fread(file.path(path.tickers, file.tickers))

      ## keep ISIN and name
      df.transaction.history <- data.table::fread(file.path(path.transactions, file.transactions))
      df.ticker.investmentnames <- unique(df.transaction.history[, c("isin", "name")])

      ## add tickers
      df.ticker.investmentnames <- merge(df.ticker.investmentnames, df.isin.ticker, by = "isin")
      df.ticker.investmentnames <- unique(df.ticker.investmentnames[, c("ticker", "name")])

      ## get most recent entry in each price-quantity panel
      df.all <- do.call(rbind, list.dfs)

      ## keep latest date for each ticker
      df.current <- stats::aggregate(date ~ ticker, data = df.all, max)

      ## add details
      df.current <- merge(df.all, df.current, by = c("ticker", "date"))

      ## keep investments with quantity greater zero
      df.current <- df.current[df.current$cum_quantity > 0, ]

      df.current <- unique(df.current)

      ## add name and ISIN
      df.current <- merge(df.current, df.ticker.investmentnames, by = "ticker")
      df.current <- merge(df.current, df.isin.ticker, by = "ticker")

      df.current <- df.current[, c("name", "isin", "ticker", "adjusted", "cum_quantity", "value")]

      ## current total portfolio value
      total.portfolio.value <- sum(df.current$value)

      ## compute weight of each investment in total portfolio value
      df.current$weight <- df.current$value / total.portfolio.value

      data.table::fwrite(df.current, paste0(path.data, file.current))

    }

  } else { message("No price quantity panels available.") }

}
