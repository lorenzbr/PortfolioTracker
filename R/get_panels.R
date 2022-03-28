#' Write full history of quantities for all tickers as csv
#'
#' @usage write_quantity_panels(df.transaction.history, path)
#' @param df.transaction.history A data frame containing the full history of transactions.
#' @param path A single character string. Directory of your data.
#'
#' @export
write_quantity_panels <- function(df.transaction.history, path) {

  get_user_names(path)

  df.transaction.history <- as.data.frame(df.transaction.history)

  df.transaction.history$transaction_date <- as.Date(df.transaction.history$transaction_date,
                                                     format = "%d-%m-%Y")

  isin.ticker.exists <- file.exists(file.path(path.tickers, file.tickers))

  if (isin.ticker.exists) {

    df.isin.ticker <- data.table::fread(file.path(path.tickers, file.tickers))
    df.isin.ticker <- df.isin.ticker[df.isin.ticker$ticker != "", ]

    df.transactions.with.tickers <- merge(df.transaction.history,
                                          df.isin.ticker,
                                          by = "isin")

    tickers <- unique(df.transactions.with.tickers$ticker)

    if (length(list.files(path.quantity.panel)) > 0)
      file.remove(file.path(path.quantity.panel, list.files(path.quantity.panel)))

    output <- mapply(write_quantity_panel, tickers,
                     MoreArgs = list(df.transactions.with.tickers, path.quantity.panel))

  }

}

#' Write quantity panel for ticker
#'
#' @usage write_quantity_panel(ticker, df.transactions.with.tickers,
#'                             path.quantity.panel)
#' @param ticker A single character string containing the ticker symbol.
#' @param df.transactions.with.tickers A data frame containing transactions
#' and ticker.
#' @param path.quantity.panel A single character string containing the folder
#' of quantity panels.
#'
#' @export
#' @import data.table
write_quantity_panel <- function(ticker, df.transactions.with.tickers,
                                 path.quantity.panel) {

  df.transactions <- as.data.frame(df.transactions.with.tickers)

  df.transactions$transaction_date <- as.Date(df.transactions$transaction_date,
                                                           format = "%d-%m-%Y")

  df.transactions <- df.transactions[df.transactions$ticker == ticker, ]

  ## Keep only sale and purchase transaction, only these types are required to create
  ## quantity panels
  df.transactions <- df.transactions[df.transactions$transaction_type == "Sale"
                                     | df.transactions$transaction_type == "Sale - Part"
                                     | df.transactions$transaction_type == "Purchase", ]

  if (nrow(df.transactions) > 0) {

    ## If transaction type is a sale, quantity needs to be negative
    ## to be subtracted at a given date
    is.sale.transaction <- df.transactions$transaction_type == "Sale" |
      df.transactions$transaction_type == "Sale - Part"
    df.transactions$quantity[is.sale.transaction] <- -df.transactions$quantity[is.sale.transaction]

    df.transactions <- df.transactions[, c("transaction_date", "quantity")]

    ## If several transactions per day, aggregate by transaction_date
    df.transactions <- stats::aggregate(quantity ~ transaction_date,
                                                      df.transactions,
                                                      FUN = sum)

    ## Take cumulative sum of transactions to get quantity over time
    df.transactions.keep <- df.transactions
    df.transactions <- df.transactions[order(df.transactions$transaction_date), ]
    df.transactions$cum_quantity <- cumsum(df.transactions$quantity)
    df.transactions <- df.transactions[, c("transaction_date", "cum_quantity")]

    ## If negative cumulative quantity exists, stop function because this must
    ## be an error (short selling not included)
    if (min(df.transactions$cum_quantity) >= 0) {

      if (all(!is.na(df.transactions$transaction_date))) {

        earliest.transaction.date <- min(df.transactions$transaction_date)
        earliest.transaction.date <- as.Date(earliest.transaction.date,
                                             format = "%d-%m-%Y")

        ## Create panel with date and quantity for ticker from first transaction
        ## date until today (remove Saturday and Sunday)
        today <- Sys.Date()
        ## Daily sequence from earliest transaction date until today
        dates <- seq(earliest.transaction.date, today, by = 1)

        ## Remove weekends (1 is Sunday, 7 is Saturday)
        ## What about holidays?
        dates <- dates[lubridate::wday(dates) != 1 & lubridate::wday(dates) != 7]

        df.panel <- data.frame(date = dates)
        data.table::setDT(df.panel)
        data.table::setDT(df.transactions)
        data.table::setkey(df.panel, "date")
        data.table::setkey(df.transactions, "transaction_date")
        dt.panel <- df.transactions[df.panel, roll = TRUE]
        df.panel <- data.table::setDF(dt.panel)


        df.panel <- merge(df.panel,
                          df.transactions.keep,
                          by = "transaction_date",
                          all = TRUE)
        df.panel$quantity[is.na(df.panel$quantity)] <- 0

        df.panel$ticker <- ticker

        names(df.panel)[names(df.panel) == "transaction_date"] <- "date"

        ## If cum_quantity of most recent date is zero, investment was sold
        ## and thus remove subsequent entries
        if (df.panel$cum_quantity[df.panel$date == max(df.panel$date)] == 0) {

          last.date.nonzero.quantity <- max(df.panel$date[df.panel$cum_quantity != 0])

          index.investment.was.sold <- which(df.panel$date == last.date.nonzero.quantity) + 1

          df.panel <- df.panel[1:index.investment.was.sold, ]

        }

        file.quantity.panel <- paste0("quantity_panel_", ticker,
                                      "_from_", min(df.panel$date),
                                      "_to_", max(df.panel$date),
                                      ".csv")

        data.table::fwrite(df.panel, file.path(path.quantity.panel,
                                               file.quantity.panel))

      }

    }

  }

}

#' Write price panels for all tickers to csv files
#'
#' @usage write_price_panels(df.transactions, path)
#' @param df.transactions A data frame containing transaction history.
#' @param path A single character string containing the directory of the user.
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
write_price_panels <- function(df.transactions, path) {

  get_user_names(path)

  tickers <- get_tickers_from_transactions(df.transactions, path)

  if (length(list.files(path.price.panel)) > 0)
    file.remove(file.path(path.price.panel, list.files(path.price.panel)))

  if (length(tickers) > 0) {

    for (i in 1:length(tickers)) {

      ticker <- tickers[i]

      if (length(list.files(path.prices.raw, pattern = ticker)) > 0) {

        df.prices.files <- data.frame(filenames = list.files(path.prices.raw,
                                                             pattern = ticker))
        df.prices.files$filenames <- as.character(df.prices.files$filenames)

        list.price.data <- lapply(df.prices.files$filenames,
                                  function(x) data.table::fread(file.path(path.prices.raw, x)))
        df.price.panel <- do.call(rbind, list.price.data)

        ## Make sure only one price per day is in data
        df.price.panel <- df.price.panel %>%
          dplyr::group_by(.data$date) %>%
          dplyr::sample_n(size = 1)

        from <- min(df.price.panel$date)
        to <- max(df.price.panel$date)

        filename.price.panel <- paste0("price_panel_", ticker,
                                       "_from_", from,
                                       "_to_", to,
                                       ".csv")

        data.table::fwrite(df.price.panel, file.path(path.price.panel,
                                                     filename.price.panel))

      }

    }

  }

}

#' Write panels for the product of prices and quantity for all tickers as csv
#'
#' @usage write_price_quantity_panels(df.transactions, path)
#' @param df.transactions A data frame containing transaction history.
#' @param path A single character string containing the directory of the user.
#'
#' @export
write_price_quantity_panels <- function(df.transactions, path) {

  get_user_names(path)

  tickers <- get_tickers_from_transactions(df.transactions, path)

  if (length(list.files(path.pricequantity.panel)) > 0)
    file.remove(file.path(path.pricequantity.panel,
                          list.files(path.pricequantity.panel)))

  output <- mapply(write_price_quantity_panel, tickers, MoreArgs = list(path))

}

#' Write panel for the product of prices and quantity for input ticker as csv
#'
#' @usage write_price_quantity_panel(ticker, path)
#' @param ticker A single character string containing a ticker symbol.
#' @param path A single character string containing the directory of the user.
#'
#' @export
write_price_quantity_panel <- function(ticker, path) {

  get_user_names(path)

  if (length(list.files(path.price.panel, pattern = ticker)) > 0) {

    df.price.panel <- data.table::fread(file.path(path.price.panel,
                                              list.files(path.price.panel,
                                                         pattern = ticker)))

    if (length(list.files(path.quantity.panel, pattern = ticker)) > 0) {

      df.quantity.panel <- data.table::fread(file.path(path.quantity.panel,
                                                   list.files(path.quantity.panel,
                                                              pattern = ticker)))

      df.pricequantity.panel <- merge(df.price.panel,
                                      df.quantity.panel,
                                      by = "date")

      df.pricequantity.panel$value <- df.pricequantity.panel$adjusted * df.pricequantity.panel$cum_quantity

      from <- min(df.pricequantity.panel$date)
      to <- max(df.pricequantity.panel$date)

      file.pricequantity.panel <- paste0("pricequantity_panel_", ticker,
                                         "_from_", from,
                                         "_to_", to,
                                         ".csv")

      data.table::fwrite(df.pricequantity.panel,
                         file.path(path.pricequantity.panel,
                                   file.pricequantity.panel))

    }

  }

}

#' Write value panel for purchase, dividend and sales transactions for given ticker
#'
#' @usage write_value_panel(transaction.type, ticker,
#'                          df.transaction.history, path)
#' @param transaction.type A single character string containing the transaction
#' type (e.g., \emph{Purchase}, \emph{Sale} or \emph{Dividend})
#' @param ticker A single character string containing the ticker symbol.
#' @param df.transaction.history A data frame containing the full history of transactions.
#' @param path A single character string containing the directory of the user.
#'
#' @export
#' @import data.table
write_value_panel <- function(transaction.type, ticker,
                              df.transaction.history, path) {

  get_user_names(path)

  transaction_type_lc <- tolower(transaction.type)

  df.transaction.history.ticker <- df.transaction.history[df.transaction.history$ticker == ticker, ]

  df.transaction.history.ticker <- df.transaction.history.ticker[df.transaction.history.ticker$transaction_type == transaction.type, ]

  if (nrow(df.transaction.history.ticker) > 0) {

    df.transaction.history.ticker <- df.transaction.history.ticker[, c("transaction_date", "transaction_value")]
    df.transaction.history.ticker.keep <- df.transaction.history.ticker
    df.transaction.history.ticker <- df.transaction.history.ticker[order(df.transaction.history.ticker$transaction_date), ]
    df.transaction.history.ticker$cum_value <- cumsum(df.transaction.history.ticker$transaction_value)
    df.transaction.history.ticker <- df.transaction.history.ticker[, c("transaction_date", "cum_value")]
    names(df.transaction.history.ticker)[names(df.transaction.history.ticker) == "cum_value"] <- paste0(transaction_type_lc,
                                                                                                        "_cum_value")

    if (all(!is.na(df.transaction.history.ticker$transaction_date))) {

      earliest.date <- min(df.transaction.history.ticker$transaction_date)

      df.transaction.history.ticker.first <- df.transaction.history.ticker[df.transaction.history.ticker$transaction_date == earliest.date, ]

      ## Create panel with date and value for ticker from transaction date
      ## until today (remove Saturday and Sunday)
      today <- Sys.Date()
      ## Daily sequence from earliest transaction date until today
      dates <- seq(earliest.date, today, by = 1)

      ## Remove weekends (1 is Sunday, 7 is Saturday)
      dates <- dates[lubridate::wday(dates) != 1 & lubridate::wday(dates) != 7]

      df.panel <- data.frame(date = dates)

      data.table::setDT(df.panel)
      data.table::setDT(df.transaction.history.ticker)
      data.table::setkey(df.panel, "date")
      data.table::setkey(df.transaction.history.ticker, "transaction_date")
      DT.panel <- df.transaction.history.ticker[df.panel, roll = TRUE]
      df.panel <- data.table::setDF(DT.panel)

      df.panel <- merge(df.panel,
                        df.transaction.history.ticker.keep,
                        by = "transaction_date",
                        all.x = TRUE)
      df.panel$transaction_value[is.na(df.panel$transaction_value)] <- 0
      names(df.panel)[names(df.panel) == "transaction_date"] <- "date"
      names(df.panel)[names(df.panel) == "transaction_value"] <- paste0(transaction_type_lc,
                                                                        "_value")

      file.value.panel <- paste0(transaction_type_lc, "value_panel_",
                                 ticker,
                                 "_from_", min(df.panel$date),
                                 "_to_", max(df.panel$date),
                                 ".csv")

      data.table::fwrite(df.panel, file.path(path.value.panel,
                                             file.value.panel))

    }

  }

}

#' Write value panels for purchase, dividend and sales transactions for given ticker
#'
#' @usage write_value_panel_all_types(ticker, df.transaction.history, path)
#' @param ticker A single character string containing the ticker symbol.
#' @param df.transaction.history A data frame containing the full history of transactions.
#' @param path A single character string containing the directory of the user.
#'
#' @export
write_value_panel_all_types <- function(ticker, df.transaction.history, path) {

  transaction.types <- c("Purchase", "Sale", "Dividend")

  # for (transaction.type in transaction.types) {
  #   write_value_panel(transaction.type, ticker, df.transaction.history, path)
  # }
  mapply(write_value_panel, transaction.types,
         MoreArgs = list(ticker, df.transaction.history, path))

}

#' Write all value panels for purchase, dividend and sales transactions for all tickers
#'
#' @usage write_all_value_panels(df.transaction.history, path)
#' @param df.transaction.history A data frame containing the full history of transactions.
#' @param path A single character string containing the directory of the user.
#'
#' @export
write_all_value_panels <- function(df.transaction.history, path) {

  get_user_names(path)

  df.transaction.history <- as.data.frame(df.transaction.history)

  df.transaction.history$transaction_date <- as.Date(df.transaction.history$transaction_date,
                                                     format = "%d-%m-%Y")

  isin.ticker.exists <- file.exists(file.path(path.tickers, file.tickers))

  if (isin.ticker.exists) {

    df.isin.ticker <- data.table::fread(file.path(path.tickers, file.tickers))

    if (!any(names(df.transaction.history) == "ticker"))
      df.transaction.history <- merge(df.transaction.history,
                                      df.isin.ticker,
                                      by = "isin")

    tickers <- unique(df.transaction.history$ticker)

    if (length(list.files(path.value.panel)) > 0) {
      file.remove(file.path(path.value.panel, list.files(path.value.panel)))
    }

    mapply(write_value_panel_all_types, tickers,
           MoreArgs = list(df.transaction.history, path))

  }

}

#' Write complete panels to a csv file
#'
#' @usage write_complete_panels(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
write_complete_panels <- function(path) {

  get_user_names(path)

  df.transaction.history <- data.table::fread(file.path(path.transactions,
                                                        file.transactions))

  tickers <- get_tickers_from_transactions(df.transaction.history, path)

  if (length(list.files(path.complete.panel)) > 0)
    file.remove(file.path(path.complete.panel, list.files(path.complete.panel)))

  output <- mapply(write_complete_panel, tickers, MoreArgs = list(path))

}

#' Write complete panel to a csv file
#'
#' @usage write_complete_panel(ticker, path)
#' @param ticker A single character string containing the ticker.
#' @param path A single character string. Path where data are stored.
#'
#' @export
write_complete_panel <- function(ticker, path) {

  get_user_names(path)

  if (length(list.files(path.pricequantity.panel, pattern = ticker)) > 0) {

    df.pricequantity.panel <- data.table::fread(file.path(path.pricequantity.panel,
                                                       list.files(path.pricequantity.panel,
                                                                  pattern = ticker)))


    df.pricequantity.panel <- df.pricequantity.panel[, c("date", "adjusted", "value",
                                                         "cum_quantity", "quantity")]

    if (length(list.files(path.value.panel, pattern = ticker)) > 0) {

      ticker.value.panels <- list.files(path.value.panel, pattern = ticker)

      purchase.exist <- startsWith(ticker.value.panels, "purchase")
      sale.exist <- startsWith(ticker.value.panels, "sale")
      dividend.exist <- startsWith(ticker.value.panels, "dividend")

      if (any(purchase.exist))
        df.purchasevalue.panel <- data.table::fread(file.path(path.value.panel,
                                                              ticker.value.panels[purchase.exist]))

      if (any(sale.exist))
        df.salevalue.panel <- data.table::fread(file.path(path.value.panel,
                                                          ticker.value.panels[sale.exist]))

      if (any(dividend.exist))
        df.dividendvalue.panel <- data.table::fread(file.path(path.value.panel,
                                                              ticker.value.panels[dividend.exist]))

      if (exists("df.purchasevalue.panel")) {

        df.panel <- merge(df.pricequantity.panel,
                          df.purchasevalue.panel,
                          by = "date",
                          all = TRUE)

        if (exists("df.salevalue.panel")) {
          df.panel <- merge(df.panel,
                            df.salevalue.panel,
                            by = "date",
                            all = TRUE)
        } else {
          df.panel$sale_cum_value <- 0
          df.panel$sale_value <- 0
        }

        if (exists("df.dividendvalue.panel")) {
          df.panel <- merge(df.panel,
                            df.dividendvalue.panel,
                            by = "date",
                            all = TRUE)
        } else {
          df.panel$dividend_cum_value <- 0
          df.panel$dividend_value <- 0
        }

      }

      df.panel[is.na(df.panel)] <- 0
      df.panel <- df.panel[df.panel$cum_quantity != 0
                           | df.panel$sale_value != 0
                           | df.panel$dividend_value != 0, ]

      ## Add ticker (makes it a bit easier when using lapply to read all panels)
      df.panel$ticker <- ticker

      from <- min(df.panel$date)
      to <- max(df.panel$date)

      file.panel <- paste0("complete_panel_", ticker,
                           "_from_", from,
                           "_to_", to,
                           ".csv")

      data.table::fwrite(df.panel, file.path(path.complete.panel, file.panel))

    }

  }

}

#' Write investment value panels for all tickers to csv files
#'
#' @description This functions writes investment value panels for all tickers
#' to separate csv files. See \code{\link{write_investment_value_panel}} for
#' further information.
#'
#' @usage write_investment_value_panels(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
write_investment_value_panels <- function(path) {

  get_user_names(path)

  df.transaction.history <- data.table::fread(file.path(path.transactions,
                                                        file.transactions))

  tickers <- get_tickers_from_transactions(df.transaction.history, path)

  mapply(write_investment_value_panel, tickers, MoreArgs = list(path))

}

#' Write investment value panel for given ticker to csv file
#'
#' @description The function writes the investment value for a given \emph{ticker}
#' and day as a csv file to \emph{path}. The investment value is defined as the sum
#' of the current value of the investment including current value of a sale and
#' dividends excluding current costs from purchasing the investment.
#' See \code{\link{write_investment_value_panels}} if you want to apply this function to all
#' tickers in your table of transactions.
#'
#' @usage write_investment_value_panel(ticker, path)
#' @param ticker A single character string containing the ticker symbol.
#' @param path A single character string. Path where data are stored.
#'
#' @export
#'
#' @import data.table
write_investment_value_panel <- function(ticker, path) {

  get_user_names(path)

  if (length(list.files(path.pricequantity.panel, pattern = ticker)) > 0) {

    df.pricequantity.panel <- data.table::fread(file.path(path.pricequantity.panel,
                                                      list.files(path.pricequantity.panel,
                                                                 pattern = ticker)))

    df.pricequantity.panel <- df.pricequantity.panel[, c("date", "adjusted",
                                                         "value", "quantity")]

    if (length(list.files(path.value.panel, pattern = ticker)) > 0) {

      ticker.value.panels <- list.files(path.value.panel, pattern = ticker)

      purchase.exist <- startsWith(ticker.value.panels, "purchase")
      sale.exist <- startsWith(ticker.value.panels, "sale")
      dividend.exist <- startsWith(ticker.value.panels, "dividend")

      if (any(purchase.exist))
        df.purchasevalue.panel <- data.table::fread(file.path(path.value.panel,
                                                              ticker.value.panels[purchase.exist]))

      if (any(sale.exist))
        df.salevalue.panel <- data.table::fread(file.path(path.value.panel,
                                                          ticker.value.panels[sale.exist]))

      if (any(dividend.exist))
        df.dividendvalue.panel <- data.table::fread(file.path(path.value.panel,
                                                              ticker.value.panels[dividend.exist]))

      if (exists("df.purchasevalue.panel")) {

        df.panel <- merge(df.pricequantity.panel, df.purchasevalue.panel,
                          by = "date", all = TRUE)

        if (exists("df.salevalue.panel")) {
          df.panel <- merge(df.panel, df.salevalue.panel,
                            by = "date", all = TRUE)
        } else {
          df.panel$sale_cum_value <- 0
          df.panel$sale_value <- 0
        }

        if (exists("df.dividendvalue.panel")) {
          df.panel <- merge(df.panel, df.dividendvalue.panel,
                            by = "date", all = TRUE)
        } else {
          df.panel$dividend_cum_value <- 0
          df.panel$dividend_value <- 0
        }

      }

      df.panel[is.na(df.panel)] <- 0

      df.panel$investment_value <- df.panel$value + df.panel$sale_value +
        df.panel$dividend_value - df.panel$purchase_value

      df.panel <- df.panel[, c("date", "investment_value")]

      from <- min(df.panel$date)
      to <- max(df.panel$date)

      file.panel <- paste0("investment_panel_", ticker,
                           "_from_", from,
                           "_to_", to,
                           ".csv")

      data.table::fwrite(df.panel, file.path(path.value.panel, file.panel))

    }

  }

}

#' Get complete portfolio panel
#'
#' @usage get_complete_portfolio_panel(path)
#' @param path A single character string. Path where data are stored.
#'
#' @return A data frame containing the complete portfolio panel
#'
#' @export
get_complete_portfolio_panel <- function(path) {

  get_user_names(path)

  files.complete.panels <- list.files(path.complete.panel)

  if (length(files.complete.panels) > 0) {

    files <- file.path(path.complete.panel, files.complete.panels)
    list.dfs <- lapply(files, data.table::fread)

    ## Get full time period for each ticker; data frame in list and store back into list
    ## Why? Because I need to have the same period for all individual investments
    ## in order to compute the cash flow in each period.

    df.all <- do.call(rbind, list.dfs)

    first.day <- min(df.all$date)
    last.day <- max(df.all$date)

    full.time.period <- seq(first.day, last.day, by = "day")

    ## Remove weekends (1 is Sunday, 7 is Saturday)
    full.time.period <- full.time.period[lubridate::wday(full.time.period) != 1
                                         & lubridate::wday(full.time.period) != 7]

    df.full.time.period <- data.frame(date = full.time.period)


    ## For some dates no price information is available. Thus, I create a column indicating this
    ## I re-calculate the cumulative quantity of the investment because I take the full time period
    for (i in 1:length(list.dfs)) {

      df <- list.dfs[[i]]

      df.new <- merge(df.full.time.period, df,
                      by = "date", all.x = TRUE)
      df.new$ticker <- df$ticker[1]
      df.new[is.na(df.new)] <- 0

      ## New cumulative sum of quantity (because time period is completed,
      ## also for dates without price information)
      df.new$cum_quantity <- cumsum(df.new$quantity)
      df.new$currently_invested <- ifelse(df.new$cum_quantity > 0, 1, 0)

      df.new$value_available <- ifelse(df.new$value != 0, 1, 0)

      list.dfs[[i]] <- df.new

    }

    df.all <- do.call(rbind, list.dfs)

  } else {

    df.all <- NULL

  }

  return(df.all)

}
