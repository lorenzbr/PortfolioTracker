#' Write full history of quantities for all tickers as csv
#'
#' @usage write_quantity_panels(df_transactions, path)
#' @param df_transactions A data frame containing the full history of transactions.
#' @param path A single character string. Directory of your data.
#'
#' @export
write_quantity_panels <- function(df_transactions, path) {

  get_user_names(path)

  df_transactions <- as.data.frame(df_transactions)

  df_transactions$transaction_date <- as.Date(
    df_transactions$transaction_date, format = "%d-%m-%Y")

  ticker_file <- file.path(path.tickers, file.tickers)

  if (file.exists(ticker_file)) {

    df_isin_ticker <- data.table::fread(ticker_file)
    df_isin_ticker <- df_isin_ticker[df_isin_ticker$ticker != "", ]

    df_transactions_with_tickers <- merge(df_transactions,
                                          df_isin_ticker,
                                          by = "isin")

    tickers <- unique(df_transactions_with_tickers$ticker)

  if (length(list.files(path.quantity.panel)) > 0)
    file.remove(file.path(path.quantity.panel, list.files(path.quantity.panel)))

    output <- mapply(write_quantity_panel, tickers,
                     MoreArgs = list(df_transactions_with_tickers,
                                     path.quantity.panel))

  }

}

#' Write quantity panel for ticker
#'
#' @usage write_quantity_panel(ticker, df_transactions_with_tickers,
#'                             path.quantity.panel)
#' @param ticker A single character string containing the ticker symbol.
#' @param df_transactions_with_tickers A data frame containing transactions
#' and ticker.
#' @param path.quantity.panel A single character string containing the folder
#' of quantity panels.
#'
#' @export
#' @import data.table
write_quantity_panel <- function(ticker, df_transactions_with_tickers,
                                 path.quantity.panel) {

  df_transactions <- as.data.frame(df_transactions_with_tickers)

  df_transactions <- df_transactions[df_transactions$ticker == ticker, ]

  ## Keep only sale and purchase transaction, only these types are required to create
  ## quantity panels
  df_transactions <- df_transactions[df_transactions$transaction_type == "Sale"
                                     | df_transactions$transaction_type == "Sale - Part"
                                     | df_transactions$transaction_type == "Purchase", ]

  if (nrow(df_transactions) > 0) {

    df_transactions$transaction_date <- as.Date(df_transactions$transaction_date,
                                                format = "%d-%m-%Y")

    ## If transaction type is a sale, quantity needs to be negative
    ## to be subtracted at a given date
    is_sale_transaction <- df_transactions$transaction_type == "Sale" |
      df_transactions$transaction_type == "Sale - Part"
    df_transactions$quantity[is_sale_transaction] <- -df_transactions$quantity[is_sale_transaction]

    df_transactions <- df_transactions[, c("transaction_date", "quantity")]

    ## If several transactions per day, aggregate by transaction_date
    df_transactions <- stats::aggregate(quantity ~ transaction_date,
                                        df_transactions, FUN = sum)

    ## Take cumulative sum of transactions to get quantity over time
    df_transactions_keep <- df_transactions
    df_transactions <- df_transactions[order(df_transactions$transaction_date), ]
    df_transactions$cum_quantity <- cumsum(df_transactions$quantity)
    df_transactions <- df_transactions[, c("transaction_date", "cum_quantity")]

    ## If negative cumulative quantity exists, stop function because this must
    ## be an error (short selling not included)
    if (min(df_transactions$cum_quantity) >= 0) {

      if (all(!is.na(df_transactions$transaction_date))) {

        earliest_transaction_date <- as.Date(min(df_transactions$transaction_date),
                                             format = "%d-%m-%Y")

        ## Create panel with date and quantity for ticker from first transaction
        ## date until today (remove Saturday and Sunday)
        today <- Sys.Date()
        ## Daily sequence from earliest transaction date until today
        dates <- seq(earliest_transaction_date, today, by = 1)

        ## Remove weekends (1 is Sunday, 7 is Saturday)
        ## What about holidays?
        dates <- dates[lubridate::wday(dates) != 1 & lubridate::wday(dates) != 7]

        df_panel <- data.frame(date = dates)
        data.table::setDT(df_panel)
        data.table::setDT(df_transactions)
        data.table::setkey(df_panel, "date")
        data.table::setkey(df_transactions, "transaction_date")
        dt_panel <- df_transactions[df_panel, roll = TRUE]
        df_panel <- data.table::setDF(dt_panel)


        df_panel <- merge(df_panel,
                          df_transactions_keep,
                          by = "transaction_date",
                          all = TRUE)
        df_panel$quantity[is.na(df_panel$quantity)] <- 0

        df_panel$ticker <- ticker

        names(df_panel)[names(df_panel) == "transaction_date"] <- "date"

        ## If cum_quantity of most recent date is zero, investment was sold
        ## and thus remove subsequent entries
        if (df_panel$cum_quantity[df_panel$date == max(df_panel$date)] == 0) {

          last_date_nonzero_quantity <- max(df_panel$date[df_panel$cum_quantity != 0])

          index_investment_was_sold <- which(df_panel$date == last_date_nonzero_quantity) + 1

          df_panel <- df_panel[1:index_investment_was_sold, ]

        }

        file_quantity_panel <- paste0("quantity_panel_", ticker,
                                      "_from_", min(df_panel$date),
                                      "_to_", max(df_panel$date),
                                      ".csv")

        data.table::fwrite(df_panel, file.path(path.quantity.panel,
                                               file_quantity_panel))

      }

    }

  }

}

#' Get quantity panel for ticker
#'
#' @usage get_quantity_panel(ticker, df_transactions_with_tickers, user_path)
#' @param ticker A single character string containing the ticker symbol.
#' @param df_transactions_with_tickers A data frame containing transactions
#' and ticker.
#' @param user_path A single character string containing the directory of the user.
#'
#' @return A data frame which contains a quantity panel.
#'
#' @export
#' @import data.table
get_quantity_panel <- function(ticker, df_transactions_with_tickers, user_path) {

  df_transactions <- as.data.frame(df_transactions_with_tickers)

  df_transactions <- df_transactions[df_transactions$ticker == ticker, ]

  ## Keep only sale and purchase transaction, only these types are required to create
  ## quantity panels
  df_transactions <- df_transactions[df_transactions$transaction_type == "Sale"
                                     | df_transactions$transaction_type == "Sale - Part"
                                     | df_transactions$transaction_type == "Purchase", ]

  if (nrow(df_transactions) > 0) {

    df_transactions$transaction_date <- as.Date(
      df_transactions$transaction_date, format = "%d-%m-%Y")

    ## If transaction type is a sale, quantity needs to be negative
    ## to be subtracted at a given date
    is_sale_transaction <- df_transactions$transaction_type == "Sale" |
      df_transactions$transaction_type == "Sale - Part"
    df_transactions$quantity[is_sale_transaction] <- -df_transactions$quantity[is_sale_transaction]

    df_transactions <- df_transactions[, c("transaction_date", "quantity")]

    ## If several transactions per day, aggregate by transaction_date
    df_transactions <- stats::aggregate(quantity ~ transaction_date,
                                        df_transactions, FUN = sum)

    ## Take cumulative sum of transactions to get quantity over time
    df_transactions_keep <- df_transactions
    df_transactions <- df_transactions[order(df_transactions$transaction_date), ]
    df_transactions$cum_quantity <- cumsum(df_transactions$quantity)
    df_transactions <- df_transactions[, c("transaction_date", "cum_quantity")]

    ## If negative cumulative quantity exists, stop function because this must
    ## be an error (short selling not included)
    if (min(df_transactions$cum_quantity) >= 0) {

      if (all(!is.na(df_transactions$transaction_date))) {

        earliest_transaction_date <- as.Date(
          min(df_transactions$transaction_date), format = "%d-%m-%Y")

        ## Create panel with date and quantity for ticker from first transaction
        ## date until today (remove Saturday and Sunday)
        today <- Sys.Date()
        ## Daily sequence from earliest transaction date until today
        dates <- seq(earliest_transaction_date, today, by = 1)

        ## Remove weekends (1 is Sunday, 7 is Saturday)
        ## What about holidays?
        dates <- dates[lubridate::wday(dates) != 1 &
                       lubridate::wday(dates) != 7]

        df_panel <- data.frame(date = dates)
        data.table::setDT(df_panel)
        data.table::setDT(df_transactions)
        data.table::setkey(df_panel, "date")
        data.table::setkey(df_transactions, "transaction_date")
        dt_panel <- df_transactions[df_panel, roll = TRUE]
        df_panel <- data.table::setDF(dt_panel)


        df_panel <- merge(df_panel,
                          df_transactions_keep,
                          by = "transaction_date",
                          all = TRUE)
        df_panel$quantity[is.na(df_panel$quantity)] <- 0

        df_panel$ticker <- ticker

        names(df_panel)[names(df_panel) == "transaction_date"] <- "date"

        ## If cum_quantity of most recent date is zero, investment was sold
        ## and thus remove subsequent entries
        if (df_panel$cum_quantity[df_panel$date == max(df_panel$date)] == 0) {

          last_date_nonzero_quantity <- max(
            df_panel$date[df_panel$cum_quantity != 0])

          index_investment_was_sold <- which(
            df_panel$date == last_date_nonzero_quantity) + 1

          df_panel <- df_panel[1:index_investment_was_sold, ]

        }

        df_panel <- correct_quantity_panels_for_stock_splits(
          df_panel, ticker, df_transactions_with_tickers, user_path)

        return(df_panel)

      }

    }

  }

}

#' Write price panels for all tickers to csv files
#'
#' @usage write_price_panels(df_transactions, path)
#' @param df_transactions A data frame containing transaction history.
#' @param path A single character string containing the directory of the user.
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
write_price_panels <- function(df_transactions, path) {

  get_user_names(path)

  tickers <- get_tickers_from_transactions(df_transactions, path)

  if (length(list.files(path.price.panel)) > 0)
    file.remove(file.path(path.price.panel, list.files(path.price.panel)))

  if (length(tickers) > 0) {

    for (i in 1:length(tickers)) {

      ticker <- tickers[i]

      if (length(list.files(path.prices.raw, pattern = ticker)) > 0) {

        df_prices_files <- data.frame(filenames = list.files(path.prices.raw,
                                                             pattern = ticker))
        df_prices_files$filenames <- as.character(df_prices_files$filenames)

        list_price_data <- lapply(df_prices_files$filenames,
                                  function(x) data.table::fread(file.path(path.prices.raw, x)))
        df_price_panel <- do.call(rbind, list_price_data)

        ## Make sure only one price per day is in data
        df_price_panel <- df_price_panel %>%
          dplyr::group_by(.data$date) %>%
          dplyr::sample_n(size = 1)

        from <- min(df_price_panel$date)
        to <- max(df_price_panel$date)

        filename_price_panel <- paste0("price_panel_", ticker,
                                       "_from_", from,
                                       "_to_", to,
                                       ".csv")

        data.table::fwrite(df_price_panel, file.path(path.price.panel,
                                                     filename_price_panel))

      }

    }

  }

}

#' Write panels for the product of prices and quantity for all tickers as csv
#'
#' @usage write_price_quantity_panels2(df_transactions, user_path, db_path)
#' @param df_transactions A data frame containing transaction history.
#' @param user_path A single character string containing the directory of the user.
#' @param db_path A single character string containing the directory of the database.
#'
#' @export
write_price_quantity_panels2 <- function(df_transactions, user_path, db_path) {

  get_user_names(user_path)
  get_db_names(db_path)

  list_output <- get_tickers_from_db(df_transactions, db_path)
  df_transactions_with_tickers <- list_output[[1]]
  tickers <- list_output[[2]]

  files_pricequantity_panel <- list.files(path.pricequantity.panel)

  if (length(files_pricequantity_panel) > 0)
    file.remove(file.path(path.pricequantity.panel, files_pricequantity_panel))

  ## Stock splits are appended to data. Thus, it should be deleted every time
  ## quantity or price-quantity panels are recreated#
  file_path_splits_previous <- file.path(path.data, file.stock.splits.previous)
  file_path_splits_current <- file.path(path.data, file.stock.splits.current)
  if (file.exists(file_path_splits_previous))
    file.remove(file_path_splits_previous)
  if (file.exists(file_path_splits_current))
    file.remove(file_path_splits_current)

  output <- mapply(write_price_quantity_panel2, tickers,
                   MoreArgs = list(df_transactions_with_tickers,
                                   user_path, db_path))

}

#' Write panel for the product of prices and quantity for input ticker as csv
#'
#' @usage write_price_quantity_panel2(ticker, df_transactions_with_tickers,
#'                                    user_path, db_path)
#' @param ticker A single character string containing a ticker symbol.
#' @param df_transactions_with_tickers A data frame containing transactions
#' and ticker.
#' @param user_path A single character string containing the directory of the user.
#' @param db_path A single character string containing the directory of the database.
#'
#' @export
write_price_quantity_panel2 <- function(ticker, df_transactions_with_tickers,
                                        user_path, db_path) {

  get_user_names(user_path)
  get_db_names(db_path)

  df_panel <- get_price_quantity_panel2(
    ticker, df_transactions_with_tickers, path.prices.db, user_path)

  if (!is.null(df_panel)) {

    file_pricequantity_panel <- paste0("pricequantity_panel_", ticker,
                                       "_from_", min(df_panel$date),
                                       "_to_", max(df_panel$date),
                                       ".csv")

    data.table::fwrite(df_panel, file.path(path.pricequantity.panel,
                                           file_pricequantity_panel))

  }

}

#' Get panel for the product of prices and quantity for input ticker
#'
#' @usage get_price_quantity_panel2(ticker, df_transactions_with_tickers,
#'                                  path.prices.db, user_path)
#' @param ticker A single character string containing a ticker symbol.
#' @param df_transactions_with_tickers A data frame containing transactions
#' and ticker.
#' @param path.prices.db A single character string containing the directory of
#' the price database.
#' @param user_path A single character string containing the directory of the user.
#'
#' @return A data frame with price quantity panel.
#'
#' @export
get_price_quantity_panel2 <- function(ticker, df_transactions_with_tickers,
                                      path.prices.db, user_path) {

  file_prices <- file.path(path.prices.db, paste0("prices_", ticker, ".csv"))

  if (file.exists(file_prices)) {

    df_prices <- data.table::fread(file_prices)

    df_quantity_panel <- get_quantity_panel(
      ticker, df_transactions_with_tickers, user_path)

    if (!is.null(df_quantity_panel)) {

      df_panel <- merge(df_prices, df_quantity_panel, by = "date")

      ## Is this needed?? Check this!!!
      df_panel$value <- df_panel$adjusted * df_panel$cum_quantity

    } else {

      df_panel <- NULL

    }

    return(df_panel)

  }

}

#' Write panels for the product of prices and quantity for all tickers as csv
#'
#' @usage write_price_quantity_panels(df_transactions, path)
#' @param df_transactions A data frame containing transaction history.
#' @param path A single character string containing the directory of the user.
#'
#' @export
write_price_quantity_panels <- function(df_transactions, path) {

  get_user_names(path)

  tickers <- get_tickers_from_transactions(df_transactions, path)

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

    df_price_panel <- data.table::fread(file.path(path.price.panel,
                                              list.files(path.price.panel,
                                                         pattern = ticker)))

    if (length(list.files(path.quantity.panel, pattern = ticker)) > 0) {

      df_quantity_panel <- data.table::fread(file.path(path.quantity.panel,
                                                   list.files(path.quantity.panel,
                                                              pattern = ticker)))

      df_panel <- merge(df_price_panel,
                        df_quantity_panel,
                        by = "date")

      df_panel$value <- df_panel$adjusted * df_panel$cum_quantity

      file_pricequantity_panel <- paste0("pricequantity_panel_", ticker,
                                         "_from_", min(df_panel$date),
                                         "_to_", max(df_panel$date),
                                         ".csv")

      data.table::fwrite(df_panel,
                         file.path(path.pricequantity.panel,
                                   file_pricequantity_panel))

    }

  }

}

#' Write all value panels for purchase, dividend and sales transactions for all tickers
#'
#' @usage write_all_value_panels(df_transactions, user_path, db_path)
#' @param df_transactions A data frame containing the full history
#' of transactions.
#' @param user_path A single character string containing the directory of the user.
#' @param db_path A single character string containing the directory of the database.
#'
#' @export
write_all_value_panels <- function(df_transactions, user_path, db_path) {

  get_user_names(user_path)
  get_db_names(db_path)

  df_transactions <- as.data.frame(df_transactions)

  df_transactions$transaction_date <- as.Date(
    df_transactions$transaction_date, format = "%d-%m-%Y")

  df_isin_ticker <- data.table::fread(file.path(path.database,
                                                file.tickers.db))

  if (!any(names(df_transactions) == "ticker"))
    df_transactions <- merge(df_transactions,
                             df_isin_ticker,
                             by = "isin")

  tickers <- unique(df_transactions$ticker)

  if (length(list.files(path.value.panel)) > 0)
    file.remove(file.path(path.value.panel,
                          list.files(path.value.panel)))

  output <- mapply(write_value_panel_all_types, tickers,
         MoreArgs = list(df_transactions, user_path))

}

#' Write value panels for purchase, dividend and sales transactions for given ticker
#'
#' @usage write_value_panel_all_types(ticker, df_transactions, user_path)
#' @param ticker A single character string containing the ticker symbol.
#' @param df_transactions A data frame containing the full history of transactions.
#' @param user_path A single character string containing the directory of the user.
#'
#' @export
write_value_panel_all_types <- function(ticker, df_transactions, user_path) {

  transaction_types <- c("Purchase", "Sale", "Dividend")

  mapply(write_value_panel, transaction_types,
         MoreArgs = list(ticker, df_transactions, user_path))

}

#' Write value panel for purchase, dividend and sales transactions for given ticker
#'
#' @usage write_value_panel(transaction_type, ticker,
#'                          df_transactions, user_path)
#' @param transaction_type A single character string containing the transaction
#' type (e.g., \emph{Purchase}, \emph{Sale} or \emph{Dividend})
#' @param ticker A single character string containing the ticker symbol.
#' @param df_transactions A data frame containing the full history of transactions.
#' @param user_path A single character string containing the directory of the user.
#'
#' @export
#' @import data.table
write_value_panel <- function(transaction_type, ticker,
                              df_transactions, user_path) {

  get_user_names(user_path)

  transaction_type_lc <- tolower(transaction_type)

  df_transactions <- df_transactions[df_transactions$ticker == ticker, ]

  df_transactions <- df_transactions[df_transactions$transaction_type == transaction_type, ]

  if (nrow(df_transactions) > 0) {

    df_transactions <- df_transactions[, c("transaction_date", "transaction_value")]

    ## Aggregate by date (because there may be several transactions per day!)
    if (nrow(df_transactions) > 1)
      df_transactions <- stats::aggregate(transaction_value ~ transaction_date,
                                   data = df_transactions,
                                   FUN = sum)

    df_transactions_keep <- df_transactions
    df_transactions <- df_transactions[order(df_transactions$transaction_date), ]
    df_transactions$cum_value <- cumsum(df_transactions$transaction_value)
    df_transactions <- df_transactions[, c("transaction_date", "cum_value")]
    names(df_transactions)[names(df_transactions) == "cum_value"] <- paste0(
      transaction_type_lc, "_cum_value")

    if (all(!is.na(df_transactions$transaction_date))) {

      earliest_date <- min(df_transactions$transaction_date)

      ## Create panel with date and value for ticker from transaction date
      ## until today (remove Saturday and Sunday)
      today <- Sys.Date()
      ## Daily sequence from earliest transaction date until today
      dates <- seq(earliest_date, today, by = 1)

      ## Remove weekends (1 is Sunday, 7 is Saturday)
      dates <- dates[lubridate::wday(dates) != 1 & lubridate::wday(dates) != 7]

      df_panel <- data.frame(date = dates)

      data.table::setDT(df_panel)
      data.table::setDT(df_transactions)
      data.table::setkey(df_panel, "date")
      data.table::setkey(df_transactions, "transaction_date")
      dt_panel <- df_transactions[df_panel, roll = TRUE]
      df_panel <- data.table::setDF(dt_panel)

      df_panel <- merge(df_panel,
                        df_transactions_keep,
                        by = "transaction_date",
                        all.x = TRUE)
      df_panel$transaction_value[is.na(df_panel$transaction_value)] <- 0
      names(df_panel)[names(df_panel) == "transaction_date"] <- "date"
      names(df_panel)[names(df_panel) == "transaction_value"] <- paste0(
        transaction_type_lc, "_value")

      file_value_panel <- paste0(transaction_type_lc, "value_panel_",
                                 ticker,
                                 "_from_", min(df_panel$date),
                                 "_to_", max(df_panel$date),
                                 ".csv")

      data.table::fwrite(df_panel, file.path(path.value.panel,
                                             file_value_panel))

    }

  }

}

#' Write complete panels to a csv file
#'
#' @usage write_complete_panels(user_path, db_path)
#' @param user_path A single character string containing the directory of the user.
#' @param db_path A single character string containing the directory of the database.
#'
#' @export
write_complete_panels <- function(user_path, db_path) {

  get_user_names(user_path)
  get_db_names(db_path)

  df_transactions <- data.table::fread(
    file.path(path.transactions, file.transactions))

  tickers <- get_tickers_from_db(df_transactions, db_path)[[2]]

  files_complete_panel <- list.files(path.complete.panel)

  if (length(files_complete_panel) > 0)
    file.remove(file.path(path.complete.panel, files_complete_panel))

  output <- mapply(write_complete_panel, tickers,
                   MoreArgs = list(user_path))

}

#' Write complete panel to a csv file
#'
#' @usage write_complete_panel(ticker, user_path)
#' @param ticker A single character string containing the ticker.
#' @param user_path A single character string. Path where data are stored.
#'
#' @export
write_complete_panel <- function(ticker, user_path) {

  get_user_names(user_path)

  pricequantity_panels <- list.files(path.pricequantity.panel,
                                     pattern = ticker)

  if (length(pricequantity_panels) > 0) {

    df_pricequantity_panel <- data.table::fread(
      file.path(path.pricequantity.panel, pricequantity_panels))

    df_pricequantity_panel <- df_pricequantity_panel[, c("date", "adjusted", "value",
                                                         "cum_quantity", "quantity")]

    ticker_value_panels <- list.files(path.value.panel, pattern = ticker)

    if (length(ticker_value_panels) > 0) {

      purchase_exist <- startsWith(ticker_value_panels, "purchase")
      sale_exist <- startsWith(ticker_value_panels, "sale")
      dividend_exist <- startsWith(ticker_value_panels, "dividend")

      if (any(purchase_exist))
        df_purchasevalue_panel <- data.table::fread(
          file.path(path.value.panel, ticker_value_panels[purchase_exist]))

      if (any(sale_exist))
        df_salevalue_panel <- data.table::fread(
          file.path(path.value.panel, ticker_value_panels[sale_exist]))

      if (any(dividend_exist))
        df_dividendvalue_panel <- data.table::fread(
          file.path(path.value.panel, ticker_value_panels[dividend_exist]))

      if (exists("df_purchasevalue_panel")) {

        df_panel <- merge(df_pricequantity_panel,
                          df_purchasevalue_panel,
                          by = "date",
                          all = TRUE)

        if (exists("df_salevalue_panel")) {
          df_panel <- merge(df_panel,
                            df_salevalue_panel,
                            by = "date",
                            all = TRUE)
        } else {
          df_panel$sale_cum_value <- 0
          df_panel$sale_value <- 0
        }

        if (exists("df_dividendvalue_panel")) {
          df_panel <- merge(df_panel,
                            df_dividendvalue_panel,
                            by = "date",
                            all = TRUE)
        } else {
          df_panel$dividend_cum_value <- 0
          df_panel$dividend_value <- 0
        }

      }

      df_panel[is.na(df_panel)] <- 0
      df_panel <- df_panel[df_panel$cum_quantity != 0
                           | df_panel$sale_value != 0
                           | df_panel$dividend_value != 0, ]

      ## Add ticker (makes it a bit easier when using lapply
      ## to read all panels)
      df_panel$ticker <- ticker

      file_panel <- paste0("complete_panel_", ticker,
                           "_from_", min(df_panel$date),
                           "_to_", max(df_panel$date),
                           ".csv")

      data.table::fwrite(
        df_panel, file.path(path.complete.panel, file_panel))

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

  df_transactions <- data.table::fread(file.path(path.transactions,
                                                 file.transactions))

  tickers <- get_tickers_from_transactions(df_transactions, path)

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

    df_pricequantity_panel <- data.table::fread(
      file.path(path.pricequantity.panel, list.files(path.pricequantity.panel,
                                                     pattern = ticker)))

    df_pricequantity_panel <- df_pricequantity_panel[, c("date", "adjusted",
                                                         "value", "quantity")]

    if (length(list.files(path.value.panel, pattern = ticker)) > 0) {

      ticker_value_panels <- list.files(path.value.panel, pattern = ticker)

      purchase_exist <- startsWith(ticker_value_panels, "purchase")
      sale_exist <- startsWith(ticker_value_panels, "sale")
      dividend_exist <- startsWith(ticker_value_panels, "dividend")

      if (any(purchase_exist))
        df_purchasevalue_panel <- data.table::fread(file.path(path.value.panel,
                                                              ticker_value_panels[purchase_exist]))

      if (any(sale_exist))
        df_salevalue_panel <- data.table::fread(file.path(path.value.panel,
                                                          ticker_value_panels[sale_exist]))

      if (any(dividend_exist))
        df_dividendvalue_panel <- data.table::fread(file.path(path.value.panel,
                                                              ticker_value_panels[dividend_exist]))

      if (exists("df_purchasevalue_panel")) {

        df_panel <- merge(df_pricequantity_panel, df_purchasevalue_panel,
                          by = "date", all = TRUE)

        if (exists("df_salevalue_panel")) {
          df_panel <- merge(df_panel, df_salevalue_panel,
                            by = "date", all = TRUE)
        } else {
          df_panel$sale_cum_value <- 0
          df_panel$sale_value <- 0
        }

        if (exists("df_dividendvalue_panel")) {
          df_panel <- merge(df_panel, df_dividendvalue_panel,
                            by = "date", all = TRUE)
        } else {
          df_panel$dividend_cum_value <- 0
          df_panel$dividend_value <- 0
        }

      }

      df_panel[is.na(df_panel)] <- 0

      df_panel$investment_value <- df_panel$value + df_panel$sale_value +
        df_panel$dividend_value - df_panel$purchase_value

      df_panel <- df_panel[, c("date", "investment_value")]

      file_panel <- paste0("investment_panel_", ticker,
                           "_from_", min(df_panel$date),
                           "_to_", max(df_panel$date),
                           ".csv")

      data.table::fwrite(df_panel, file.path(path.value.panel, file_panel))

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

  files_complete_panels <- list.files(path.complete.panel)

  if (length(files_complete_panels) > 0) {

    files <- file.path(path.complete.panel, files_complete_panels)
    list_dfs <- lapply(files, data.table::fread)

    ## Get full time period for each ticker; data frame in list and store back into list
    ## Why? Because I need to have the same period for all individual investments
    ## in order to compute the cash flow in each period.

    df_all <- do.call(rbind, list_dfs)

    full_time_period <- seq(min(df_all$date),
                            max(df_all$date),
                            by = "day")

    ## Remove weekends (1 is Sunday, 7 is Saturday)
    full_time_period <- full_time_period[lubridate::wday(full_time_period) != 1
                                         & lubridate::wday(full_time_period) != 7]

    df_full_time_period <- data.frame(date = full_time_period)


    ## For some dates no price information is available. Thus, I create a column indicating this
    ## I re-calculate the cumulative quantity of the investment because I take the full time period
    for (i in 1:length(list_dfs)) {

      df <- list_dfs[[i]]

      df_new <- merge(df_full_time_period, df,
                      by = "date", all.x = TRUE)
      df_new$ticker <- df$ticker[1]
      df_new[is.na(df_new)] <- 0

      ## New cumulative sum of quantity (because time period is completed,
      ## also for dates without price information)
      df_new$cum_quantity <- cumsum(df_new$quantity)
      df_new$currently_invested <- ifelse(df_new$cum_quantity > 0, 1, 0)

      df_new$value_available <- ifelse(df_new$value != 0, 1, 0)

      list_dfs[[i]] <- df_new

    }

    df_all <- do.call(rbind, list_dfs)

  } else {

    df_all <- NULL

  }

  return(df_all)

}
