#' Write full history of quantities for all tickers as csv
#'
#' @usage write_quantity_panels(df.transaction.history, path)
#' @param df.transaction.history A data.frame containing the full history of transactions.
#' @param path A single character string. Directory of your data.
#'
#' @export
write_quantity_panels <- function(df.transaction.history, path) {

  list.names <- get_names(path)
  path.quantitypanel <- list.names$path.quantitypanel
  path.tickers <- list.names$path.tickers
  file.tickers <- list.names$file.tickers

  ## convert to data frame
  df.transaction.history <- as.data.frame(df.transaction.history)

  ## convert transaction date into date type
  df.transaction.history$transaction_date <- as.Date(df.transaction.history$transaction_date, "%d-%m-%Y")

  ## get table that converts ISIN to ticker
  df.isin.ticker <- data.table::fread(paste0(path.tickers, file.tickers))

  ## add ticker to transaction data
  df.transaction.history <- merge(df.transaction.history, df.isin.ticker, by = "isin")

  ## all tickers
  tickers <- unique(df.transaction.history$ticker)

  ## delete all files in folder
  if (!rlang::is_empty(list.files(path.quantitypanel))) {
    file.remove(paste0(path.quantitypanel, list.files(path.quantitypanel)))
  }

  ## create quantity panels for all tickers
  output <- mapply(write_quantity_panel, tickers,
                   MoreArgs = list(df.transaction.history, path.quantitypanel))

}

#' Write quantity panel for ticker
#'
#' @usage write_quantity_panel(ticker, df.transaction.history, path.quantitypanel)
#' @param ticker A single character string containing the ticker symbol.
#' @param df.transaction.history A data.frame containing the full history of transactions.
#' @param path.quantitypanel A single character string containing the folder of quantity panels.
#'
#' @export
#' @import data.table
write_quantity_panel <- function(ticker, df.transaction.history, path.quantitypanel) {

  ## get transactions only for ticker
  df.transaction.history.ticker <- df.transaction.history[df.transaction.history$ticker == ticker, ]

  ## only sale and purchase transaction types are required to create quantity panels
  df.transaction.history.ticker <- df.transaction.history.ticker[grepl("^Sale$|^Purchase$", df.transaction.history.ticker$transaction_type), ]

  if (nrow(df.transaction.history.ticker) > 0) {

    ## if transaction type is a sale, quantity needs to be negative (in order to be substracted at a given point in time)
    df.transaction.history.ticker$quantity[df.transaction.history.ticker$transaction_type == "Sale"] <- (-1) * df.transaction.history.ticker$quantity[df.transaction.history.ticker$transaction_type == "Sale"]

    ## take cumulative sum of transactions to get quantity over time
    df.transaction.history.ticker <- df.transaction.history.ticker[, c("transaction_date", "quantity")]
    df.transaction.history.ticker <- df.transaction.history.ticker[order(df.transaction.history.ticker$transaction_date), ]
    df.transaction.history.ticker$cum_quantity <- cumsum(df.transaction.history.ticker$quantity)
    df.transaction.history.ticker <- df.transaction.history.ticker[, c("transaction_date", "cum_quantity")]

    ## if negative cumulative quantity exists, stop function because this must be an error (short selling not included)
    if (min(df.transaction.history.ticker$cum_quantity) >= 0) {

      ## if transaction date is unequal NA
      if ( all(!is.na(df.transaction.history.ticker$transaction_date)) ) {

        ## earliest transaction_date
        earliest.transaction.date <- min(df.transaction.history.ticker$transaction_date)

        ## first transaction for each ticker (to get panel of quantity for each ticker)
        df.transaction.history.ticker.first <- df.transaction.history.ticker[df.transaction.history.ticker$transaction_date == earliest.transaction.date, ]

        ## create panel with date and quantity for ticker from transaction date until today (remove Saturday and Sunday)
        today <- Sys.Date()
        ## daily sequence from earliest transaction date until today
        dates <- seq(earliest.transaction.date, today, by = 1)
        mysysgetlocale <- Sys.getlocale('LC_TIME')
        Sys.setlocale('LC_TIME', 'ENGLISH')
        ## remove weekends
        dates <- dates[!weekdays(dates) %in% c('Saturday', 'Sunday')]
        Sys.setlocale('LC_TIME', mysysgetlocale)
        df.quantity.panel <- data.frame(date = dates)
        df.quantity.panel$ticker <- ticker

        data.table::setDT(df.quantity.panel)
        data.table::setDT(df.transaction.history.ticker)
        data.table::setkey(df.quantity.panel, "date")
        data.table::setkey(df.transaction.history.ticker, "transaction_date")
        DT.quantity.panel <- df.transaction.history.ticker[df.quantity.panel, roll = TRUE]
        df.quantity.panel <- data.table::setDF(DT.quantity.panel)
        names(df.quantity.panel)[names(df.quantity.panel) == "transaction_date"] <- "date"

        ## if cum_quantity of most recent date is zero, investment was sold
        if (df.quantity.panel$cum_quantity[df.quantity.panel$date == max(df.quantity.panel$date)] == 0) {

          last.date.nonzero.quantity <- max(df.quantity.panel$date[df.quantity.panel$cum_quantity != 0])
          entry.investment.was.sold <- which(df.quantity.panel$date == last.date.nonzero.quantity) + 1

          df.quantity.panel <- df.quantity.panel[1:entry.investment.was.sold, ]

        }

        ## remove entries with zero cumulative quantity
        # df.quantity.panel <- df.quantity.panel[df.quantity.panel$cum_quantity != 0, ]

        from <- min(df.quantity.panel$date)
        to <- max(df.quantity.panel$date)

        filename.quantity.panel <- paste0("quantity_panel_", ticker, "_from_", from, "_to_", to, ".csv")

        data.table::fwrite(df.quantity.panel, paste0(path.quantitypanel, filename.quantity.panel))

        message("Quantity panel for ", ticker, " successfully created!")

    } else { message("Transaction dates contain NA. Please check!") }

    } else { message("Negative quantity for ", ticker, ". Creating quantity panel not possible.") } ## end of if else statement minimum quantity is positive

  } else { message("No purchases or sale transactions available.") } ## end of if else statement whether purchases or sales transactions are available

}

#' Write full history of prices for all tickers as csv
#'
#' @usage write_price_panels(df.transactions, path)
#' @param df.transactions A data.frame containing transaction history.
#' @param path A single character string containing the directory of the project.
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
write_price_panels <- function(df.transactions, path){

  list.names <- get_names(path)
  path.pricepanel <- list.names$path.pricepanel
  path.prices.raw <- list.names$path.prices.raw

  ## get tickers from history of transactions
  tickers <- get_tickers_from_transactions(df.transactions, path)

  ## delete all files in folder
  if (!rlang::is_empty(list.files(path.pricepanel))) {
    file.remove(paste0(path.pricepanel, list.files(path.pricepanel)))
  }

  ## list all files with prices
  if (!rlang::is_empty(tickers)) {

    for (i in 1:length(tickers)) {

      ticker <- tickers[i]

      if (!rlang::is_empty(list.files(paste0(path.prices.raw), pattern = ticker))) {

        df.prices.files <- data.frame(filenames = list.files(paste0(path.prices.raw), pattern = ticker))
        df.prices.files$filenames <- as.character(df.prices.files$filenames)

        ## load all files containing prices for ticker
        list.price.data <- lapply(df.prices.files$filenames,
                                               function(x) data.table::fread(paste0(path.prices.raw, x)))
        df.price.panel <- do.call(rbind, list.price.data)

        ## select randomly one row per day
        df.price.panel <- df.price.panel %>% dplyr::group_by(.data$date) %>% dplyr::sample_n(size = 1)

        from <- min(df.price.panel$date)
        to <- max(df.price.panel$date)

        filename.price.panel <- paste0("price_panel_", ticker, "_from_", from, "_to_", to, ".csv")

        data.table::fwrite(df.price.panel, paste0(path.pricepanel, filename.price.panel))

        message("Price panel for ", ticker, " successfully created.")

      } else { message("No price data available.") }

    }

  } else { message("No tickers to create price panels available.") }

}

#' Write panels for the product of prices and quantity for all tickers as csv
#'
#' @usage write_price_quantity_panels(df.transactions, path)
#' @param df.transactions A data.frame containing transaction history.
#' @param path A single character string containing the directory of the project.
#'
#' @export
write_price_quantity_panels <- function(df.transactions, path) {

  list.names <- get_names(path)
  path.pricequantitypanel <- list.names$path.pricequantitypanel
  path.tickers <- list.names$path.tickers

  ## get tickers from history of transactions
  tickers <- get_tickers_from_transactions(df.transactions, path)

  ## delete all files in folder
  if (!rlang::is_empty(list.files(path.pricequantitypanel))) {
    file.remove(paste0(path.pricequantitypanel, list.files(path.pricequantitypanel)))
  }

  ## write price-quantity panels
  output <- mapply(write_price_quantity_panel, tickers, MoreArgs = list(path))

}

#' Write panel for the product of prices and quantity for input ticker as csv
#'
#' @usage write_price_quantity_panel(ticker, path)
#' @param ticker A single character string containing a ticker symbol.
#' @param path A single character string containing the directory of the project.
#'
#' @export
write_price_quantity_panel <- function(ticker, path) {

  list.names <- get_names(path)
  path.quantitypanel <- list.names$path.quantitypanel
  path.pricepanel <- list.names$path.pricepanel
  path.pricequantitypanel <- list.names$path.pricequantitypanel
  path.tickers <- list.names$path.tickers

  ## load price panel
  if (!rlang::is_empty(list.files(paste0(path.pricepanel), pattern = ticker))) {

    df.pricepanel <- data.table::fread(paste0(path.pricepanel,
                                              list.files(paste0(path.pricepanel), pattern = ticker)))

    ## load quantity panel
    if(!rlang::is_empty(list.files(paste0(path.quantitypanel), pattern = ticker))) {

      df.quantitypanel <- data.table::fread(paste0(path.quantitypanel,
                                                   list.files(paste0(path.quantitypanel), pattern = ticker)))

      df.pricequantitypanel <- merge(df.pricepanel, df.quantitypanel, by = "date")

      ## get value of investment in each period
      df.pricequantitypanel$value <- df.pricequantitypanel$adjusted * df.pricequantitypanel$cum_quantity

      ## start and end date
      from <- min(df.pricequantitypanel$date)
      to <- max(df.pricequantitypanel$date)

      ## file name
      filename.pricequantity.panel <- paste0("pricequantity_panel_", ticker, "_from_", from, "_to_", to, ".csv")

      ## store price quantity panel as csv
      data.table::fwrite(df.pricequantitypanel, paste0(path.pricequantitypanel, filename.pricequantity.panel))

      message("Price-quantity panel for ", ticker, " successfully created!")

    } else { message("No quantity panel available.") } ## end of if else statement to check whether quantity panel is available

  } else { message("No price panel available.") }

}

#' Write value panel for purchase, dividend and sales transactions for given ticker
#'
#' @usage write_value_panel(transaction.type, path, ticker, df.transaction.history)
#' @param transaction.type A single character string containing the transaction type (e.g., \emph{Purchase}, \emph{Sale} or \emph{Dividend})
#' @param path A single character string containing the directory of the project.
#' @param ticker A single character string containing the ticker symbol.
#' @param df.transaction.history A data.frame containing the full history of transactions.
#'
#' @export
#' @import data.table
write_value_panel <- function(transaction.type, path, ticker, df.transaction.history) {

  list.names <- get_names(path)
  path.value.panel <- list.names$path.value.panel

  df.transaction.history.ticker <- df.transaction.history[df.transaction.history$ticker == ticker, ]

  df.transaction.history.ticker <- df.transaction.history.ticker[df.transaction.history.ticker$transaction_type == transaction.type, ]

  if (nrow(df.transaction.history.ticker) > 0) {

    df.transaction.history.ticker <- df.transaction.history.ticker[, c("transaction_date", "transaction_value")]
    df.transaction.history.ticker <- df.transaction.history.ticker[order(df.transaction.history.ticker$transaction_date), ]
    df.transaction.history.ticker$cum_value <- cumsum(df.transaction.history.ticker$transaction_value)
    df.transaction.history.ticker <- df.transaction.history.ticker[, c("transaction_date", "cum_value")]

    ## if transaction date is unequal NA
    if ( all(!is.na(df.transaction.history.ticker$transaction_date)) ) {

      ## earliest transaction_date
      earliest.date <- min(df.transaction.history.ticker$transaction_date)

      df.transaction.history.ticker.first <- df.transaction.history.ticker[df.transaction.history.ticker$transaction_date == earliest.date, ]

      ## create panel with date and value for ticker from transaction date until today (remove Saturday and Sunday)
      today <- Sys.Date()
      ## daily sequence from earliest transaction date until today
      dates <- seq(earliest.date, today, by = 1)
      mysysgetlocale <- Sys.getlocale('LC_TIME')
      Sys.setlocale('LC_TIME', 'ENGLISH')
      ## remove weekends
      dates <- dates[!weekdays(dates) %in% c('Saturday', 'Sunday')]
      Sys.setlocale('LC_TIME', mysysgetlocale)
      df.panel <- data.frame(date = dates)
      df.panel$ticker <- ticker

      data.table::setDT(df.panel)
      data.table::setDT(df.transaction.history.ticker)
      data.table::setkey(df.panel, "date")
      data.table::setkey(df.transaction.history.ticker, "transaction_date")
      DT.panel <- df.transaction.history.ticker[df.panel, roll = TRUE]
      df.panel <- data.table::setDF(DT.panel)
      names(df.panel)[names(df.panel) == "transaction_date"] <- "date"

      from <- min(df.panel$date)
      to <- max(df.panel$date)

      file.value.panel <- paste0(tolower(transaction.type), "value_panel_", ticker, "_from_", from, "_to_", to, ".csv")

      data.table::fwrite(df.panel, paste0(path.value.panel, file.value.panel))

      message(transaction.type, "-value panel for ", ticker, " successfully created!")

    } else { message("Transaction dates contain NA. Please check!") }

  } else { message("No purchase, dividend or sale transactions available.") }

}

#' Write value panels for purchase, dividend and sales transactions for given ticker
#'
#' @usage write_value_panel_all_types(ticker, df.transaction.history, path)
#' @param ticker A single character string containing the ticker symbol.
#' @param df.transaction.history A data.frame containing the full history of transactions.
#' @param path A single character string containing the directory of the project.
#'
#' @export
write_value_panel_all_types <- function(ticker, df.transaction.history, path) {

  transaction.types <- c("Purchase", "Sale", "Dividend")

  mapply(write_value_panel, transaction.types, MoreArgs = list(path, ticker, df.transaction.history))

}

#' Write all value panels for purchase, dividend and sales transactions for all tickers
#'
#' @usage write_all_value_panels(df.transaction.history, path)
#' @param df.transaction.history A data.frame containing the full history of transactions.
#' @param path A single character string containing the directory of the project.
#'
#' @export
write_all_value_panels <- function(df.transaction.history, path) {

  list.names <- get_names(path)
  path.value.panel <- list.names$path.value.panel
  path.tickers <- list.names$path.tickers
  file.tickers <- list.names$file.tickers

  df.transaction.history <- as.data.frame(df.transaction.history)

  ## convert transaction date into date type
  df.transaction.history$transaction_date <- as.Date(df.transaction.history$transaction_date, "%d-%m-%Y")

  ## get table that converts ISIN to ticker
  df.isin.ticker <- data.table::fread(paste0(path.tickers, file.tickers))

  ## add ticker to transaction data
  df.transaction.history <- merge(df.transaction.history, df.isin.ticker, by = "isin")

  ## all tickers
  tickers <- unique(df.transaction.history$ticker)

  ## delete all files in folder
  if (!rlang::is_empty(list.files(path.value.panel))) {
    file.remove(paste0(path.value.panel, list.files(path.value.panel)))
  }

  mapply(write_value_panel_all_types, tickers, MoreArgs = list(path, df.transaction.history))

}

# Helpers -----------------------------------------------------------------

get_tickers_from_transactions <- function(df.transaction.history, path) {

  list.names <- get_names(path)
  path.tickers <- list.names$path.tickers
  file.tickers <- list.names$file.ticker

  ## get table that converts ISIN to ticker
  df.isin.ticker <- data.table::fread(paste0(path.tickers, file.tickers))

  ## add ticker to transaction data
  df.transaction.history <- merge(df.transaction.history, df.isin.ticker, by = "isin")

  ## all tickers
  tickers <- unique(df.transaction.history$ticker)

  return(tickers)

}
