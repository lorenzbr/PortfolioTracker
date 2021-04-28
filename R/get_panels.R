#' Write full history of quantities for all tickers as csv
#'
#' @usage write_quantity_panels(df.transaction.history, path, file.ticker = "isin_ticker.csv")
#' @param df.transaction.history A data.frame containing the full history of transactions.
#' @param path A single character string. Directory of your data.
#' @param file.ticker A single character string. Name of csv containing ISIN-ticker pairs.
#'
#' @export
write_quantity_panels <- function(df.transaction.history, path, file.ticker = "isin_ticker.csv"){

  #### get full set of all quantity panels

  ## create folder if not exists and get folder name for quantity panel and tickers
  list.paths <- PortfolioTracker::create_portfoliotracker_dir(path)
  path.quantitypanel <- list.paths$path.quantitypanel
  path.tickers <- list.paths$path.tickers

  ## convert to data frame
  df.transaction.history <- as.data.frame(df.transaction.history)

  ## convert transaction date into date type
  df.transaction.history$transaction_date <- as.Date(df.transaction.history$transaction_date, "%d-%m-%Y")

  ## get table that converts ISIN to ticker
  df.isin.ticker <- data.table::fread(paste0(path.tickers, file.ticker))

  ## add ticker to transaction data
  df.transaction.history <- merge(df.transaction.history, df.isin.ticker, by = "isin")

  ## all tickers
  tickers <- unique(df.transaction.history$ticker)

  ## delete all files in folder
  if (!rlang::is_empty(list.files(path.quantitypanel))) {
    file.remove(paste0(path.quantitypanel, list.files(path.quantitypanel)))
  }

  ## create quantity panels for all tickers
  output <- mapply(PortfolioTracker::write_quantity_panel, tickers,
                   MoreArgs = list(df.transaction.history, path.quantitypanel))

  return(output)

} ## end of function write_quantity_panels

#' Write quantity panel for ticker
#'
#' @usage write_quantity_panel(ticker, df.transaction.history, path.quantitypanel)
#' @param ticker A single character string containing the ticker symbol.
#' @param df.transaction.history A data.frame containing the full history of transactions.
#' @param path.quantitypanel A single character string containing the folder of quantity panels.
#'
#' @export
#' @import data.table
write_quantity_panel <- function(ticker, df.transaction.history, path.quantitypanel){

  #### create quantity panel based on ticker and transaction history

  ## get transactions only for ticker
  df.transaction.history.ticker <- df.transaction.history[df.transaction.history$ticker == ticker, ]

  ## only sale and purchase transaction types are required to create quantity panels
  df.transaction.history.ticker <- df.transaction.history.ticker[grepl("^Sale$|^Purchase$", df.transaction.history.ticker$transaction_type), ]

  if (nrow(df.transaction.history.ticker) > 0) {

    ## if transaction type is a sale, quantity needs to be negative (in order to be substracted at a given point in time)
    df.transaction.history.ticker$quantity[df.transaction.history.ticker$transaction_type == "Sale"] <- (-1) * df.transaction.history.ticker$quantity[df.transaction.history.ticker$transaction_type == "Sale"]

    ## take cumulative sum of transactions to get quantity over time
    df.transaction.history.ticker <- df.transaction.history.ticker[, c("transaction_date", "quantity")]
    df.transaction.history.ticker <- df.transaction.history.ticker[order(df.transaction.history.ticker$transaction_date),]
    df.transaction.history.ticker$cum_quantity <- cumsum(df.transaction.history.ticker$quantity)
    df.transaction.history.ticker <- df.transaction.history.ticker[, c("transaction_date", "cum_quantity")]

    ## if negative cumulative quantity exists, stop function because this must be an error (short selling not included)
    if (min(df.transaction.history.ticker$cum_quantity) >= 0) {

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

      ## start and end date
      from <- min(df.quantity.panel$date)
      to <- max(df.quantity.panel$date)

      ## file name
      filename.quantity.panel <- paste0("quantity_panel_", ticker, "_from_", from, "_to_", to, ".csv")

      ## store quantity panel as csv
      data.table::fwrite(df.quantity.panel, paste0(path.quantitypanel, filename.quantity.panel))

      message("Quantity panel for ", ticker, " successfully created!")

    } else {message("Negative quantity for ", ticker, ". Creating quantity panel not possible.")} ## end of if else statement minimum quantity is positive

  } else {message("No purchases or sale transactions available.")} ## end of if else statement whether purchases or sales transactions are available

} ## end of function write_quantity_panel

#' Write full history of prices for all tickers as csv
#'
#' @usage write_price_panels(df.transactions, path)
#' @param df.transactions A data.frame containing transaction history.
#' @param path A single character string.
#'
#' @export
write_price_panels <- function(df.transactions, path){

  #### get panel of prices for each ticker

  ## create folder if not exists and get folder name for price panel and tickers
  list.paths <- PortfolioTracker::create_portfoliotracker_dir(path)
  path.pricepanel <- list.paths$path.pricepanel
  path.prices.raw <- list.paths$path.prices.raw

  ## get tickers from history of transactions
  tickers <- get_tickers_from_transactions(df.transactions, path)

  ## delete all files in folder
  if (!rlang::is_empty(list.files(path.pricepanel))) {
    file.remove(paste0(path.pricepanel, list.files(path.pricepanel)))
  }

  ## list all files with prices
  if (!rlang::is_empty(tickers)) {

    for(i in 1:length(tickers)){

      ticker <- tickers[i]

      if (!rlang::is_empty(list.files(paste0(path.prices.raw), pattern = ticker))) {

        df.prices.files <- data.frame(filenames = list.files(paste0(path.prices.raw), pattern = ticker))
        df.prices.files$filenames <- as.character(df.prices.files$filenames)

        ## load all prices (full time series for each ticker)
        df.all.prices.for.ticker <- lapply(df.prices.files$filenames,
                                               function(x) data.table::fread(paste0(path.prices.raw, x)))
        df.all.prices.for.ticker <- do.call(rbind, df.all.prices.for.ticker)

        ## start and end date
        from <- min(df.all.prices.for.ticker$date)
        to <- max(df.all.prices.for.ticker$date)

        ## file name
        filename.price.panel <- paste0("price_panel_", ticker, "_from_", from, "_to_", to, ".csv")

        ## store price panel as csv
        data.table::fwrite(df.all.prices.for.ticker, paste0(path.pricepanel, filename.price.panel))

        message("Price panel successfully created.")

      } else {message("No financials available.")}

    } ## end of for loop

  } else (message("No tickers for price panel available.")) ## end of if else statement

} ## end of function write_price_panels

#' Write panels for the product of prices and quantity for all tickers as csv
#'
#' @usage write_price_quantity_panels(df.transactions, path)
#' @param df.transactions A data.frame containing transaction history.
#' @param path A single character string.
#'
#' @export
write_price_quantity_panels <- function(df.transactions, path){

  #### get panels for prices times quantity for all tickers

  ## create folder if not exists and get folder name for quantity panel and tickers
  list.paths <- PortfolioTracker::create_portfoliotracker_dir(path)
  path.pricequantitypanel <- list.paths$path.pricequantitypanel
  path.tickers <- list.paths$path.tickers

  ## get tickers from history of transactions
  tickers <- get_tickers_from_transactions(df.transactions, path)

  ## delete all files in folder
  if (!rlang::is_empty(list.files(path.pricequantitypanel))) {
    file.remove(paste0(path.pricequantitypanel, list.files(path.pricequantitypanel)))
  }

  ## write price quantity panels
  output <- mapply(PortfolioTracker::write_price_quantity_panel, tickers, MoreArgs = list(path))

} ## end of function write_price_quantity_panels

#' Write panel for the product of prices and quantity for input ticker as csv
#'
#' @usage write_price_quantity_panel(ticker, path)
#' @param ticker A single character string containing a ticker symbol.
#' @param path A single character string.
#'
#' @export
write_price_quantity_panel <- function(ticker, path){

  #### get price quantity panel for a ticker

  ## create folder if not exists and get folder name for price panel and tickers
  list.paths <- PortfolioTracker::create_portfoliotracker_dir(path)
  path.quantitypanel <- list.paths$path.quantitypanel
  path.pricepanel <- list.paths$path.pricepanel
  path.pricequantitypanel <- list.paths$path.pricequantitypanel
  path.tickers <- list.paths$path.tickers

  ## load price panel
  if (!rlang::is_empty(list.files(paste0(path.pricepanel), pattern = ticker))) {
    df.pricepanel <- data.table::fread(paste0(path.pricepanel,
                                              list.files(paste0(path.pricepanel), pattern = ticker)))

    ## load quantity panel
    if(!rlang::is_empty(list.files(paste0(path.quantitypanel), pattern = ticker))) {
      df.quantitypanel <- data.table::fread(paste0(path.quantitypanel,
                                                   list.files(paste0(path.quantitypanel), pattern = ticker)))

      ## merge tables
      df.pricequantitypanel <- merge(df.pricepanel, df.quantitypanel, by = "date")

      ## get value of investment at time t
      df.pricequantitypanel$value <- df.pricequantitypanel$adjusted * df.pricequantitypanel$cum_quantity

      ## start and end date
      from <- min(df.pricequantitypanel$date)
      to <- max(df.pricequantitypanel$date)

      ## file name
      filename.pricequantity.panel <- paste0("pricequantity_panel_", ticker, "_from_", from, "_to_", to, ".csv")

      ## store price quantity panel as csv
      data.table::fwrite(df.pricequantitypanel, paste0(path.pricequantitypanel, filename.pricequantity.panel))

      message("Price-quantity panel successfully created!")

    } else (message("No quantity panel available.")) ## end of if else statement to check whether quantity panel is available

  } else (message("No price panel available."))

} ## end of write_price_quantity_panel


# Helpers -----------------------------------------------------------------

get_tickers_from_transactions <- function(df.transaction.history, path, file.ticker = "isin_ticker.csv") {

  #### get all tickers from the history of all transactions

  ## create folder if not exists and get folder name for quantity panel and tickers
  list.paths <- PortfolioTracker::create_portfoliotracker_dir(path)
  path.tickers <- list.paths$path.tickers

  ## get table that converts ISIN to ticker
  df.isin.ticker <- data.table::fread(paste0(path.tickers, file.ticker))

  ## add ticker to transaction data
  df.transaction.history <- merge(df.transaction.history, df.isin.ticker, by = "isin")

  ## all tickers
  tickers <- unique(df.transaction.history$ticker)

  return(tickers)

} ## end of function get_tickers_from_transactions
