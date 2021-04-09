#' Update and store prices as csv based on new financial transactions
#'
#' @usage update_prices_based_on_transactions(df.transactions, path,
#'                                            file.ticker = "isin_ticker.csv")
#' @param df.transactions A data frame. Results from [extractBankStatements::get_transactions()]
#' @param path A single character string. Folder where all data are stored.
#' @param file.ticker A single character string. Name of ISIN-ticker csv file (Default: isin_ticker.csv)
#'
#' @export
update_prices_based_on_transactions <- function(df.transactions, path, file.ticker = "isin_ticker.csv"){

  #### update and store prices for tickers as csv. Starts with transaction date

  ## create folders for tickers and prices (if not exists)
  list.paths <- portfoliotracker::create_portfoliotracker_dir(path)
  path.tickers <- list.paths$path.tickers
  path.prices.raw <- list.paths$path.prices.raw

  ## unique ISINs
  isins <- unique(df.transactions$isin)

  ## update ISIN-ticker table
  update_ticker_isin(isins, path.tickers)

  ## get table that converts ISIN to ticker (which is needed by Yahoo Finance)
  df.isin.ticker <- data.table::fread(paste0(path.tickers, file.ticker))

  ## add ticker to transaction data
  df.transactions <- merge(df.transactions, df.isin.ticker, by = "isin", all.x = T)

  ## transaction date to date format
  df.transactions$transaction_date <- as.Date(df.transactions$transaction_date, "%d-%m-%Y")

  ## get current date
  today <- Sys.Date()

  ## for loop over all transactions in file
  for(i in 1:nrow(df.transactions)){

    tryCatch({

    ## select transaction date and ticker
    transaction.date <- df.transactions$transaction_date[i]
    ticker <- df.transactions$ticker[i]

    ## check whether price data for this ticker based on transaction date already exists
    # if no: continue
    # if yes: 1) if the focal one is younger, don't do anything 2) if the focal one is older, continue

    ## get all price data with same ticker
    df.prices.same.ticker <- data.frame(filename = list.files(path.prices.raw, pattern = ticker))

    ## initialize earliest.date (required to have an existing date)
    earliest.date <- as.Date("1900-01-01", "%Y-%m-%d")

    ## identify earliest date for each of those files and compare to transaction.date
    if(nrow(df.prices.same.ticker) > 0){

      df.prices.same.ticker$first_date <- stringr::str_match(df.prices.same.ticker$filename, "from_(.*?)_to")[,2]
      df.prices.same.ticker$first_date <- as.Date(df.prices.same.ticker$first_date, "%Y-%m-%d")
      df.prices.same.ticker <- df.prices.same.ticker[df.prices.same.ticker$first_date == min(df.prices.same.ticker$first_date),]
      earliest.date <- unique(df.prices.same.ticker$first_date)

      ## if new transaction is older than earliest date updated "to" date
      if(transaction.date < earliest.date & !(is.na(earliest.date))){today <- earliest.date - 1}

    } ## end of if statement

    ## file name for the data
    filename.data.raw.prices <- paste0("prices_ticker_", ticker, "_from_", transaction.date, "_to_", today, ".csv")

    ## if transaction.date is older than earliest.date of existing transactions or no prices with same ticker exist, do this
    if(transaction.date < earliest.date | nrow(df.prices.same.ticker) == 0){

      ## check whether such a file exists already, then no need to download again
      if(!(file.exists(paste0(path.prices.raw, filename.data.raw.prices)))){

        ## get price data from Yahoo Finance
        df.ticker.prices <- portfoliotracker::get_prices_from_yahoo(ticker, from = transaction.date, to = today)

        ## start and end date
        from <- min(df.ticker.prices$date)
        to <- max(df.ticker.prices$date)

        ## file name for the data
        filename.data.raw.prices <- paste0("prices_ticker_", ticker, "_from_", from, "_to_", to, ".csv")

        ## store as csv in raw price data
        data.table::fwrite(df.ticker.prices, paste0(path.prices.raw, filename.data.raw.prices))

        if(today != Sys.Date()){

          print(paste("Older transaction: Price data update for", ticker, "from", transaction.date, "to", today, "successfully downloaded."))

        } else {print(paste("Price data for", ticker, "from", transaction.date, "to", today, "successfully downloaded."))}

      } else {

        print(paste("Price data for", ticker, "from", transaction.date, "to", today, "already downloaded.", " File",
                    filename.data.raw.prices, "exists already."))

      } ## end of else if condition which checks whether file already exists

    } else {print("Prices based on older transaction already exist.")} ## end of if else statement: transaction.date older than any other or no transactions exist

    }, error = function(cond){

      message(paste0("No prices for ticker '", ticker, "' available."))
      message("Original message:")
      message(cond)

    }

    ) ## end of try catch

  } ## end of for loop over all transactions in file

} ## end of function update_prices_based_on_transactions


#' Update most recent prices and store as csv
#'
#' @usage update_latest_prices(path)
#' @param path A single character string. Folder where all data are stored.
#'
#' @export
update_latest_prices <- function(path){

  #### update all price data based on all tickers in folder and last date for each ticker

  ## create folders for tickers and prices (if not exists)
  list.paths <- portfoliotracker::create_portfoliotracker_dir(path)
  path.tickers <- list.paths$path.tickers
  path.prices.raw <- list.paths$path.prices.raw

  ## load file names for price data
  filename.prices.raw.with.ticker <- list.files(path.prices.raw)

  ## only run code if filename.prices.raw.with.ticker is not empty
  if(!(rlang::is_empty(filename.prices.raw.with.ticker))){

    ## create data frame
    df.files.price.data <- data.frame(filename = filename.prices.raw.with.ticker)

    ## identify last date
    df.files.price.data$last_date <- stringr::str_match(df.files.price.data$filename, "to_(.*?).csv")[,2]

    ## identify tickers
    df.files.price.data$ticker <- stringr::str_match(df.files.price.data$filename, "ticker_(.*?)_from")[,2]

    ## keep latest date for each ticker
    df.files.price.data <- stats::aggregate(last_date ~ ticker, data = df.files.price.data, max)

    ## get date of today
    today <- Sys.Date()

    ## keep only tickers which are not up to date
    df.files.price.data <- df.files.price.data[df.files.price.data$last_date < today,]

    ## if at least one ticker is not up to date
    if(nrow(df.files.price.data) > 0){

      for(i in 1:nrow(df.files.price.data)){

        skip_to_next <- FALSE

        tryCatch({

        from <- as.Date(df.files.price.data$last_date[i]) + 1
        ticker <- df.files.price.data$ticker[i]

        if(today > from){

          ## get price data for ticker
          df.updated.prices <- portfoliotracker::get_prices_from_yahoo(ticker, from, today)

          ## start and end date
          from <- min(df.updated.prices$date)
          to <- max(df.updated.prices$date)

          ## file name for the data
          filename.prices.raw <- paste0("prices_ticker_", ticker, "_from_", from, "_to_", to, ".csv")

          ## store as csv in raw financial data
          data.table::fwrite(df.updated.prices, paste0(path.prices.raw, filename.prices.raw))

          print(paste("Prices for", ticker, "from", from, "to", to, "successfully downloaded."))

        } else {print(paste("Prices for ticker", ticker, "up to date."))}

        }, error = function(e) { skip_to_next <- TRUE})

        if(skip_to_next) { next }

      } ## end of for loop over all tickers which are not up to date


    } else {

      print("Everything up to date!")

    } ## end of else statement that checks whether updates are needed

  } else {print("No prices available for update.")} ## end of if else statement whether file names are non empty

} ## end of function update_latest_prices

#' Get price data from Yahoo Finance
#'
#' @usage get_prices_from_yahoo(ticker, from, to, preferred.stock.exchange = "Xetra",
#'                           stock.exchanges = c(".DE", ".F", ".SG", ".MU", ".DU"))
#' @param ticker A single character string. Ticker symbol.
#' @param from A single character string. Start date.
#' @param to A single character string. End date.
#' @param preferred.stock.exchange A single character string. Stock exchange (default is empty string)
#' @param stock.exchanges A vector of single character strings. Possible stock exchanges to get price data.
#'
#' @export
get_prices_from_yahoo <- function(ticker, from, to, preferred.stock.exchange = "Xetra",
                                  stock.exchanges = c(".DE", ".F", ".SG", ".MU", ".DU")){

  #### get prices from Yahoo Finance API and clean output data a bit

  ## produce final ticker for Yahoo Finance
  if (preferred.stock.exchange == "Xetra") {
    ticker.yahoo <- paste0(ticker, ".DE")
  } else if (preferred.stock.exchange == "Frankfurt") {
    ticker.yahoo <- paste0(ticker, ".F")
  } else if (preferred.stock.exchange == "Stuttgart") {
    ticker.yahoo <- paste0(ticker, ".SG")
  } else if (preferred.stock.exchange == "Muenchen") {
    ticker.yahoo <- paste0(ticker, ".MU")
  } else if (preferred.stock.exchange == "Duesseldorf") {
    ticker.yahoo <- paste0(ticker, ".DU")
  }

  ## get prices
  try(ticker.prices <- quantmod::getSymbols(ticker.yahoo, from = from, to = to, auto.assign = FALSE))

  iter.stock.exchanges <- 1
  while (!exists("ticker.prices" && iter.stock.exchanges <= length(stock.exchanges))) {
    stock.exchange <- stock.exchanges[iter.stock.exchanges]
    ticker.yahoo <- paste0(ticker, stock.exchange)
    iter.stock.exchanges <- iter.stock.exchanges + 1
    try(ticker.prices <- quantmod::getSymbols(ticker.yahoo, from = from, to = to, auto.assign = FALSE))
  }

  ## convert to data frame
  df.ticker.prices <- data.frame(ticker.prices)

  ## change column names
  names(df.ticker.prices) <- gsub(paste0(ticker.yahoo, "\\."), "", names(df.ticker.prices))

  ## column names to lower case
  names(df.ticker.prices) <- tolower(names(df.ticker.prices))

  ## create date variable based on row names
  df.ticker.prices$date <- rownames(df.ticker.prices)

  ## new index for row names
  rownames(df.ticker.prices) <- 1:nrow(df.ticker.prices)

  ##  convert dates to date format
  df.ticker.prices$date <- as.Date(df.ticker.prices$date)

  ## remove entries with prices equal to NA
  df.ticker.prices <- df.ticker.prices[!(is.na(df.ticker.prices$adjusted)),]

  print(paste("Prices for ticker", ticker.yahoo, "found."))

  return(df.ticker.prices)

} ## end of function get_prices_from_yahoo
