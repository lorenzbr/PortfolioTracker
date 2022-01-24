#' Update and store prices as csv based on new financial transactions
#'
#' @usage update_prices_based_on_transactions(df.transactions, path, external.search = TRUE)
#' @param df.transactions A data frame. Results from [BankStatementParser::get_transactions()]
#' @param path A single character string. Path where data are stored.
#' @param external.search Logical if TRUE, the function searches external sources to find the ticker.
#'
#' @export
update_prices_based_on_transactions <- function(df.transactions, path,
                                                external.search = TRUE) {

  get_names(path)

  ## To get prices, only purchase transactions make sense
  df.transactions <- df.transactions[df.transactions$transaction_type == "Purchase", ]

  ## Unique ISINs
  isins <- unique(df.transactions$isin)

  ## Update ISIN-ticker table
  update_ticker_isin(isins = isins,
                     path.tickers= path.tickers,
                     external.search = external.search)

  isin.ticker.exists <- file.exists(file.path(path.tickers, file.tickers))

  if ( isin.ticker.exists ) {

    ## get table that converts ISIN to ticker (which is needed by Yahoo Finance)
    df.isin.ticker <- data.table::fread(file.path(path.tickers, file.tickers))

    ## add ticker to transaction data
    df.transactions <- merge(df.transactions, df.isin.ticker, by = "isin",
                             all.x = TRUE)
    df.transactions <- df.transactions[!is.na(df.transactions$ticker), ]

    ## transaction date to date format
    df.transactions$transaction_date <- as.Date(df.transactions$transaction_date,
                                                "%d-%m-%Y")

    ## get current date
    today <- Sys.Date()

    ## For loop over all transactions in file
    ## to do: alternatively, use new function and apply function
    for ( i in 1:nrow(df.transactions) ) {

      tryCatch({

      ## select transaction date and ticker
      transaction.date <- df.transactions$transaction_date[i]
      ticker <- df.transactions$ticker[i]

      ## check whether prices for this ticker based on transaction date already exists
      # if no: continue
      # if yes: 1) if the focal date is younger, don't do anything 2) if the focal dates is older, continue

      ## get all prices with same ticker
      df.prices.same.ticker <- data.frame(filename = list.files(path.prices.raw,
                                                                pattern = ticker))

      ## initialize earliest.date (required to have an existing valid date)
      earliest.date <- as.Date("1900-01-01", "%Y-%m-%d")

      ## identify earliest date for each of those files and compare to transaction.date
      if ( nrow(df.prices.same.ticker) > 0 ) {

        df.prices.same.ticker$first_date <- stringr::str_match(df.prices.same.ticker$filename,
                                                               "from_(.*?)_to")[, 2]
        df.prices.same.ticker$first_date <- as.Date(df.prices.same.ticker$first_date,
                                                    "%Y-%m-%d")
        min.date <- min(df.prices.same.ticker$first_date)
        df.prices.same.ticker <- df.prices.same.ticker[df.prices.same.ticker$first_date == min.date, ]
        earliest.date <- unique(df.prices.same.ticker$first_date)

        ## If new transaction is older than earliest date updated "to" date
        if ( transaction.date < earliest.date && !(is.na(earliest.date)) ) today <- earliest.date - 1

      }

      ## file name for the data
      filename.data.raw.prices <- paste0("prices_ticker_", ticker, "_from_",
                                         transaction.date, "_to_", today, ".csv")

      ## If transaction.date is older than earliest.date of existing transactions
      ## or no prices with same ticker exist, do this
      if ( transaction.date < earliest.date || nrow(df.prices.same.ticker) == 0 ) {

        ## Check whether such a file exists already, then no need to download again
        if ( !file.exists(file.path(path.prices.raw, filename.data.raw.prices)) ) {

          ## Get price data from Yahoo Finance
          df.ticker.prices <- get_prices_from_yahoo(ticker,
                                                    from = transaction.date,
                                                    to = today)

          ## start and end date
          from <- min(df.ticker.prices$date)
          to <- max(df.ticker.prices$date)

          ## file name for the data
          filename.data.raw.prices <- paste0("prices_ticker_", ticker, "_from_",
                                             from, "_to_", to, ".csv")

          ## Store as csv in raw price data
          data.table::fwrite(df.ticker.prices, file.path(path.prices.raw,
                                                         filename.data.raw.prices))

          if ( today != Sys.Date() ) {

            # print(paste("Older transaction: Price update for", ticker, "from",
            #             transaction.date, "to",
            #             today, "successfully downloaded."))

          } else {

            # print(paste("Prices for", ticker, "from", transaction.date,
            #                    "to", today, "successfully downloaded."))

          }

        } else {

          # print(paste("Prices for", ticker, "from", transaction.date, "to",
          #             today, "already downloaded.", " File",
          #             filename.data.raw.prices, "exists already."))

        } ## End of else if condition which checks whether file already exists

      } else {

        # message("Prices based on older transaction already exist.")

      }
      ## End of if else statement: transaction.date older than any other or no transactions exist

      }, error = function(cond){

        # message(paste0("No prices for ticker '", ticker, "' available."))
        message("Original message:")
        message(cond)

      }

      )

    }

  }

}

#' Update most recent prices and store as csv
#'
#' @usage update_latest_prices(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
update_latest_prices <- function(path) {

  get_names(path)

  ## Load file names with price data
  filename.prices.raw.with.ticker <- list.files(path.prices.raw)

  ## Only run code if filename.prices.raw.with.ticker is not empty
  if ( length(filename.prices.raw.with.ticker) > 0 ) {

    ## create data frame
    df.files.price.data <- data.frame(filename = filename.prices.raw.with.ticker)

    ## identify last date
    df.files.price.data$last_date <- stringr::str_match(df.files.price.data$filename,
                                                        "to_(.*?).csv")[, 2]

    ## identify tickers
    df.files.price.data$ticker <- stringr::str_match(df.files.price.data$filename,
                                                     "ticker_(.*?)_from")[, 2]

    ## keep latest date for each ticker
    df.files.price.data <- stats::aggregate(last_date ~ ticker,
                                            data = df.files.price.data, max)

    ## get date of today
    today <- Sys.Date()

    ## keep only tickers which are not up to date
    df.files.price.data <- df.files.price.data[df.files.price.data$last_date < today, ]

    ## if at least one ticker is not up to date
    if ( nrow(df.files.price.data) > 0 ) {

      for ( i in 1:nrow(df.files.price.data) ) {

        skip_to_next <- FALSE

        tryCatch({

        from <- as.Date(df.files.price.data$last_date[i]) + 1
        ticker <- df.files.price.data$ticker[i]

        if (today > from) {

          ## get price data for ticker
          df.updated.prices <- get_prices_from_yahoo(ticker, from = from, to = today)

          if ( !is.null(df.updated.prices) ) {

            ## start and end date
            from <- min(df.updated.prices$date)
            to <- max(df.updated.prices$date)

            ## file name for the data
            filename.prices.raw <- paste0("prices_ticker_", ticker, "_from_", from, "_to_", to, ".csv")

            ## store as csv in raw financial data
            data.table::fwrite(df.updated.prices, file.path(path.prices.raw, filename.prices.raw))

            # print(paste("Prices for", ticker, "from", from, "to", to, "successfully downloaded."))

          } else {

            # print(paste("No recent prices for", ticker, "available"))

          }

        } else {

          # print(paste("Prices for ticker", ticker, "up to date."))

        }

        }, error = function(e) { skip_to_next <- TRUE } )

        if ( skip_to_next ) { next }

      }

    } else {

      # print("Everything up to date!")

    }

  } else {

    # print("No prices available for update.")

  }

}

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
                                  stock.exchanges = c(".DE", ".F", ".SG", ".MU", ".DU")) {

  ## Check if specific exchange for ticker has been saved in file. Then, takes this one
  ticker.exchange.file.exists <- file.exists(file.path(path.tickers, file.ticker.exchange))
  ticker.exchange.pair.exists <- FALSE

  if ( ticker.exchange.file.exists ) {

    df.ticker.exchanges <- data.table::fread(file.path(path.tickers, file.ticker.exchange))

    ## needed because using "ticker" produces same data frame
    ticker.current <- ticker
    df.ticker.exchange <- df.ticker.exchanges[df.ticker.exchanges$ticker == ticker.current, ]
    ticker.exchange.pair.exists <- nrow(df.ticker.exchange) == 1

    if ( ticker.exchange.pair.exists ) {
      stock.exchange <- df.ticker.exchange$exchange
      ticker.yahoo <- paste0(ticker, stock.exchange)
      }

  }


  if ( !exists("ticker.yahoo") ) {

    ## produce final ticker for Yahoo Finance
    if (preferred.stock.exchange == "Xetra") {
      stock.exchange <- ".DE"
    } else if (preferred.stock.exchange == "Frankfurt") {
      stock.exchange <- ".F"
    } else if (preferred.stock.exchange == "Stuttgart") {
      stock.exchange <- ".SG"
    } else if (preferred.stock.exchange == "Muenchen") {
      stock.exchange <- ".MU"
    } else if (preferred.stock.exchange == "Duesseldorf") {
      stock.exchange <- ".DU"
    }

    ticker.yahoo <- paste0(ticker, stock.exchange)

  }

  ## Get prices from Yahoo API
  try({
    ticker.prices <- quantmod::getSymbols(ticker.yahoo, from = from, to = to,
                                          auto.assign = FALSE, warnings = FALSE)
  })



  ## If not yet found and ticker with exchanges does not exist, iterate over all other stock exchanges to get prices
  if ( !ticker.exchange.file.exists && !exists("ticker.prices") ) {

    iter.stock.exchanges <- 1
    while ( !exists("ticker.prices") && iter.stock.exchanges <= length(stock.exchanges) ) {
      stock.exchange <- stock.exchanges[iter.stock.exchanges]
      ticker.yahoo <- paste0(ticker, stock.exchange)
      iter.stock.exchanges <- iter.stock.exchanges + 1
      try({
        ticker.prices <- quantmod::getSymbols(ticker.yahoo, from = from, to = to,
                                              auto.assign = FALSE, warnings = FALSE)
      })
    }

  } else if ( ticker.exchange.file.exists ) {

    if ( nrow(df.ticker.exchange) != 1 ) {

      iter.stock.exchanges <- 1
      while ( !exists("ticker.prices") && iter.stock.exchanges <= length(stock.exchanges) ) {
        stock.exchange <- stock.exchanges[iter.stock.exchanges]
        ticker.yahoo <- paste0(ticker, stock.exchange)
        iter.stock.exchanges <- iter.stock.exchanges + 1
        try({
          ticker.prices <- quantmod::getSymbols(ticker.yahoo, from = from, to = to,
                                                auto.assign = FALSE, warnings = FALSE)
        })
      }

    }

  }




  if ( exists("ticker.prices") ) {

    ## Store stock exchange and ticker in csv file if it not yet exists
    df.ticker.exchange <- data.frame(ticker = ticker, exchange = stock.exchange)

    if ( ticker.exchange.file.exists && !ticker.exchange.pair.exists ) {

      df.ticker.exchanges <- data.table::fread(file.path(path.tickers, file.ticker.exchange))

      df.ticker.exchanges <- rbind(df.ticker.exchanges, df.ticker.exchange)

      df.ticker.exchanges <- unique(df.ticker.exchanges)

      data.table::fwrite(df.ticker.exchanges, file.path(path.tickers, file.ticker.exchange))

    } else if ( !ticker.exchange.file.exists ) {

      data.table::fwrite(df.ticker.exchange, file.path(path.tickers, file.ticker.exchange))

    }

    ## convert time series with prices to data frame
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
    df.ticker.prices <- df.ticker.prices[!(is.na(df.ticker.prices$adjusted)), ]

    # message("Prices for ticker ", ticker.yahoo, " found.")

    return(df.ticker.prices)

  } else {

    # message("Prices for ticker ", ticker, " not found!")

    return(NULL)

  }

}
