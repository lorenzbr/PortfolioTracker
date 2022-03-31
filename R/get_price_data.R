#' Update and store prices in database based on new transactions
#'
#' @description Update and store prices in the database (currently csv files)
#' shared by all users. For each ticker, prices are written to a separate csv file.
#' Prices are updated if incoming transactions are older than the latest price
#' that is available according to a list with first and last date of a given ticker
#' (this list is stored in a csv as well). Prices are also updated if there are
#' no prices available for the given ticker.
#'
#' @usage update_db_prices_based_on_transactions(df_transactions, db_path,
#'          external_search = TRUE)
#' @param df_transactions A data frame. Results from
#' \code{\link[BankStatementParser]{get_transactions}}.
#' At least three variables/columns are required: \emph{transaction_type},
#' \emph{isin} and \emph{transaction_date}
#' @param db_path A single character string containing the directory of the database.
#' @param external_search Logical; if TRUE, the function searches external
#' sources to find the ticker.
#'
#' @export
update_db_prices_based_on_transactions <- function(df_transactions, db_path,
                                                   external_search = TRUE) {

  get_db_names(db_path)

  ## Only purchase transactions are relevant
  df_transactions <- df_transactions[df_transactions$transaction_type == "Purchase", ]

  isins <- unique(df_transactions$isin)

  ## With external search it may take very long (due to crawling a website)
  update_ticker_isin(isins = isins,
                     path_tickers = path.database,
                     file_tickers = file.tickers.db,
                     external_search = external_search)

  ## Get table that converts ISIN to ticker (which is needed by Yahoo Finance)
  df_isin_ticker <- data.table::fread(file.path(path.database, file.tickers.db))

  df_transactions <- merge(df_transactions, df_isin_ticker,
                           by = "isin", all.x = TRUE)
  df_transactions <- df_transactions[!is.na(df_transactions$ticker), ]

  if (nrow(df_transactions) > 0) {

    ## Be aware of the specific date format
    df_transactions$transaction_date <- as.Date(df_transactions$transaction_date,
                                                format = "%d-%m-%Y")

    ## Keep oldest transactions for each ticker because younger transactions
    ## produce price data which are a subset of older transactions
    ## Base solution is more convoluted but should be FASTER than dplyr for
    ## small data (select minimum date and if more than one choose 1 randomly)
    df_transactions <- as.data.frame(df_transactions)
    col_names_transactions <- names(df_transactions)
    df <- stats::aggregate(transaction_date ~ ticker, data = df_transactions, FUN = min)
    df_transactions <- merge(df_transactions, df, by = c("ticker", "transaction_date"))
    df_transactions <- do.call(rbind,
                               lapply(split(df_transactions, df_transactions$ticker),
                                      function(x) x[sample(nrow(x), 1), ]))
    df_transactions <- df_transactions[, col_names_transactions]
    row.names(df_transactions) <- 1:nrow(df_transactions)

    df_price_range <- get_available_price_date_range(
      path.database, file.ticker.price.available.db
    )

    today <- Sys.Date()

    for (i in 1:nrow(df_transactions)) {

      tryCatch({

        transaction_date <- df_transactions$transaction_date[i]
        ticker <- df_transactions$ticker[i]

        ## Select ticker, first and latest date in price available df
        df_prices_same_ticker <- df_price_range[df_price_range$ticker == ticker, ]

        ## Initialize earliest_date (required to have an existing valid date)
        earliest_date <- as.Date("1900-01-01", format = "%Y-%m-%d")

        if (nrow(df_prices_same_ticker) == 1) {

          earliest_date <- df_prices_same_ticker$first_date

          ## If new transaction is older than earliest date:
          ## Update prices from transaction_date until earliest_date - 1 day
          if (transaction_date < earliest_date && !is.na(earliest_date))
            today <- earliest_date - 1

        }

        ## If transaction_date is older than earliest_date of existing transactions
        ## or no prices with same ticker exist, get prices
        if (transaction_date < earliest_date || nrow(df_prices_same_ticker) == 0) {

          df_ticker_prices <- get_prices_from_yahoo(ticker = ticker,
                                                    from = transaction_date,
                                                    to = today,
                                                    user_specific_exchange = FALSE)

          if (!is.null(df_ticker_prices)) {

            #### Update price availability date range
            ## If prices are already available for given ticker, update dates
            ## in price availability csv
            from <- as.Date(min(df_ticker_prices$date), format = "%Y-%m-%d")
            to <- as.Date(max(df_ticker_prices$date), format = "%Y-%m-%d")
            is_current_ticker <- df_price_available$ticker == ticker
            if (length(df_price_available$first_date[is_current_ticker]) > 0) {
              if (from < earliest_date)
                df_price_available$first_date[is_current_ticker] <- from
              if (to > df_prices_same_ticker$last_date)
                df_price_available$last_date[is_current_ticker] <- to
            } else {
              df_new <- data.frame(ticker = ticker, first_date = from,
                                   last_date = to)
              df_price_available <- rbind(df_price_available, df_new)
            }
            data.table::fwrite(df_price_available,
                               file.path(path.database,
                                         file.ticker.price.available.db))

            #### Update csv with prices
            filename_prices <- paste0("prices_", ticker, ".csv")
            ## Append prices in correct order and store again as csv
            df_ticker_prices_all <- data.table::fread(file.path(path.prices.db,
                                                                filename_prices))
            ## Order of row binding data frames important because this should be
            ## correct already
            df_ticker_prices <- rbind(df_ticker_prices,
                                      df_ticker_prices_all)
            ## Order by date: latest date should be first (on top) - Is it redundant
            ## because of rbind?
            df_ticker_prices <- df_ticker_prices[order(df_ticker_prices$date), ]
            data.table::fwrite(df_ticker_prices,
                               file.path(path.prices.db, filename_prices))

          }

        }

      }, error = function(cond) {

        message("Original message:")
        message(cond)

      })

    }

  }

}

#' Update and store prices as csv based on new transactions
#'
#' @usage update_prices_based_on_transactions(df.transactions, path, external_search = TRUE)
#' @param df.transactions A data frame. Results from \code{\link[BankStatementParser]{get_transactions}}.
#' At least three variables/columns are required: \emph{transaction_type}, \emph{isin}
#' and \emph{transaction_date}
#' @param path A single character string. Path where data are stored.
#' @param external_search Logical; if TRUE, the function searches external
#' sources to find the ticker.
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
update_prices_based_on_transactions <- function(df.transactions, path,
                                                external_search = TRUE) {

  get_user_names(path)

  ## To get prices, only purchase transactions are relevant (?)
  df.transactions <- df.transactions[df.transactions$transaction_type == "Purchase", ]

  isins <- unique(df.transactions$isin)

  update_ticker_isin(isins = isins,
                     path_tickers = path.tickers,
                     file_tickers = file.tickers,
                     external_search = external_search)

  isin.ticker.exists <- file.exists(file.path(path.tickers, file.tickers))

  if (isin.ticker.exists) {

    ## Get table that converts ISIN to ticker (which is needed by Yahoo Finance)
    df.isin.ticker <- data.table::fread(file.path(path.tickers, file.tickers))

    df.transactions <- merge(df.transactions, df.isin.ticker, by = "isin",
                             all.x = TRUE)
    df.transactions <- df.transactions[!is.na(df.transactions$ticker), ]

    df.transactions$transaction_date <- as.Date(df.transactions$transaction_date,
                                                format = "%d-%m-%Y")

    ## Keep oldest transactions for each ticker because younger transactions
    ## produce price data which are a subset of older transactions
    df.transactions <- df.transactions %>%
      dplyr::group_by(.data$ticker) %>%
      dplyr::filter(transaction_date == min(.data$transaction_date))
    df.transactions <- df.transactions %>%
      dplyr::group_by(.data$ticker) %>%
      dplyr::sample_n(size = 1)

    ## Get current date
    today <- Sys.Date()

    ## For loop over all transactions in file
    ## To do: alternatively, use new function and apply function (probably NOT faster)
    for (i in 1:nrow(df.transactions)) {

      tryCatch({

        ## Select transaction date and ticker
        transaction.date <- df.transactions$transaction_date[i]
        ticker <- df.transactions$ticker[i]

        ## Check whether prices for this ticker based on transaction date already exists:
        ## if no: continue
        ## if yes: 1) if the focal date is younger, don't do anything 2) if the
        ## focal date is older, continue

        ## Get all prices with same ticker
        df.prices.same.ticker <- data.frame(filename = list.files(path.prices.raw,
                                                                  pattern = ticker))

        ## Initialize earliest.date (required to have an existing valid date)
        earliest.date <- as.Date("1900-01-01", format = "%Y-%m-%d")

        ## Identify earliest date for each of those files and compare to transaction.date
        if (nrow(df.prices.same.ticker) > 0) {

          df.prices.same.ticker$first_date <- stringr::str_match(df.prices.same.ticker$filename,
                                                                 "from_(.*?)_to")[, 2]
          df.prices.same.ticker$first_date <- as.Date(df.prices.same.ticker$first_date,
                                                      format = "%Y-%m-%d")
          min.date <- min(df.prices.same.ticker$first_date)
          df.prices.same.ticker <- df.prices.same.ticker[df.prices.same.ticker$first_date == min.date, ]
          earliest.date <- unique(df.prices.same.ticker$first_date)

          ## If new transaction is older than earliest date updated "to" date
          if (transaction.date < earliest.date && !is.na(earliest.date)) {
            today <- earliest.date - 1
          }

        }

        filename_data_raw_prices <- paste0("prices_ticker_", ticker, "_from_",
                                           transaction.date, "_to_", today, ".csv")

        ## If transaction.date is older than earliest.date of existing transactions
        ## or no prices with same ticker exist, do this
        if (transaction.date < earliest.date || nrow(df.prices.same.ticker) == 0) {

          ## Check whether such a file exists already, then no need to download again
          if (!file.exists(file.path(path.prices.raw, filename_data_raw_prices))) {

            df_ticker_prices <- get_prices_from_yahoo(ticker = ticker,
                                                      from = transaction.date,
                                                      to = today)

            filename_data_raw_prices <- paste0("prices_ticker_", ticker,
                                               "_from_",
                                               min(df_ticker_prices$date),
                                               "_to_",
                                               max(df_ticker_prices$date),
                                               ".csv")

            data.table::fwrite(df_ticker_prices, file.path(path.prices.raw,
                                                           filename_data_raw_prices))

          }

        }

      }, error = function(cond) {

        message("Original message:")
        message(cond)

      }

      )

    }

  }

}

#' Append most recent prices to existing files
#'
#' @description Append prices for all tickers specified as an argument.
#' The existing csv files are used to append prices.
#'
#' @usage append_latest_prices_db(db_path, tickers)
#' @param db_path A single character string. Path where data are stored.
#' @param tickers A vector of character strings containing tickers.
#'
#' @export
append_latest_prices_db <- function(db_path, tickers) {

  get_db_names(db_path)

  df_price_range <- get_available_price_date_range(
    path.database, file.ticker.price.available.db
  )

  ## Limit tickers to set of user relevant tickers
  df_price_range <- df_price_range[df_price_range$ticker %in% tickers, ]

  if (nrow(df_price_range) > 0) {

    today <- Sys.Date()

    ## Keep only tickers which are not up to date
    df_price_range <- df_price_range[df_price_range$last_date < today, ]

    ## If at least one ticker is not up to date
    if (nrow(df_price_range) > 0) {

      ## Loop over all tickers: get prices from Yahoo Finance API
      for (i in 1:nrow(df_price_range)) {

        skip_to_next <- FALSE

        tryCatch({

          ## Date "from" is the last date + one day (since last date exists already)
          from <- as.Date(df_price_range$last_date[i]) + 1

          ticker <- df_price_range$ticker[i]

          if (today > from) {

            df_updated_prices <- get_prices_from_yahoo(ticker = ticker,
                                                      from = from,
                                                      to = today,
                                                      user_specific_exchange = FALSE)

            if (!is.null(df_updated_prices)) {

              ## Update price availability date range
              to <- as.Date(max(df_updated_prices$date), format = "%Y-%m-%d")
              is_current_ticker <- df_price_range$ticker == ticker
              df_price_range$last_date[is_current_ticker] <- to
              data.table::fwrite(df_price_range,
                                 file.path(path.database,
                                           file.ticker.price.available.db))

              ## Append updated prices in csv with price data for current ticker
              data.table::fwrite(df_updated_prices,
                                 file.path(path.prices.db,
                                           paste0("prices_", ticker, ".csv")),
                                 append = TRUE)

            }

          }

        }, error = function(e) {

          skip_to_next <- TRUE

        })

        if (skip_to_next) next

      }

    }

  }

}

#' Append most recent prices to existing files
#'
#' @description Append prices for all tickers in the specified path. The existing
#' csv files are used.
#'
#' @usage append_latest_prices(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
append_latest_prices <- function(path) {

  ## Currently existing csv file names carry the date period for which prices available
  ## To do: use external csv with price availability

  get_user_names(path)

  filename.prices.raw.with.ticker <- list.files(path.prices.raw)

  if (length(filename.prices.raw.with.ticker) > 0) {

    df.files.price.data <- data.frame(filename = filename.prices.raw.with.ticker)

    df.files.price.data$first_date <- stringr::str_match(df.files.price.data$filename,
                                                        "from_(.*?)_to")[, 2]

    df.files.price.data$last_date <- stringr::str_match(df.files.price.data$filename,
                                                        "to_(.*?).csv")[, 2]

    df.files.price.data$ticker <- stringr::str_match(df.files.price.data$filename,
                                                     "ticker_(.*?)_from")[, 2]

    ## Keep latest date for each ticker
    ## To do: there should be only one file for each ticker anyways! So, no need
    ## to do this!?
    df.files.price.data2 <- stats::aggregate(last_date ~ ticker,
                                            data = df.files.price.data, max)
    df.files.price.data <- merge(df.files.price.data, df.files.price.data2,
                                 by = c("ticker", "last_date"))

    today <- Sys.Date()

    ## Keep only tickers which are not up to date
    df.files.price.data <- df.files.price.data[df.files.price.data$last_date < today, ]

    ## If at least one ticker is not up to date
    if (nrow(df.files.price.data) > 0) {

      ## Loop over all tickers: get prices from Yahoo Finance API
      for (i in 1:nrow(df.files.price.data)) {

        skip_to_next <- FALSE

        tryCatch({

          ## Date "from" is the last date + one day (since last date exists already)
          from <- as.Date(df.files.price.data$last_date[i]) + 1

          ticker <- df.files.price.data$ticker[i]
          current_filename <- df.files.price.data$filename[i]

          if (today > from) {

            df_updated_prices <- get_prices_from_yahoo(ticker = ticker,
                                                       from = from,
                                                       to = today)

            if (!is.null(df_updated_prices)) {

              filename_prices_raw <- paste0("prices_ticker_", ticker,
                                            "_from_", df.files.price.data$first_date[i],
                                            "_to_", max(df_updated_prices$date),
                                            ".csv")

              ## Possibly too slow: better to use different approach: create a
              ## separate csv file with ticker, first_date and last_date
              file.rename(from = file.path(path.prices.raw, current_filename),
                          to = file.path(path.prices.raw, filename_prices_raw))

              ## Append updated prices in csv with price data for current ticker
              data.table::fwrite(df_updated_prices,
                                 file.path(path.prices.raw, filename_prices_raw),
                                 append = TRUE)

            }

          }

        }, error = function(e) {

          skip_to_next <- TRUE

        })

        if (skip_to_next) next

      }

    }

  }

}

#' Update most recent prices and store in csv files
#'
#' Update prices for all tickers in the specified path. A new csv is created
#' for the updated prices. Better use \code{\link{append_latest_prices}} which is
#' more efficient.
#'
#' @usage update_latest_prices(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
update_latest_prices <- function(path) {

  get_user_names(path)

  filename.prices.raw.with.ticker <- list.files(path.prices.raw)

  if (length(filename.prices.raw.with.ticker) > 0) {

    df.files.price.data <- data.frame(filename = filename.prices.raw.with.ticker)

    df.files.price.data$last_date <- stringr::str_match(df.files.price.data$filename,
                                                        "to_(.*?).csv")[, 2]

    df.files.price.data$ticker <- stringr::str_match(df.files.price.data$filename,
                                                     "ticker_(.*?)_from")[, 2]

    df.files.price.data <- stats::aggregate(last_date ~ ticker,
                                            data = df.files.price.data, max)

    today <- Sys.Date()

    ## Keep only tickers which are not up to date
    df.files.price.data <- df.files.price.data[df.files.price.data$last_date < today, ]

    ## If at least one ticker is not up to date
    if (nrow(df.files.price.data) > 0) {

      for (i in 1:nrow(df.files.price.data)) {

        skip_to_next <- FALSE

        tryCatch({

        ## Date from is last date + 1 day
        from <- as.Date(df.files.price.data$last_date[i]) + 1
        ticker <- df.files.price.data$ticker[i]

          if (today > from) {

            df_updated_prices <- get_prices_from_yahoo(ticker = ticker,
                                                       from = from,
                                                       to = today)

            if (!is.null(df_updated_prices)) {

              filename_prices_raw <- paste0("prices_ticker_", ticker,
                                            "_from_", min(df_updated_prices$date),
                                            "_to_", max(df_updated_prices$date),
                                            ".csv")

              data.table::fwrite(df_updated_prices, file.path(path.prices.raw,
                                                              filename_prices_raw))

            }

          }

        }, error = function(e) { skip_to_next <- TRUE } )

        if (skip_to_next) next

      }

    }

  }

}

#' Get prices for a given ticker from the Yahoo Finance API
#'
#' @description Get prices for a given ticker from the Yahoo Finance API. Wrapper around
#' \code{\link{get_ticker_prices}} with further arguments. E.g., select the stock exchange.
#'
#' @usage get_prices_from_yahoo(ticker, from, to, preferred_exchange = "Xetra",
#'                              stock.exchanges = c(".DE", ".F", ".SG", ".MU", ".DU"),
#'                              method = "simple", user_specific_exchange = TRUE)
#' @param ticker A single character string. Ticker symbol.
#' @param from A single character string. Start date.
#' @param to A single character string. End date.
#' @param preferred_exchange A single character string. Stock exchange
#' (default is \emph{Xetra})
#' @param stock.exchanges A vector of single character strings. Possible stock
#' exchanges to get prices from.
#' @param method A single character string (default: \emph{simple}). The method
#' how to get prices from Yahoo. Use the R package \emph{quantmod} or a \emph{simple}
#' and fast solution.
#' @param user_specific_exchange Logical indicating whether prices are taken from
#' exchange that is preferred by the user (i.e., stored in the user's profile).
#' Default is \emph{TRUE}.
#'
#' @return A data frame containing dates, prices (open, high, low, close, adjusted) and volume.
#'
#' @export
get_prices_from_yahoo <- function(ticker, from, to, preferred_exchange = "Xetra",
                                  stock.exchanges = c(".DE", ".F", ".SG", ".MU", ".DU"),
                                  method = "simple", user_specific_exchange = TRUE) {


  if (user_specific_exchange) {
    path.tick <- path.tickers
    file.tick.ex <- file.ticker.exchange
  } else if (!user_specific_exchange) {
    path.tick <- path.database
    file.tick.ex <- file.ticker.exchange.db
  }

  ## Check if specific exchange for ticker has been saved in file.
  ## Then, takes this one
  ticker.exchange.file.exists <- file.exists(file.path(path.tick, file.tick.ex))

  ## Initiate: ticker exchange pair does not exists (in the following code
  ## this will be checked)
  ticker.exchange.pair.exists <- FALSE

  if (ticker.exchange.file.exists) {

    df.ticker.exchanges <- data.table::fread(file.path(path.tick, file.tick.ex))

    ## Needed because using "ticker" produces same data frame (?)
    ticker.current <- ticker
    df.ticker.exchange <- df.ticker.exchanges[df.ticker.exchanges$ticker == ticker.current, ]
    ticker.exchange.pair.exists <- nrow(df.ticker.exchange) == 1

    if (ticker.exchange.pair.exists) {
      stock.exchange <- df.ticker.exchange$exchange
      ticker.yahoo <- paste0(ticker, stock.exchange)
    }

  }


  if (!exists("ticker.yahoo")) {

    ## Produce final ticker for Yahoo Finance
    if (preferred_exchange == "Xetra") {
      stock.exchange <- ".DE"
    } else if (preferred_exchange == "Frankfurt") {
      stock.exchange <- ".F"
    } else if (preferred_exchange == "Stuttgart") {
      stock.exchange <- ".SG"
    } else if (preferred_exchange == "Muenchen") {
      stock.exchange <- ".MU"
    } else if (preferred_exchange == "Duesseldorf") {
      stock.exchange <- ".DU"
    }

    ticker.yahoo <- paste0(ticker, stock.exchange)

  }

  ## Get prices from Yahoo API
  try({
    if (method == "quantmod") {
      ticker.prices <- quantmod::getSymbols(ticker.yahoo,
                                            from = from,
                                            to = to,
                                            auto.assign = FALSE,
                                            warnings = FALSE)
    } else if (method == "simple") {
      ticker.prices <- get_ticker_prices(ticker.yahoo,
                                         from = from,
                                         to = to)
    }
  })



  ## If not yet found and ticker with exchanges does not exist, iterate over all
  ## other stock exchanges to get prices
  if (!ticker.exchange.file.exists && !exists("ticker.prices")) {

    iter.stock.exchanges <- 1
    while (!exists("ticker.prices") && iter.stock.exchanges <= length(stock.exchanges)) {
      stock.exchange <- stock.exchanges[iter.stock.exchanges]
      ticker.yahoo <- paste0(ticker, stock.exchange)
      iter.stock.exchanges <- iter.stock.exchanges + 1
      try({
        if (method == "quantmod") {
          ticker.prices <- quantmod::getSymbols(ticker.yahoo,
                                                from = from,
                                                to = to,
                                                auto.assign = FALSE,
                                                warnings = FALSE)
        } else if (method == "simple") {
          ticker.prices <- get_ticker_prices(ticker.yahoo,
                                             from = from,
                                             to = to)
        }
      })
    }

  } else if (ticker.exchange.file.exists) {

    if (nrow(df.ticker.exchange) != 1) {

      iter.stock.exchanges <- 1
      while (!exists("ticker.prices")
             && iter.stock.exchanges <= length(stock.exchanges)) {
        stock.exchange <- stock.exchanges[iter.stock.exchanges]
        ticker.yahoo <- paste0(ticker, stock.exchange)
        iter.stock.exchanges <- iter.stock.exchanges + 1
        try({
          if (method == "quantmod") {
            ticker.prices <- quantmod::getSymbols(Symbols = ticker.yahoo,
                                                  from = from, to = to,
                                                  auto.assign = FALSE,
                                                  warnings = FALSE)
          } else if (method == "simple") {
            ticker.prices <- get_ticker_prices(ticker = ticker.yahoo,
                                               from = from,
                                               to = to)
          }
        })
      }

    }

  }


  if (exists("ticker.prices")) {

    ## Store stock exchange and ticker in csv file if it not yet exists
    df.ticker.exchange <- data.frame(ticker = ticker, exchange = stock.exchange)

    if (ticker.exchange.file.exists && !ticker.exchange.pair.exists) {

      df.ticker.exchanges <- data.table::fread(file.path(path.tick, file.tick.ex))
      df.ticker.exchanges <- rbind(df.ticker.exchanges, df.ticker.exchange)
      df.ticker.exchanges <- unique(df.ticker.exchanges)

      data.table::fwrite(df.ticker.exchanges, file.path(path.tick, file.tick.ex))

    } else if (!ticker.exchange.file.exists) {

      data.table::fwrite(df.ticker.exchange, file.path(path.tick, file.tick.ex))

    }


    ## Convert time series with prices to data frame
    df.ticker.prices <- data.frame(ticker.prices)

    if (method == "quantmod") {
      names(df.ticker.prices) <- gsub(pattern = paste0(ticker.yahoo, "\\."),
                                      replacement = "",
                                      names(df.ticker.prices))
      df.ticker.prices$date <- rownames(df.ticker.prices)
      rownames(df.ticker.prices) <- 1:nrow(df.ticker.prices)
    } else if (method == "simple") {

      names(df.ticker.prices)[startsWith(names(df.ticker.prices), "Adj")] <- "Adjusted"
      ## Columns of data frame need to have same order as quantmod method
      df.ticker.prices <- df.ticker.prices[, c("Open", "High", "Low", "Close",
                                               "Volume", "Adjusted", "Date")]
    }

    names(df.ticker.prices) <- tolower(names(df.ticker.prices))

    df.ticker.prices$date <- as.Date(df.ticker.prices$date)

    df.ticker.prices <- df.ticker.prices[!is.na(df.ticker.prices$adjusted), ]
    df.ticker.prices <- df.ticker.prices[!is.null(df.ticker.prices$adjusted), ]
    df.ticker.prices <- df.ticker.prices[df.ticker.prices$adjusted != "NA", ]
    df.ticker.prices <- df.ticker.prices[df.ticker.prices$adjusted != "null", ]

    return(df.ticker.prices)

  } else if (!exists("ticker.prices")) {

    return(NULL)

  }

}

#' Get prices from the Yahoo Finance API fast and simple
#'
#' @usage get_ticker_prices(ticker, from, to)
#' @param ticker A single character string. Ticker symbol.
#' @param from A single character string. Start date.
#' @param to A single character string. End date.
#'
#' @return A data frame containing dates, prices (open, high, low, close, adjusted) and volume.
#'
#' @export
get_ticker_prices <- function(ticker, from, to) {

  ## See https://www.datasciencecentral.com/getting-historical-data-from-yahoo-finance-in-r/
  ## Be careful with excessive downloads because no limitation is included in this function
  url <- paste0("https://query1.finance.yahoo.com/v7/finance/download/",
                ticker,
                "?period1=",
                as.integer(as.POSIXct(from)),
                "&period2=",
                as.integer(as.POSIXct(to)),
                "&interval=1d&events=history")

  df <- data.table::fread(url)

  return(df)

}
