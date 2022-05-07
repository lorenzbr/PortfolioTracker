#' Get stock splits from the Yahoo Finance API
#'
#' @usage get_stock_splits_from_yahoo(ticker, from = "1970-01-01",
#'                                    to = Sys.Date())
#' @param ticker A single character string. Ticker symbol.
#' @param from A single character string. Start date. Default is 01-01-1970.
#' @param to A single character string. End date. Default is current date.
#'
#' @return A data frame containing dates and stock splits.
#'
#' @export
get_stock_splits_from_yahoo <- function(ticker, from = "1970-01-01",
                                        to = Sys.Date()) {

  ## See https://www.datasciencecentral.com/getting-historical-data-from-yahoo-finance-in-r/
  ## Be careful with excessive downloads because no limitation is included in this function
  url <- paste0("https://query2.finance.yahoo.com/v7/finance/download/",
                ticker,
                "?period1=",
                as.integer(as.POSIXct(from)),
                "&period2=",
                as.integer(as.POSIXct(to)),
                "&interval=1d&events=split")

  df <- data.table::fread(url)

  names(df) <- c("date", "stock_split")

  return(df)

}


#' Get quantity panel for ticker corrected for stock splits
#'
#' @usage correct_quantity_panels_for_stock_splits(
#'          df_panel, ticker, df_transactions_with_tickers, user_path)
#' @param df_panel A data frame which contains a quantity panel for a given ticker.
#' @param ticker A single character string containing the ticker symbol.
#' @param df_transactions_with_tickers A data frame containing transactions
#' and ticker.
#' @param user_path A single character string containing the directory of the user.
#'
#' @return A data frame which contains a quantity panel corrected for stock splits.
#'
#' @export
correct_quantity_panels_for_stock_splits <- function(
  df_panel, ticker, df_transactions_with_tickers, user_path) {

  get_user_names(user_path)

  file_path_stock_splits_db <- file.path(
    path.database, file.stock.splits.db)
  file_path_stock_splits_log <- file.path(path.logs, file.stock.splits.log)
  df_stock_splits <- data.table::fread(file_path_stock_splits_db)
  df_stock_splits_log <- data.table::fread(file_path_stock_splits_log)
  ## Coerce data frame because "== ticker" in the subsequent lines requires this
  df_stock_splits <- as.data.frame(df_stock_splits)
  df_stock_splits_log <- as.data.frame(df_stock_splits_log)
  ## 1. Check if stock splits for ticker are up to date (max(date) == Sys.Date())
  ## If yes, use "df_stock_splits"
  last_updated <- df_stock_splits_log$last_updated[df_stock_splits_log$ticker == ticker]

  if (length(last_updated) > 0) {
    if (last_updated == Sys.Date()) {
      df_splits <- df_stock_splits[df_stock_splits$ticker == ticker, ]
    }
  }

  if (length(last_updated) == 0 || last_updated < Sys.Date()) {
    ## For Yahoo finance need to get ticker with stock exchange
    file_path_ticker_exchange <- file.path(path.database, file.ticker.exchange.db)
    df_ticker_exchanges <- data.table::fread(file_path_ticker_exchange)
    exchange <- df_ticker_exchanges$exchange[df_ticker_exchanges$ticker == ticker]
    ticker_ex <- paste0(ticker, exchange[1])
    ## try is needed because otherwise error is thrown during R CMD CHECK
    try(splits <- suppressWarnings(quantmod::getSplits(ticker_ex)))
    ## Alternatively use own function
    # df_splits <- get_stock_splits_from_yahoo(ticker_ex)
    if (exists("splits")) {
      if (any(!is.na(splits))) {
        df_splits <- data.frame(splits)
        df_splits$date <- row.names(df_splits)
        df_splits$stock_split <- df_splits[[1]]
        df_splits$ticker <- ticker
        df_splits <- df_splits[, c("ticker", "date", "stock_split")]
        row.names(df_splits) <- 1:nrow(df_splits)
        ## Write in stock.splits.db (check for non-unique entries)
        ## Remove all splits for given ticker and add updated splits
        df_stock_splits <- df_stock_splits[!(df_stock_splits$ticker %in% ticker), ]
        df_stock_splits <- rbind(df_stock_splits, df_splits)
        data.table::fwrite(df_stock_splits, file_path_stock_splits_db)
        ## Update log file
        df_stock_splits_log <- df_stock_splits_log[!(df_stock_splits_log$ticker %in% ticker), ]
        last_updated <- as.character(Sys.Date())
        df_stock_splits_log <- rbind(df_stock_splits_log,
                                     data.frame(ticker, last_updated))
        data.table::fwrite(df_stock_splits_log, file_path_stock_splits_log)
      }
    }
  }

  if (exists("df_splits")) {
    ## Keep only splits after first purchase because the purchase price was
    ## executed after those earlier splits. They are therefore not of interest
    df_splits <- df_splits[df_splits$date >= min(df_panel$date), ]
    for(i in 1:nrow(df_splits)) {
      # date_after_split <- df_panel$date >= df_splits$date[i]
      # df_panel$cum_quantity[date_after_split] <- df_panel$cum_quantity[date_after_split] / df_splits$stock_split[i]
      ## Change quantities for all entries because prices are changed for the entire history
      ## This may be the best solution because otherwise, you would have jumps in prices
      df_panel$cum_quantity <- df_panel$cum_quantity / df_splits$stock_split[i]
      df_panel$quantity <- df_panel$quantity / df_splits$stock_split[i]
      if (df_splits$date[i] >= min(df_panel$date)) {
        if (df_panel$cum_quantity[df_panel$date == max(df_panel$date)] > 0) {
          file_path_stock_splits_current <- file.path(
            path.data, file.stock.splits.current)
          data.table::fwrite(df_splits[i, ],
                             file_path_stock_splits_current,
                             append = TRUE)
        } else {
          ## Write only to previous stocks if stock splits was during the period
          ## when stocks were held. Stock is still used for quantity panel
          if (df_splits$date[i] <= max(df_panel$date)) {
            file_path_stock_splits_previous <- file.path(
              path.data, file.stock.splits.previous)
            data.table::fwrite(df_splits[i, ],
                               file_path_stock_splits_previous,
                               append = TRUE)
          }
        }
      }
    }
  }

  return(df_panel)

}
