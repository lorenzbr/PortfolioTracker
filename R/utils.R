# Helpers -----------------------------------------------------------------

get_tickers_from_transactions <- function(df.transaction.history, path) {

  get_names(path)

  isin.ticker.exists <- file.exists(file.path(path.tickers, file.tickers))

  if (isin.ticker.exists) {

    ## get table that converts ISIN to ticker
    df.isin.ticker <- data.table::fread(file.path(path.tickers, file.tickers))
    df.isin.ticker <- df.isin.ticker[df.isin.ticker$ticker != "", ]

    ## add ticker to transaction data
    df.transaction.history <- merge(df.transaction.history, df.isin.ticker, by = "isin")

    ## all tickers
    tickers <- unique(df.transaction.history$ticker)

    return(tickers)

  }

}


get_df_with_selected_time_period <- function(df, nb_period = NULL, period_type = "max") {

  ## Reason for while loop: if first.date does not exist (i.e. NA) go one more day into the past
  ## E.g. Feb 29 does not exist for all years
  if (period_type == "months") {

    first.date <- Sys.Date() - months(nb_period)

    j <- 1
    while ( is.na(first.date) && j < 10) {
      first.date <- (Sys.Date() - j) - months(nb_period)
      j = j + 1
    }

  } else if (period_type == "weeks") {

    first.date <- Sys.Date() - lubridate::weeks(nb_period)

    j <- 1
    while ( is.na(first.date) && j < 10 ) {
      first.date <- (Sys.Date() - j) - lubridate::weeks(nb_period)
      j = j + 1
    }

  } else if (period_type == "days") {

    first.date <- Sys.Date() - lubridate::days(nb_period)

    j <- 1
    while ( is.na(first.date) && j < 10) {
      first.date <- (Sys.Date() - j) - lubridate::days(nb_period)
      j = j + 1
    }

  } else if (period_type == "max") {

    df.selected.time.period <- df

  }


  if (period_type == "months" || period_type == "weeks" || period_type == "days") {

    df.selected.time.period <- df[df$date >= first.date, ]

  }

  return(df.selected.time.period)

}
