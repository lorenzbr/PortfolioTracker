# Helpers -----------------------------------------------------------------

get_tickers_from_db <- function(df.transactions, db_path) {

  get_db_names(db_path)

  isin_ticker_exists <- file.exists(file.path(path.database, file.tickers.db))

  if (isin_ticker_exists) {

    df_isin_ticker <- data.table::fread(file.path(path.database, file.tickers.db))
    df_isin_ticker <- df_isin_ticker[df_isin_ticker$ticker != "", ]

    df.transactions <- merge(df.transactions,
                             df_isin_ticker,
                             by = "isin")

    tickers <- unique(df.transactions$ticker)

    return(tickers)

  }

}

get_tickers_from_transactions <- function(df.transaction.history, path) {

  get_user_names(path)

  isin.ticker.exists <- file.exists(file.path(path.tickers, file.tickers))

  if (isin.ticker.exists) {

    df.isin.ticker <- data.table::fread(file.path(path.tickers, file.tickers))
    df.isin.ticker <- df.isin.ticker[df.isin.ticker$ticker != "", ]

    df.transaction.history <- merge(df.transaction.history,
                                    df.isin.ticker,
                                    by = "isin")

    tickers <- unique(df.transaction.history$ticker)

    return(tickers)

  }

}

#' Get annualized returns based on daily returns for matrix or xts
#'
#' @usage get_annualized_returns(R, scale = 252)
#'
#' @param R An xts or matrix with returns
#' @param scale A numeric for the number of periods in a year (daily = 252 is
#' default, monthly = 12, quarterly = 4, yearly = 1)
#'
#' @return A data frame with annualized returns for all investments
#'
#' @export
get_annualized_returns <- function(R, scale = 252) {

  result <- apply(R, 2, get_annualized_return, scale = scale)
  dim(result) <- c(1, NCOL(R))
  colnames(result) <- colnames(R)
  rownames(result) = "annualized_return"
  result <- as.data.frame(t(result))

  return(result)

}

#' Get annualized return based on daily returns for vector
#'
#' @usage get_annualized_return(R, scale = 252)
#'
#' @param R An xts or vector with returns
#' @param scale A numeric for the number of periods in a year (daily = 252 is
#' default, monthly = 12, quarterly = 4, yearly = 1)
#'
#' @return A vector with annualized returns
#'
#' @export
get_annualized_return <- function(R, scale = 252) {

  R <- as.vector(R)
  R <- stats::na.omit(R)
  n <- length(R)
  result <- prod(1 + R)^(scale/n) - 1
  return(result)

}

#' Get data frame for specified time period
#'
#' @usage get_df_with_selected_time_period(df, nb_period = NULL, period_type = "max")
#' @param df A data frame containing a panel.
#' @param nb_period An integer indicating the number of months. Default is
#' \emph{NULL}. Does not need to be specified for period_type \emph{max} and \emph{ytd}.
#' @param period_type A single character string. Default \emph{max}. Possible
#' values \emph{max}, \emph{ytd}, \emph{weeks} and \emph{months}.
#'
#' @return A data frame containing the original data frame only for the specified time period.
#'
#' @export
get_df_with_selected_time_period <- function(df, nb_period = NULL, period_type = "max") {

  ## Reason for while loop: if first.date does not exist (i.e., NA) go one more day into the past
  ## E.g., Feb 29 does not exist for all years
  ## Holidays and weekend days may not exist either
  if (period_type == "months") {

    first.date <- Sys.Date() - months(nb_period)

    j <- 1
    while (is.na(first.date) && j < 10) {
      first.date <- (Sys.Date() - j) - months(nb_period)
      j = j + 1
    }

  } else if (period_type == "weeks") {

    first.date <- Sys.Date() - lubridate::weeks(nb_period)

    j <- 1
    while (is.na(first.date) && j < 10) {
      first.date <- (Sys.Date() - j) - lubridate::weeks(nb_period)
      j = j + 1
    }

  } else if (period_type == "days") {

    first.date <- Sys.Date() - lubridate::days(nb_period)

    j <- 1
    while (is.na(first.date) && j < 10) {
      first.date <- (Sys.Date() - j) - lubridate::days(nb_period)
      j = j + 1
    }

  } else if (period_type == "ytd") {

    first.date <- as.Date(paste0("01-01-", lubridate::year(Sys.Date())),
                          format = "%d-%m-%Y")

  } else if (period_type == "max") {

    df.selected.time.period <- df

  }

  if (period_type == "months" || period_type == "weeks" || period_type == "days"
      || period_type == "ytd") {

    df.selected.time.period <- df[df$date >= first.date, ]

  }

  return(df.selected.time.period)

}
