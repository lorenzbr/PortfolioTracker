# Helpers -----------------------------------------------------------------

#' Get dates of available prices for all tickers in database
#'
#' @usage get_available_price_date_range(path.database, file.ticker.price.available.db)
#' @param path.database A path where prices are stored.
#' @param file.ticker.price.available.db A single character string containing the directory of the database.
#'
#' @return A data frame containing the tickers with dates of available prices.
#'
#' @export
get_available_price_date_range <- function(path.database,
                                           file.ticker.price.available.db) {

  file_path_price_available <- file.path(path.database,
                                         file.ticker.price.available.db)

  df_price_range <- data.table::fread(file_path_price_available)
  df_price_range <- as.data.frame(df_price_range)

  return(df_price_range)

}

#' Get transactions with tickers
#'
#' @usage get_tickers_from_db(df_transactions, db_path)
#' @param df_transactions A data frame containing the transaction history.
#' @param db_path A single character string containing the directory of the database.
#'
#' @return A list with a data frame containing transactions including the ticker
#' and a vector of all unique tickers in this data frame.
#'
#' @export
get_tickers_from_db <- function(df_transactions, db_path) {

  get_db_names(db_path)

  file_path_tickers <- file.path(path.database, file.tickers.db)

  df_isin_ticker <- data.table::fread(file_path_tickers)
  df_isin_ticker <- df_isin_ticker[df_isin_ticker$ticker != "", ]

  df_transactions <- merge(df_transactions,
                           df_isin_ticker,
                           by = "isin")

  tickers <- unique(df_transactions$ticker)

  return(list(df_transactions, tickers))


}

get_tickers_from_transactions <- function(df_transactions, path) {

  get_user_names(path)

  file_path_tickers <- file.path(path.tickers, file.tickers)

  if (file.exists(file_path_tickers)) {

    df_isin_ticker <- data.table::fread(file_path_tickers)
    df_isin_ticker <- df_isin_ticker[df_isin_ticker$ticker != "", ]

    df_transactions <- merge(df_transactions,
                             df_isin_ticker,
                                    by = "isin")

    tickers <- unique(df_transactions$ticker)

    return(tickers)

  }

}

#' Compute annualized returns based on daily returns for matrix or xts
#'
#' @usage compute_annualized_returns(R, scale = 252)
#'
#' @param R An xts or matrix with returns
#' @param scale A numeric for the number of periods in a year (daily = 252 is
#' default, monthly = 12, quarterly = 4, yearly = 1)
#'
#' @return A data frame with annualized returns for all investments
#'
#' @export
compute_annualized_returns <- function(R, scale = 252) {

  result <- apply(R, 2, compute_annualized_return, scale = scale)
  dim(result) <- c(1, NCOL(R))
  colnames(result) <- colnames(R)
  rownames(result) = "annualized_return"
  result <- as.data.frame(t(result))

  return(result)

}

#' Compute annualized return based on daily returns for vector
#'
#' @usage compute_annualized_return(R, scale = 252)
#'
#' @param R An xts or vector with returns
#' @param scale A numeric for the number of periods in a year (daily = 252 is
#' default, monthly = 12, quarterly = 4, yearly = 1)
#'
#' @return A vector with annualized returns
#'
#' @export
compute_annualized_return <- function(R, scale = 252) {

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

  ## Reason for while loop: if first_date does not exist (i.e., NA) go one more
  ## day into the past
  ## E.g., Feb 29 does not exist for all years, April 31, Feb 30 and Feb 31 do
  ## not exist
  ## Holidays and weekend days may not exist either
  if (period_type == "months") {

    first_date <- Sys.Date() - months(nb_period)

    j <- 1
    while (is.na(first_date) && j < 10) {
      first_date <- (Sys.Date() - j) - months(nb_period)
      j = j + 1
    }

  } else if (period_type == "weeks") {

    first_date <- Sys.Date() - lubridate::weeks(nb_period)

    j <- 1
    while (is.na(first_date) && j < 10) {
      first_date <- (Sys.Date() - j) - lubridate::weeks(nb_period)
      j = j + 1
    }

  } else if (period_type == "days") {

    first_date <- Sys.Date() - lubridate::days(nb_period)

    j <- 1
    while (is.na(first_date) && j < 10) {
      first_date <- (Sys.Date() - j) - lubridate::days(nb_period)
      j = j + 1
    }

  } else if (period_type == "ytd") {

    first_date <- as.Date(paste0("01-01-", lubridate::year(Sys.Date())),
                          format = "%d-%m-%Y")

  } else if (period_type == "max") {

    df_selected_time_period <- df

  }

  if (period_type == "months" ||
      period_type == "weeks" ||
      period_type == "days" ||
      period_type == "ytd") {

    df_selected_time_period <- df[df$date >= first_date, ]

  }

  return(df_selected_time_period)

}

#' Format column names of a data frame
#'
#' @usage clean_column_names(df)
#' @param df A data frame with unformatted column names.
#'
#' @return A data frame with formatted column names.
#'
#' @export
clean_column_names <- function(df) {

  df <- as.data.frame(df)

  names(df)[names(df) == "isin"] <- "ISIN"
  names(df)[names(df) == "wkn"] <- "WKN"
  names(df)[names(df) == "name"] <- "Name"
  names(df)[names(df) == "quantity"] <- "Quantity"
  names(df)[names(df) == "transaction_price"] <- "Price [EUR]"
  names(df)[names(df) == "transaction_value"] <- "Value [EUR]"
  names(df)[names(df) == "value"] <- "Value [EUR]"
  names(df)[names(df) == "transaction_date"] <- "Date"
  names(df)[names(df) == "transaction_type"] <- "Type"
  names(df)[names(df) == "transaction_fee"] <- "Fee [EUR]"
  names(df)[names(df) == "transaction_time"] <- "Time"
  names(df)[names(df) == "document_page"] <- "Document page"
  names(df)[names(df) == "document_name"] <- "Document name"

  names(df)[names(df) == "ticker"] <- "Ticker"
  names(df)[names(df) == "investment"] <- "Investment"
  names(df)[names(df) == "income"] <- "Income"
  names(df)[names(df) == "return_absolute"] <- "Return [EUR]"
  names(df)[names(df) == "return_percent"] <- "Return [%]"
  names(df)[names(df) == "quantity"] <- "Quantity"

  names(df)[names(df) == "weight"] <- "Weight [%]"

  names(df)[names(df) == "actual_share"] <- "Actual [%]"
  names(df)[names(df) == "target_share"] <- "Target [%]"
  names(df)[names(df) == "value_to_invest"] <- "Value to invest [EUR]"
  names(df)[names(df) == "new_value"] <- "New value [EUR]"
  names(df)[names(df) == "new_share"] <- "New [%]"
  names(df)[names(df) == "target_value"] <- "Target value [EUR]"
  names(df)[names(df) == "share_deviation"] <- "Deviation [%]"
  names(df)[names(df) == "value_deviation"] <- "Value deviation"

  return(df)

}

#' Clean names of investments
#'
#' @usage clean_investment_names(investment_names)
#' @param investment_names A single character string.
#'
#' @return A string with cleaned names.
#'
#' @export
clean_investment_names <- function(investment_names) {

  investment_names <- gsub("UCITS ETF|UC.ETF|U.ETF||Inhaber-Anteile|Reg\\.Shs|Registered Shares|Reg\\. Shares|oN$|o\\.N\\.$", "",
                           investment_names)
  investment_names <- gsub("\\s+", " ", investment_names)
  investment_names <- gsub("^\\s+|\\s+$", "", investment_names)

  return(investment_names)

}
