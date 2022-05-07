#' Format current portfolio
#'
#' @usage format_current_portfolio(df_investments)
#' @param df_investments A data frame containing current portfolio.
#'
#' @return A data frame containing the current portfolio with formatted
#' names and numbers.
#'
#' @export
format_current_portfolio <- function(df_investments) {

  names_investments <- c("Name", "ISIN", "Ticker", "Price [EUR]",
                         "Quantity", "Value [EUR]", "Weight [%]")

  if (!is.null(df_investments)) {

    if (nrow(df_investments) > 0) {

      df_investments$weight <- as.numeric(
        formatC(df_investments$weight * 100, digits = 2, format = "f"))
      df_investments$adjusted <- as.numeric(
        formatC(df_investments$adjusted, digits = 2, format = "f"))
      df_investments$value <- as.numeric(
        formatC(df_investments$value, digits = 2, format = "f"))

    } else {

      df_investments <- data.frame(
        matrix(nrow = 0, ncol = length(names_investments),
               dimnames = list(NULL, names_investments)))

    }

    names(df_investments) <- names_investments

    return(df_investments)

  }

}

#' Format previous investment portfolio
#'
#' @usage format_previous_investments(df_investments)
#' @param df_investments A data frame containing previous investments
#'
#' @return A data frame containing previous investments with formatted
#' names and numbers.
#'
#' @export
format_previous_investments <- function(df_investments) {

  names_investments <- names(df_investments)

  if (!is.null(df_investments)) {

    if (nrow(df_investments) > 0) {

      df_investments$investment <- as.numeric(
        formatC(df_investments$investment, digits = 2, format = "f"))
      df_investments$income <- as.numeric(
        formatC(df_investments$income, digits = 2, format = "f"))
      df_investments$return_absolute <- as.numeric(
        formatC(df_investments$return_absolute, digits = 2, format = "f"))
      df_investments$return_percent <- as.numeric(
        formatC(df_investments$return_percent * 100, digits = 2, format = "f"))

    } else {

      df_investments <- data.frame(
        matrix(nrow = 0, ncol = length(names_investments),
               dimnames = list(NULL, names_investments)))

    }

    df_investments <- PortfolioTracker::clean_column_names(df_investments)

    return(df_investments)

  }

}

#' Format current or previous stock splits
#'
#' @usage format_stock_splits(df_stock_splits)
#' @param df_stock_splits A data frame containing current or previous
#' stock splits
#'
#' @return A data frame containing the current or previous stock splits
#' with formatted names and numbers.
#'
#' @export
format_stock_splits <- function(df_stock_splits) {

  # names_stock_splits <- c("Name", "ISIN", "Ticker", "Stock split")
  names_stock_splits <- c("Ticker", "Date", "Stock split")

  if (!is.null(df_stock_splits)) {

    if (nrow(df_stock_splits) > 0) {

      # ...

    } else {

      df_stock_splits <- data.frame(
        matrix(nrow = 0, ncol = length(names_stock_splits),
               dimnames = list(NULL, names_stock_splits)))

    }

    names(df_stock_splits) <- names_stock_splits

    return(df_stock_splits)

  }

}


#' Format table with returns
#'
#' @usage format_return_tables(path, df_returns)
#' @param path A single character string. Directory of the data.
#' @param df_returns A data frame with table structure of returns,
#' i.e., ticker and time periods.
#'
#' @return A data frame containing formatted table with returns.
#'
#' @export
format_return_tables <- function(path, df_returns) {

  PortfolioTracker::get_user_names(path)

  file_path_transactions <- file.path(path.transactions, file.transactions)

  if (file.exists(file_path_transactions)) {

    df_transactions <- data.table::fread(file_path_transactions)

    df_isin_tickers <- data.table::fread(file.path(path.database,
                                                   file.tickers.db))

    df_returns <- as.data.frame(df_returns)
    df_isin_tickers <- as.data.frame(df_isin_tickers)
    df_transactions <- as.data.frame(df_transactions)

    df_isin_name_ticker <- unique(df_transactions[, c("isin", "name")])
    df_isin_name_ticker <- merge(df_isin_name_ticker,
                                 df_isin_tickers,
                                 by = "isin")

    df_isin_name_ticker$name <- PortfolioTracker::clean_investment_names(
      df_isin_name_ticker$name)

    df_returns <- merge(df_isin_name_ticker, df_returns, by = "ticker")

    names(df_returns)[names(df_returns) == "ticker"] <- "Ticker"
    names(df_returns)[names(df_returns) == "isin"] <- "ISIN"
    names(df_returns)[names(df_returns) == "name"] <- "Name"
    names(df_returns)[names(df_returns) == "ytd"] <- "YTD"
    names(df_returns)[names(df_returns) == "1y"] <- "1Y"
    names(df_returns)[names(df_returns) == "3y"] <- "3Y"
    names(df_returns)[names(df_returns) == "5y"] <- "5Y"
    names(df_returns)[names(df_returns) == "10y"] <- "10Y"
    names(df_returns)[names(df_returns) == "max"] <- "Max"

    col_names <- names(df_returns)[names(df_returns) != "Ticker"
                                   & names(df_returns) != "ISIN"
                                   & names(df_returns) != "Name"]

    if (nrow(df_returns) > 0) {

      df_returns[, col_names] <- df_returns[, col_names] * 100
      df_returns[, col_names] <- lapply(df_returns[, col_names], formatC,
                                        digits = 2, format = "f")
      suppressWarnings(df_returns[, col_names] <- lapply(
        df_returns[, col_names], as.numeric))
      df_returns[is.na(df_returns)] <- "-"

    }

    ## Order columns
    col_names <- c("Ticker", "ISIN", "Name", "1Y", "3Y", "5Y", "10Y", "Max")
    df_returns <- df_returns[, col_names]

    return(df_returns)

  }

}

#' Format rebalanced portfolio
#'
#' @usage format_rebalanced_portfolio(df_investments)
#' @param df_investments A data frame containing a rebalanced portfolio.
#'
#' @return A data frame containing the rebalanced portfolio with
#' formatted numbers.
#'
#' @export
format_rebalanced_portfolio <- function(df_investments) {

  df_investments$value <- as.numeric(
    formatC(df_investments$value, digits = 2, format = "f"))
  df_investments$new_value <- as.numeric(
    formatC(df_investments$new_value, digits = 2, format = "f"))
  df_investments$target_value <- as.numeric(
    formatC(df_investments$target_value, digits = 2, format = "f"))
  df_investments$value_deviation <- as.numeric(
    formatC(df_investments$value_deviation, digits = 2, format = "f"))

  df_investments$actual_share <- as.numeric(
    formatC(df_investments$actual_share * 100, digits = 2, format = "f"))
  df_investments$target_share <- as.numeric(
    formatC(df_investments$target_share * 100, digits = 2, format = "f"))
  df_investments$new_share <- as.numeric(
    formatC(df_investments$new_share * 100, digits = 2, format = "f"))
  df_investments$share_deviation <- as.numeric(
    formatC(df_investments$share_deviation * 100, digits = 2, format = "f"))

  return(df_investments)

}

