#' Get dividend history
#'
#' @usage get_dividends(path)
#' @param path A single character string. Directory of your data.
#' @return A data frame containing dividend history.
#'
#' @export
get_dividends <- function(path) {

  PortfolioTracker::get_user_names(path)

  file_path_dividends <- file.path(path.dividends, file.dividend.history)

  if (file.exists(file_path_dividends)) {

    df_dividends <- data.table::fread(file_path_dividends)

    df_dividends <- as.data.frame(df_dividends)

    df_dividends$name <- PortfolioTracker::clean_investment_names(df_dividends$name)

    df_dividends$transaction_date <- as.Date(df_dividends$transaction_date,
                                             format = "%d-%m-%Y")

    ## Order by date (most recent date at the top)
    df_dividends <- df_dividends[rev(order(df_dividends$transaction_date)), ]

    df_dividends$transaction_price <- as.numeric(formatC(
      df_dividends$transaction_price, digits = 2, format = "f"))
    df_dividends$transaction_value <- as.numeric(formatC(
      df_dividends$transaction_value, digits = 2, format = "f"))

    df_dividends <- df_dividends[, names(df_dividends) != "wkn"]
    df_dividends <- df_dividends[, names(df_dividends) != "document_page"]
    df_dividends <- df_dividends[, names(df_dividends) != "document_name"]
    df_dividends <- df_dividends[, names(df_dividends) != "transaction_fee"]
    df_dividends <- df_dividends[, names(df_dividends) != "transaction_time"]
    df_dividends <- df_dividends[, names(df_dividends) != "year"]
    df_dividends <- df_dividends[, names(df_dividends) != "yearmon"]

    df_dividends <- PortfolioTracker::clean_column_names(df_dividends)

    return(df_dividends)

  }

}


#' Get dividends by year
#'
#' @usage get_dividends_by_year(path)
#' @param path A single character string. Directory of your data.
#' @return A data frame containing dividends by year in EUR.
#'
#' @export
get_dividends_by_year <- function(path) {

  PortfolioTracker::get_user_names(path)

  file_path_dividends_yr <- file.path(path.dividends, file.dividend.year)

  if (file.exists(file_path_dividends_yr)) {

    df_dividends_year <- data.table::fread(file_path_dividends_yr)

    df_dividends_year$year <- lubridate::ymd(df_dividends_year$year,
                                             truncated = 2L)

    df_dividends_year <- as.data.frame(df_dividends_year)

    return(df_dividends_year)

  }

}

#' Get dividends by month
#'
#' @usage get_dividends_by_month(path)
#' @param path A single character string. Directory of your data.
#' @return A data frame containing dividends by year in EUR.
#'
#' @export
get_dividends_by_month <- function(path) {

  PortfolioTracker::get_user_names(path)

  file_path_dividends_month <- file.path(path.dividends, file.dividend.month)

  if (file.exists(file_path_dividends_month)) {

    df_dividends_month <- data.table::fread(file_path_dividends_month)

    df_dividends_month <- as.data.frame(df_dividends_month)

    df_dividends_month$yearmon <- as.Date(df_dividends_month$yearmon,
                                          format = "%Y-%m-%d")

    return(df_dividends_month)

  }

}
