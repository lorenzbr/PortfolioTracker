#' Get annual returns
#'
#' @usage get_annual_returns(path)
#' @param path A single character string. Directory of your data.
#'
#' @return A data frame containing current portfolio.
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
get_annual_returns <- function(path) {

  PortfolioTracker::get_user_names(path)

  file_path_annual_returns <- file.path(path.returns, file.returns.annual)

  if (file.exists(file_path_annual_returns)) {

    df_annual_returns <- data.table::fread(file_path_annual_returns)

    df_annual_returns <- as.data.frame(df_annual_returns)

    names(df_annual_returns) <- gsub("\\.yearly", "", names(df_annual_returns))

    df_annual_returns$date <- as.Date(df_annual_returns$date,
                                      format = "%Y-%m-%d")
    df_annual_returns$year <- as.numeric(format(df_annual_returns$date, "%Y"))

    df_annual_returns <- df_annual_returns[, names(df_annual_returns) != "date"]

    year.col <- names(df_annual_returns)[length(df_annual_returns)]
    investment.cols <- names(df_annual_returns)[1:(length(df_annual_returns) - 1)]
    investment.cols <- investment.cols[investment.cols != ""]

    df_annual_returns <- df_annual_returns[, c(year.col, investment.cols)]

    df_annual_returns[, investment.cols] <- df_annual_returns[, investment.cols] * 100

    if (length(investment.cols) > 1) {

      df_annual_returns[, investment.cols] <- lapply(
        df_annual_returns[, investment.cols], formatC, digits = 2, format = "f")
      suppressWarnings(
        df_annual_returns[, investment.cols] <- lapply(
          df_annual_returns[, investment.cols], as.numeric)
      )

    } else if (length(investment.cols) == 1) {

      df_annual_returns[, investment.cols] <- formatC(
        df_annual_returns[, investment.cols], digits = 2, format = "f")
      suppressWarnings(
        df_annual_returns[, investment.cols] <- as.numeric(
          df_annual_returns[, investment.cols])
      )

    }

    df_annual_returns[is.na(df_annual_returns)] <- "-"
    df_annual_returns$year <- as.integer(df_annual_returns$year)

    ## Commented to see in which cases this occurs and then try to come up with
    ## different code without tidyselect and tidyr
    # ## Sometimes the same year appears twice because of different first dates
    # ## in the same year. Select the one that is filled (i.e. non "-")?
    # df_annual_returns <- df_annual_returns %>%
    #   dplyr::group_by(.data$year) %>%
    #   tidyr::fill(tidyselect::everything())
    #
    # ##
    # df_annual_returns <- df_annual_returns %>%
    #   dplyr::group_by(.data$year) %>%
    #   tidyr::fill(tidyselect::everything(), .direction = "up")

    df_annual_returns <- as.data.frame(unique(df_annual_returns))

    names(df_annual_returns)[names(df_annual_returns) == "year"] <- "Year"

    return(df_annual_returns)

  }

}

#' Get annualized returns
#'
#' @usage get_annualized_returns(path)
#' @param path A single character string. Directory of the data.
#'
#' @return A data frame containing formatted table with annualized returns
#' for all tickers.
#'
#' @export
get_annualized_returns <- function(path) {

  PortfolioTracker::get_user_names(path)

  returns_exists <- file.exists(file.path(path.returns, file.returns.annualized))

  if (returns_exists) {

    df_returns <- data.table::fread(file.path(path.returns, file.returns.annualized))

    df_returns <- PortfolioTracker::format_return_tables(path, df_returns)

  } else {

    col_names <- c("Ticker", "ISIN", "Name", "1Y", "3Y", "5Y", "10Y", "Max")
    df_returns <- as.data.frame(matrix(nrow = 0, ncol = length(col_names),
                                       dimnames = list(NULL, col_names)))

  }

  return(df_returns)

}
