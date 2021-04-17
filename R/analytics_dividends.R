#' Write history of dividends in a csv file
#'
#' @usage write_dividend_history(df.transaction.history, path,
#'          file.dividend.history = "dividends_fullhistory.csv")
#' @param df.transaction.history data frame containing history of transactions.
#' @param path A single character string. Directory where all data are stored.
#' @param file.dividend.history file name of csv.
#'
#' @export
write_dividend_history <- function(df.transaction.history, path, file.dividend.history = "dividends_fullhistory.csv") {

  #### get dividend history

  ## create folder if not exists and get folder name for price panel
  list.paths <- PortfolioTracker::create_portfoliotracker_dir(path)
  path.dividends <- list.paths$path.dividends

  ## get list of dividends
  df.dividend.history <- df.transaction.history[grepl("Dividend", df.transaction.history$transaction_type), ]

  if (nrow(df.dividend.history) > 0) {

    ## make sure values are positive
    df.dividend.history$transaction_value <- abs(df.dividend.history$transaction_value)

    ## write dividend history
    data.table::fwrite(df.dividend.history, paste0(path.dividends, file.dividend.history))

  } else {

    message("No dividend transactions available.")

  } ## end of if else statement whether dividends exist

} ## end of function write_dividend_history


#' Write dividend payments by year to a csv file
#'
#' @usage write_dividend_by_yr(path, file.dividend.history = "dividends_fullhistory.csv")
#' @param path A single character string. Directory where all data are stored.
#' @param file.dividend.history A single character string. Name of csv containing full history of dividends.
#'
#' @export
#' @importFrom magrittr %>%
write_dividend_by_yr <- function(path, file.dividend.history = "dividends_fullhistory.csv") {

  #### get dividend by year from first dividend payment until today

  tryCatch({

    ## create folder if not exists and get folder name for price panel
    list.paths <- PortfolioTracker::create_portfoliotracker_dir(path)
    path.dividends <- list.paths$path.dividends

    ## load dividend history
    df.dividend.history <- data.table::fread(paste0(path.dividends, file.dividend.history))

    ## storno needs to be a negative amount (i.e. payment)
    df.dividend.history$transaction_value[df.dividend.history$transaction_type == "Storno - Dividend"] <- -df.dividend.history$transaction_value[df.dividend.history$transaction_type == "Storno - Dividend"]

    ## get year
    df.dividend.history$year <- lubridate::year(df.dividend.history$transaction_date)

    ## get dividends by year
    df.dividend.history.sum.yr <- stats::aggregate(transaction_value ~ year, data = df.dividend.history, sum)
    df.dividend.history.sum.yr <- df.dividend.history.sum.yr %>%
      dplyr::mutate(year = year) %>%
      tidyr::complete(year = seq(min(year), as.numeric(lubridate::year(Sys.Date())), by = 1))
    df.dividend.history.sum.yr <- as.data.frame(df.dividend.history.sum.yr)
    df.dividend.history.sum.yr$transaction_value[is.na(df.dividend.history.sum.yr$transaction_value)] <- 0

    file.dividend.yr <- "dividends_by_year.csv"

    data.table::fwrite(df.dividend.history.sum.yr, paste0(path.dividends, file.dividend.yr))

  },

  error = function(e) {

    message(e)

  }) ## end of tryCatch statement

} ## end of function write_dividend_by_yr

#' Write dividend payments by month to a csv file
#'
#' @usage write_dividend_by_month(path,
#'            file.dividend.history = "dividends_fullhistory.csv")
#' @param path A single character string. Directory where all data are stored.
#' @param file.dividend.history A single character string. Name of csv containing full history of dividends.
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
write_dividend_by_month <- function(path, file.dividend.history = "dividends_fullhistory.csv") {

  #### get dividend payments by month from first payment until today

  tryCatch({

    ## create folder if not exists and get folder name for price panel
    list.paths <- PortfolioTracker::create_portfoliotracker_dir(path)
    path.dividends <- list.paths$path.dividends

    ## load dividend history
    df.dividend.history <- data.table::fread(paste0(path.dividends, file.dividend.history))

    ## storno needs to be a negative amount (i.e., payment)
    df.dividend.history$transaction_value[df.dividend.history$transaction_type == "Storno - Dividend"] <- -df.dividend.history$transaction_value[df.dividend.history$transaction_type == "Storno - Dividend"]

    ## get year-month
    df.dividend.history$yearmon <- lubridate::floor_date(df.dividend.history$transaction_date, unit = "month")

    ## get dividends by month
    df.dividend.history.sum.month <- stats::aggregate(transaction_value ~ yearmon, data = df.dividend.history, sum)
    df.dividend.history.sum.month <- df.dividend.history.sum.month %>%
      dplyr::mutate(yearmon = as.Date(.data$yearmon)) %>%
      tidyr::complete(yearmon = seq.Date(min(.data$yearmon), lubridate::floor_date(Sys.Date(), unit = "month"), by = "month"))
    df.dividend.history.sum.month <- as.data.frame(df.dividend.history.sum.month)
    df.dividend.history.sum.month$transaction_value[is.na(df.dividend.history.sum.month$transaction_value)] <- 0

    file.dividend.month <- "dividends_by_month.csv"

    data.table::fwrite(df.dividend.history.sum.month, paste0(path.dividends, file.dividend.month))

  },

  error = function(e) {

    message(e)

  }) ## end of tryCatch statement

} ## end of function write_dividend_by_month
