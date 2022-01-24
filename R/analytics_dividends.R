#' Write history of dividends in a csv file
#'
#' @usage write_dividend_history(df.transaction.history, path)
#' @param df.transaction.history data.frame containing history of transactions.
#' @param path A single character string. Path where data are stored.
#'
#' @export
write_dividend_history <- function(df.transaction.history, path) {

  get_names(path)

  ## get list of dividends
  df.dividend.history <- df.transaction.history[grepl("Dividend", df.transaction.history$transaction_type), ]

  if (nrow(df.dividend.history) > 0) {

    ## make sure values are positive
    df.dividend.history$transaction_value <- abs(df.dividend.history$transaction_value)

    ## write dividend history
    data.table::fwrite(df.dividend.history, file.path(path.dividends, file.dividend.history))

  } else {

    # message("No dividend transactions available.")

  }

}

#' Write dividend payments by year to a csv file
#'
#' @usage write_dividend_by_yr(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
#' @importFrom magrittr %>%
write_dividend_by_yr <- function(path) {

  tryCatch({

    get_names(path)

    if ( file.exists(file.path(path.dividends, file.dividend.history)) ) {

      ## load dividend history
      df.dividend.history <- data.table::fread(file.path(path.dividends, file.dividend.history))

      ## storno needs to be a negative amount (i.e. payment)
      is.storno <- df.dividend.history$transaction_type == "Storno - Dividend"
      df.dividend.history$transaction_value[is.storno] <- (-1) * df.dividend.history$transaction_value[is.storno]

      df.dividend.history$transaction_date <- as.Date(df.dividend.history$transaction_date, "%d-%m-%Y")

      ## get year
      df.dividend.history$year <- lubridate::year(df.dividend.history$transaction_date)

      ## get dividends by year
      df.dividend.history.sum.yr <- stats::aggregate(transaction_value ~ year, data = df.dividend.history, sum)
      df.dividend.history.sum.yr <- df.dividend.history.sum.yr %>%
        dplyr::mutate(year = year) %>%
        tidyr::complete(year = seq(min(year), as.numeric(lubridate::year(Sys.Date())), by = 1))
      df.dividend.history.sum.yr <- as.data.frame(df.dividend.history.sum.yr)
      df.dividend.history.sum.yr$transaction_value[is.na(df.dividend.history.sum.yr$transaction_value)] <- 0

      data.table::fwrite(df.dividend.history.sum.yr, file.path(path.dividends, file.dividend.year))

    }

  },

  error = function(e) {

    message(e)

  })

}

#' Write dividend payments by month to a csv file
#'
#' @usage write_dividend_by_month(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
write_dividend_by_month <- function(path) {

  get_names(path)

  tryCatch({

    if ( file.exists(file.path(path.dividends, file.dividend.history)) ) {

      ## load dividend history
      df.dividend.history <- data.table::fread(file.path(path.dividends, file.dividend.history))

      ## storno needs to be a negative amount (i.e., payment)
      is.storno <- df.dividend.history$transaction_type == "Storno - Dividend"
      df.dividend.history$transaction_value[is.storno] <- (-1) * df.dividend.history$transaction_value[is.storno]

      df.dividend.history$transaction_date <- as.Date(df.dividend.history$transaction_date, "%d-%m-%Y")

      ## get year-month
      df.dividend.history$yearmon <- lubridate::floor_date(df.dividend.history$transaction_date, unit = "month")

      ## get dividends by month
      df.dividend.history.sum.month <- stats::aggregate(transaction_value ~ yearmon, data = df.dividend.history, sum)
      df.dividend.history.sum.month <- df.dividend.history.sum.month %>%
        dplyr::mutate(yearmon = as.Date(.data$yearmon)) %>%
        tidyr::complete(yearmon = seq.Date(min(.data$yearmon), lubridate::floor_date(Sys.Date(), unit = "month"), by = "month"))
      df.dividend.history.sum.month <- as.data.frame(df.dividend.history.sum.month)
      df.dividend.history.sum.month$transaction_value[is.na(df.dividend.history.sum.month$transaction_value)] <- 0

      data.table::fwrite(df.dividend.history.sum.month, file.path(path.dividends, file.dividend.month))

    }

  },

  error = function(e) {

    message(e)

  })

}
