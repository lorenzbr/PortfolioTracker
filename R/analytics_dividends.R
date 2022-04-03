#' Write history of dividends in a csv file
#'
#' @usage write_dividend_history(df_transactions, path)
#' @param df_transactions A data frame containing the history of transactions.
#' @param path A single character string. Path where data are stored.
#'
#' @export
write_dividend_history <- function(df_transactions, path) {

  get_user_names(path)

  df_dividends <- df_transactions[grepl("Dividend",
                                           df_transactions$transaction_type,
                                           fixed = TRUE), ]

  if (nrow(df_dividends) > 0) {

    ## Make sure values are positive - Why is there a need anyways? Explain!
    df_dividends$transaction_value <- abs(df_dividends$transaction_value)

    data.table::fwrite(df_dividends,
                       file.path(path.dividends, file.dividend.history))

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

    get_user_names(path)

    file_path_dividends <- file.path(path.dividends, file.dividend.history)

    if (file.exists(file_path_dividends)) {

      df_dividends <- data.table::fread(file_path_dividends)

      ## Storno needs to be a negative amount (i.e., payment)
      is_storno <- df_dividends$transaction_type == "Storno - Dividend"
      df_dividends$transaction_value[is_storno] <- -df_dividends$transaction_value[is_storno]

      df_dividends$transaction_date <- as.Date(df_dividends$transaction_date,
                                               format = "%d-%m-%Y")

      df_dividends$year <- lubridate::year(df_dividends$transaction_date)

      df_dividends_sum_yr <- stats::aggregate(transaction_value ~ year,
                                              data = df_dividends, sum)
      df_dividends_sum_yr <- df_dividends_sum_yr %>%
        dplyr::mutate(year = year) %>%
        tidyr::complete(year = seq(min(year), as.numeric(lubridate::year(Sys.Date())),
                                   by = 1))
      df_dividends_sum_yr <- as.data.frame(df_dividends_sum_yr)
      df_dividends_sum_yr$transaction_value[is.na(df_dividends_sum_yr$transaction_value)] <- 0

      data.table::fwrite(df_dividends_sum_yr,
                         file.path(path.dividends, file.dividend.year))

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

  get_user_names(path)

  tryCatch({

    file_path_dividends <- file.path(path.dividends, file.dividend.history)

    if (file.exists(file_path_dividends)) {

      df_dividends <- data.table::fread(file_path_dividends)

      ## Storno needs to be a negative amount (i.e., payment)
      is_storno <- df_dividends$transaction_type == "Storno - Dividend"
      df_dividends$transaction_value[is_storno] <- -df_dividends$transaction_value[is_storno]

      df_dividends$transaction_date <- as.Date(df_dividends$transaction_date,
                                               format = "%d-%m-%Y")

      df_dividends$yearmon <- lubridate::floor_date(df_dividends$transaction_date,
                                                    unit = "month")

      ## Get dividends by month
      df_dividends_sum_month <- stats::aggregate(transaction_value ~ yearmon,
                                                 data = df_dividends, sum)
      df_dividends_sum_month <- df_dividends_sum_month %>%
        dplyr::mutate(yearmon = as.Date(.data$yearmon)) %>%
        tidyr::complete(yearmon = seq.Date(min(.data$yearmon),
                                           lubridate::floor_date(Sys.Date(),
                                                                 unit = "month"),
                                           by = "month"))
      df_dividends_sum_month <- as.data.frame(df_dividends_sum_month)
      df_dividends_sum_month$transaction_value[is.na(df_dividends_sum_month$transaction_value)] <- 0

      data.table::fwrite(df_dividends_sum_month,
                         file.path(path.dividends, file.dividend.month))

    }

  },

  error = function(e) {

    message(e)

  })

}
