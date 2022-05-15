#' Write portfolio statistics
#'
#' @usage write_portfolio_stats(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
#' @import data.table
write_portfolio_stats <- function(path) {

  get_user_names(path)

  df_complete_portfolio <- get_complete_portfolio_panel(path)

  if (!is.null(df_complete_portfolio)) {

    col_names_portfolio <- c("date", "value", "purchase_value",
                             "sale_value", "dividend_value",
                             "currently_invested", "value_available")

    df_complete_portfolio <- df_complete_portfolio[, col_names_portfolio]

    ## Take sum by group "date" for value, purchase_value, sale_value and dividend_value
    ## data.table solution should be best  because these will be somewhat
    ## larger tables (at least several thousands of rows)
    df_portfolio <- data.table::setDT(
      df_complete_portfolio)[, lapply(.SD, sum, na.rm = TRUE), by = date]

    ## Remove all dates where currently_invested is unequal value_available
    ## This means that prices are not available for all current investments at time t
    df_portfolio <- df_portfolio[df_portfolio$currently_invested == df_portfolio$value_available, ]

    df_portfolio[is.na(df_portfolio)] <- 0

    ## Remove all periods with no portfolio value, no purchase and no sale
    ## value at the same time
    days_empty_portfolio <- df_portfolio$value == 0 &
      df_portfolio$purchase_value == 0 &
      df_portfolio$sale_value == 0
    df_portfolio <- df_portfolio[!days_empty_portfolio, ]

    portfolio_value <- df_portfolio$value[df_portfolio$date == max(df_portfolio$date)]

    ## Select time period to compute portfolio stats
    nb_periods <- c(1, 3, 6, 12, 36, 60, 120, 1, 1)
    period_types <- c(rep("months", 7), "ytd", "max")

    col_names_stats <- c("time_period", "portfolio_value",
                         "amount_invested", "amount_sold",
                         "price_gains", "price_gains_relative",
                         "dividends", "irr", "ttwror")

    df_stats <- data.frame(
      matrix(ncol = 9, nrow = 0, dimnames = list(NULL, col_names_stats)))

    for (i in 1:length(nb_periods)) {

      df_selected <- get_df_with_selected_time_period(df = df_portfolio,
                                                 nb_period = nb_periods[i],
                                                 period_type = period_types[i])

      time_period <- paste(nb_periods[i], period_types[i])

      ## Sometimes dates do not exist (e.g., Feb 29, Feb 30, Feb 31, April 31)
      first_date_target <- Sys.Date() - months(nb_periods[i])
      j <- 1
      while (is.na(first_date_target) && j < 5) {
        first_date_target <- (Sys.Date() - j) - months(nb_periods[i])
        j = j + 1
      }
      first_date_actual <- min(df_selected$date)

      ## If difference is very large, data for this period not available
      if (difftime(first_date_actual, first_date_target) <= 30) {

        amount_invested <- sum(df_selected$purchase_value)
        amount_sold <- sum(df_selected$sale_value)

        is_first_day <- df_selected$date == min(df_selected$date)
        is_last_day <- df_selected$date == max(df_selected$date)

        invested_without_first_day <- sum(df_selected$purchase_value[2:nrow(df_selected)])
        sold_without_first_day <- sum(df_selected$sale_value[2:nrow(df_selected)])
        start_value <- df_selected$value[is_first_day] +
          invested_without_first_day
        price_gains <- df_selected$value[is_last_day] +
          sold_without_first_day - start_value

        price_gains_relative <- price_gains / start_value

        dividends <- sum(df_selected$dividend_value)

        irr <- get_portfolio_irr(path, nb_period = nb_periods[i],
                                 period_type = period_types[i]) * 100

        ttwror <- get_portfolio_ttwror(path, nb_period = nb_periods[i],
                                       period_type = period_types[i]) * 100

        df_temp <- data.frame(
          time_period, portfolio_value, amount_invested,
          amount_sold, price_gains, price_gains_relative,
          dividends, irr, ttwror)

      } else {

        df_temp <- data.frame(
          time_period, portfolio_value = NA, amount_invested = NA,
          amount_sold = NA, price_gains = NA, price_gains_relative = NA,
          dividends = NA, irr = NA, ttwror = NA)

      }

      df_stats <- rbind(df_stats, df_temp)

    }

    data.table::fwrite(df_stats, file.path(path.data, file.stats))

  } else {

    ## Delete file with stats if no complete panel exists
    unlink(file.path(path.data, file.stats))

  }

}

#' Get total dividend payments
#'
#' @usage get_dividends_max(path, file.dividend.history = "dividends_fullhistory.csv")
#' @param path A single character string. Path where data are stored.
#' @param file.dividend.history A single character string. Name of csv
#' containing full history of dividends.
#'
#' @return A single value containing the overall dividends.
#'
#' @export
get_dividends_max <- function(path, file.dividend.history = "dividends_fullhistory.csv") {

  get_user_names(path)

  file_path_dividends <- file.path(path.dividends, file.dividend.history)

  if (file.exists(file_path_dividends)) {

    df_dividends <- data.table::fread(file_path_dividends)

    dividends_max <- sum(df_dividends$transaction_value[df_dividends$transaction_type == "Dividend"])
    dividends_storno_max <- sum(df_dividends$transaction_value[df_dividends$transaction_type == "Storno - Dividend"])
    dividends_max <- dividends_max - dividends_storno_max

  } else {

    dividends_max <- 0

  }

  return(dividends_max)

}
