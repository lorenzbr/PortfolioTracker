#' Write portfolio statistics
#'
#' @usage write_portfolio_stats(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
#' @import data.table
write_portfolio_stats <- function(path) {

  get_names(path)

  df.complete.portfolio <- get_complete_portfolio_panel(path)

  if ( !is.null(df.complete.portfolio) ) {

    df.complete.portfolio <- df.complete.portfolio[, c("date", "value", "purchase_value",
                                                       "sale_value", "dividend_value",
                                                       "currently_invested", "value_available")]

    ## Take sum by group "date" for value, purchase_value, sale_value and dividend_value
    df.portfolio <- data.table::setDT(df.complete.portfolio)[, lapply(.SD, sum,
                                                                      na.rm = TRUE),
                                                                      by = date]

    ## Remove all dates where currently invested is unequal value available
    ## This means that prices are not available for all current investments at time t
    df.portfolio <- df.portfolio[df.portfolio$currently_invested == df.portfolio$value_available, ]

    df.portfolio[is.na(df.portfolio)] <- 0

    ## Remove all periods with no portfolio value, no purchase and no sale value at the same time
    days.empty.portfolio <- df.portfolio$value == 0 & df.portfolio$purchase_value == 0 & df.portfolio$sale_value == 0
    df.portfolio <- df.portfolio[ !days.empty.portfolio, ]

    portfolio_value <- df.portfolio$value[df.portfolio$date == max(df.portfolio$date)]


    ## Select time period to compute portfolio stats
    nb_periods <- c(1, 3, 6, 12, 36, 60, 120, 1, 1)
    period_types <- c(rep("months", 7), "ytd", "max")

    df.stats <- data.frame(matrix(ncol = 9, nrow = 0,
                                  dimnames = list(NULL, c("time_period", "portfolio_value",
                                                          "amount_invested", "amount_sold",
                                                          "price_gains", "price_gains_relative",
                                                          "dividends", "irr", "ttwror")
                                                  )
                                  )
                           )

    for (i in 1:length(nb_periods) ) {

      df.selected <- get_df_with_selected_time_period(df = df.portfolio,
                                                 nb_period = nb_periods[i],
                                                 period_type = period_types[i])

      time_period <- paste(nb_periods[i], period_types[i])

      first.date.target <- Sys.Date() - months(nb_periods[i])
      first.date.actual <- min(df.selected$date)

      ## If difference is very large, data for this period not available
      if (difftime(first.date.actual, first.date.target) <= 30 ) {

        amount_invested <- sum(df.selected$purchase_value)
        amount_sold <- sum(df.selected$sale_value)

        is.first.day <- df.selected$date == min(df.selected$date)
        is.last.day <- df.selected$date == max(df.selected$date)

        invested.without.first.day <- sum(df.selected$purchase_value[2:nrow(df.selected)])
        sold.without.first.day <- sum(df.selected$sale_value[2:nrow(df.selected)])
        start.value <- df.selected$value[is.first.day] + invested.without.first.day
        price_gains <- df.selected$value[is.last.day] + sold.without.first.day - start.value

        price_gains_relative <- price_gains / start.value

        dividends <- sum(df.selected$dividend_value)

        irr <- PortfolioTracker::get_portfolio_irr(path, nb_period = nb_periods[i],
                                         period_type = period_types[i]) * 100

        ttwror <- PortfolioTracker::get_portfolio_ttwror(path, nb_period = nb_periods[i],
                                               period_type = period_types[i]) * 100

        df.temp <- data.frame(time_period, portfolio_value, amount_invested, amount_sold,
                              price_gains, price_gains_relative, dividends, irr, ttwror)

      } else {

        df.temp <- data.frame(time_period, portfolio_value = NA, amount_invested = NA,
                              amount_sold = NA, price_gains = NA, price_gains_relative = NA,
                              dividends = NA, irr = NA, ttwror = NA)

      }

      df.stats <- rbind(df.stats, df.temp)

    }

    data.table::fwrite(df.stats, file.path(path.data, file.stats))

  } else {

    # message("Complete portfolio panel not available!")

  }

}

#' Get total dividend payments
#'
#' @usage get_dividends_max(path, file.dividend.history = "dividends_fullhistory.csv")
#' @param path A single character string. Path where data are stored.
#' @param file.dividend.history A single character string. Name of csv containing full history of dividends.
#'
#' @export
get_dividends_max <- function(path, file.dividend.history = "dividends_fullhistory.csv") {

  get_names(path)

  if ( file.exists(file.path(path.dividends, file.dividend.history)) ) {

    ## Load dividend history
    df.dividend.history <- data.table::fread(file.path(path.dividends, file.dividend.history))

    ## compute max dividends
    dividends.max <- sum(df.dividend.history$transaction_value[df.dividend.history$transaction_type == "Dividend"])
    dividends.storno.max <- sum(df.dividend.history$transaction_value[df.dividend.history$transaction_type == "Storno - Dividend"])
    dividends.max <- dividends.max - dividends.storno.max

  } else {

    dividends.max <- 0

  }

  return(dividends.max)

}
