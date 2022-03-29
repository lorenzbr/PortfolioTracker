#' Write IRRs for all tickers to csv files
#'
#' @usage write_investment_irr_all(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
write_investment_irr_all <- function(path) {

  #### TO DO: use apply functions rather than two for loops
  ####        or simply do everything in one data frame and use group by

  cash_flow <- NULL

  get_user_names(path)

  files.complete.panels <- list.files(path.complete.panel)

  if (length(files.complete.panels) > 0) {

    files <- file.path(path.complete.panel, files.complete.panels)
    list.dfs <- lapply(files, data.table::fread)

    panel.name <- "complete_panel"

    col.names <- c("ticker", "time_period", "irr")
    df.irr <- data.frame(matrix(ncol = length(col.names), nrow = 0,
                                dimnames = list(NULL, col.names)))

    ## Could also do mapply and do.call (if necessary)
    ## For loop over all investments
    for (i in 1:length(list.dfs)) {

      ticker <- stringr::str_match(files[i], paste0(panel.name, "_(.*?)_from"))[, 2]
      df.panel <- list.dfs[[i]]

      ## Select time period to compute IRR
      nb_periods <- c(1, 3, 6, 12, 36, 60, 120, 1, 1)
      period_types <- c(rep("months", 7), "ytd", "max")

      for (t in 1:length(nb_periods)) {

        df.selected <- get_df_with_selected_time_period(df = df.panel,
                                                        nb_period = nb_periods[t],
                                                        period_type = period_types[t])

        if (nrow(df.selected) > 0) {

          time_period <- paste(nb_periods[t], period_types[t])

          first.date.target <- Sys.Date() - months(nb_periods[t])
          first.date.actual <- min(df.selected$date)


          ## If difference is very large, data for this period not available
          if (difftime(first.date.actual, first.date.target) <= 30) {

            ## Remove all periods with no portfolio value, no purchase and no sale value at the same time
            days.empty.portfolio <- df.selected$value == 0 &
              df.selected$purchase_value == 0 &
              df.selected$sale_value == 0
            df.selected <- df.selected[!days.empty.portfolio, ]

            ## In the first period, the investment would be hypothetically purchased
            ## If something was indeed purchased, this would appear in the "value" column. So no need to
            ## consider it twice
            is.first.day <- df.selected$date == min(df.selected$date)
            df.selected$purchase_value[is.first.day] <- df.selected$value[is.first.day]

            ## In the last period, the investment would be hypothetically sold
            ## If something was indeed sold on the last day, this would NOT appear in the "value" column.
            ## So need to consider this as well.
            is.last.day <- df.selected$date == max(df.selected$date)
            df.selected$sale_value[is.last.day] <- df.selected$sale_value[is.last.day] +
              df.selected$value[is.last.day]

            df.selected$cash_flow <- df.selected$sale_value + df.selected$dividend_value -
              df.selected$purchase_value

            ## Aggregate cash flow on month level
            ## If it gets too slow, compute IRR on year level
            ## For now month level should be fine
            df.selected.by.month <- data.table::setDT(df.selected)[, list(cash_flow = sum(cash_flow)),
                                                                     by = list(yr = lubridate::year(date),
                                                                               mon = months(date))]

            ## Computation of IRR on monthly basis
            irr <- jrvFinance::irr(df.selected.by.month$cash_flow, cf.freq = 12,
                                   comp.freq = Inf)

            df.temp <- data.frame(ticker = ticker, time_period, irr)

          } else {

            df.temp <- data.frame(ticker = ticker, time_period, irr = NA)

          }

          df.irr <- rbind(df.irr, df.temp)

        } ## If statement: if selected time period is non-empty

      } ## For loop over all periods

    } ## For loop over all investments

    ## Long to wide
    data.table::setDT(df.irr)
    df.irr <- data.table::dcast(df.irr, ticker ~ time_period, value.var = "irr")

    data.table::setDF(df.irr)

    names(df.irr)[names(df.irr) == "1 max"] <- "max"
    names(df.irr)[names(df.irr) == "1 ytd"] <- "ytd"
    names(df.irr)[names(df.irr) == "1 months"] <- "1m"
    names(df.irr)[names(df.irr) == "3 months"] <- "3m"
    names(df.irr)[names(df.irr) == "6 months"] <- "6m"
    names(df.irr)[names(df.irr) == "12 months"] <- "1y"
    names(df.irr)[names(df.irr) == "36 months"] <- "3y"
    names(df.irr)[names(df.irr) == "60 months"] <- "5y"
    names(df.irr)[names(df.irr) == "120 months"] <- "10y"

    data.table::fwrite(df.irr, file.path(path.returns, file.returns.irr))

  }

}

#' Write returns for all tickers to csv files
#'
#' @usage write_returns(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
write_returns <- function(path) {

  get_user_names(path)

  files.price.panels <- list.files(path.price.panel)

  if (length(files.price.panels) > 0) {

    files <- file.path(path.price.panel, files.price.panels)
    list.dfs <- lapply(files, data.table::fread)

    # last.year <- lubridate::year(Sys.Date()) - 1
    # df.annual <- data.frame(year = last.year)
    df.daily <- data.frame(matrix(nrow = 0, ncol = 1, dimnames = list(NULL, "date")))
    df.monthly <- data.frame(matrix(nrow = 0, ncol = 1, dimnames = list(NULL, "date")))
    df.annual <- data.frame(matrix(nrow = 0, ncol = 1, dimnames = list(NULL, "date")))

    ## Could also do mapply and do.call (if necessary)
    for (i in 1:length(list.dfs)) {

      ticker <- stringr::str_match(files[i], "price_panel_(.*?)_from")[, 2]
      df.price.panel <- list.dfs[[i]]

      df.temp <- get_returns_all(df.price.panel, ticker)

      ticker.daily <- paste0(ticker, ".daily")
      ticker.monthly <- paste0(ticker, ".monthly")
      ticker.yearly <- paste0(ticker, ".yearly")

      df.daily.temp <- df.temp[!is.na(df.temp[, ticker.daily]), c("date", ticker.daily)]
      df.monthly.temp <- df.temp[!is.na(df.temp[, ticker.monthly]), c("date", ticker.monthly)]
      df.yearly.temp <- df.temp[!is.na(df.temp[, ticker.yearly]), c("date", ticker.yearly)]

      df.yearly.temp$date <- lubridate::floor_date(df.yearly.temp$date, unit = "year")

      df.daily <- merge(df.daily, df.daily.temp,
                        by = "date",
                        all.x = TRUE, all.y = TRUE)
      df.monthly <- merge(df.monthly, df.monthly.temp,
                          by = "date",
                          all.x = TRUE, all.y = TRUE)
      df.annual <- merge(df.annual, df.yearly.temp,
                         by = "date",
                         all.x = TRUE, all.y = TRUE)

    }

    data.table::fwrite(df.daily, file.path(path.returns, file.returns.daily))
    data.table::fwrite(df.monthly, file.path(path.returns, file.returns.monthly))
    data.table::fwrite(df.annual, file.path(path.returns, file.returns.annual))

  }

}

#' Get all returns (daily, monthly, annual) for ticker
#'
#' @usage get_returns_all(df, ticker)
#' @param df A data frame containing date and prices.
#' @param ticker A single character string containing the ticker.
#' @return A data frame containing daily, monthly and annual returns.
#'
#' @export
get_returns_all <- function(df, ticker) {

  df$date <- as.Date(df$date, format = "%Y-%m-%d")

  df <- df[, c("date", "adjusted")]
  xts.prices <- xts::as.xts(df)

  xts.monthlyreturns <- quantmod::monthlyReturn(xts.prices)
  xts.annualreturns <- quantmod::annualReturn(xts.prices)

  xts.allreturns <- quantmod::allReturns(xts.prices)

  df.allreturns <- as.data.frame(xts.allreturns)

  names(df.allreturns) <- paste0(ticker, ".", names(df.allreturns))

  df.allreturns$date <- rownames(df.allreturns)

  rownames(df.allreturns) <- 1:nrow(df.allreturns)

  df.allreturns$date <- as.Date(df.allreturns$date, format = "%Y-%m-%d")

  return(df.allreturns)

}

#' Write annualized returns to a csv file
#'
#' @usage write_annualized_returns(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
write_annualized_returns <- function(path) {

  get_user_names(path)

  returns.period <- "daily"
  returns.period.file <- "daily_returns"

  file.name <- list.files(path.returns, pattern = returns.period.file)

  df.returns <- data.table::fread(file.path(path.returns, file.name))

  names(df.returns) <- gsub(paste0("\\.", returns.period), "", names(df.returns))

  df.returns$date <- as.Date(df.returns$date, format = "%Y-%m-%d")

  xts.returns.max <- xts::as.xts(df.returns)

  df.annualized.years <- data.frame(matrix(nrow = length(names(xts.returns.max)),
                                           ncol = 0,
                                     dimnames = list(names(xts.returns.max), NULL)))

  ## Annualized returns for 1, 3, 5 and 10 years
  annualize.return.periods <- c(1, 3, 5, 10)

  for (annualize.return.period in annualize.return.periods) {

    ## Select period
    xts.returns.Xy <- xts.returns.max[paste0(Sys.Date() - lubridate::years(annualize.return.period), "/")]

    ## Compute annualized return if prices exist for X years

    df.temp <- get_annualized_returns(xts.returns.Xy)

    names(df.temp) <- paste0(annualize.return.period, "y")

    df.annualized.years <- cbind(df.annualized.years, df.temp)

  }

  df.annualized.max <- get_annualized_returns(xts.returns.max)
  names(df.annualized.max) <- "max"
  df.annualized <- cbind(df.annualized.years, df.annualized.max)

  col.names <- names(df.annualized)
  df.annualized$ticker <- rownames(df.annualized)
  df.annualized <- df.annualized[, c("ticker", col.names)]


  transaction.history.exists <- file.exists(file.path(path.transactions,
                                                      file.transactions))
  isin.ticker.exists <- file.exists(file.path(path.tickers, file.tickers))

  if (transaction.history.exists && isin.ticker.exists) {

    df.transaction.history <- data.table::fread(file.path(path.transactions,
                                                          file.transactions))
    df.isin.ticker <- data.table::fread(file.path(path.tickers, file.tickers))

    df.transaction.history <- merge(df.transaction.history, df.isin.ticker, by = "isin")

    df.ticker.date <- df.transaction.history[df.transaction.history$transaction_type == "Purchase",
                                                        c("ticker", "transaction_date")]
    names(df.ticker.date)[names(df.ticker.date) == "transaction_date"] <- "date"
    df.ticker.date$date <- as.Date(df.ticker.date$date, format = "%d-%m-%Y")
    df.ticker.date <- df.ticker.date %>%
      dplyr::group_by(.data$ticker) %>%
      dplyr::filter(date == min(.data$date))
    df.ticker.date <- df.ticker.date %>%
      dplyr::group_by(.data$ticker) %>%
      dplyr::sample_n(size = 1)
    df.ticker.date$age_yrs <- floor(lubridate::time_length(difftime(Sys.Date(),
                                                                    df.ticker.date$date),
                                                           "years"))

    ## make return NA or "-" if column year greater than age_yrs
    df.annualized <- merge(df.annualized, df.ticker.date, by = "ticker")
    for (i in 1:nrow(df.annualized)) {
      incomplete.positions <- which(df.annualized$age_yrs[i] < annualize.return.periods) + 1
      df.annualized[i, incomplete.positions] <- NA
    }

    df.annualized <- df.annualized[, names(df.annualized) != "age_yrs"
                                   & names(df.annualized) != "date"]

    data.table::fwrite(df.annualized, file.path(path.returns, file.returns.annualized))

  }

}

#' Write portfolio return to a csv file
#'
#' @usage write_portfolio_return(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
write_portfolio_return <- function(path) {

  get_user_names(path)

  # df.transaction.history <- data.table::fread(file.path(path.transactions, file.transactions))
  # df.isin.ticker <- data.table::fread(file.path(path.tickers, file.tickers))
  #
  # df.transaction.history <- merge(df.transaction.history, df.isin.ticker, by = "isin")

  returns.period <- "daily"
  returns.period.file <- "daily_returns"

  file.name <- list.files(path.returns, pattern = returns.period.file)

  df.returns <- data.table::fread(file.path(path.returns, file.name))

  names(df.returns) <- gsub(paste0("\\.", returns.period), "", names(df.returns))

  df.returns$date <- as.Date(df.returns$date, format = "%Y-%m-%d")

  df.returns[is.na(df.returns)] <- 0

  xts.returns.max <- xts::as.xts(df.returns)


  files.pricequantity.panels <- list.files(path.pricequantity.panel)

  no.pricequantity.panels <- length(files.pricequantity.panels) == 0

  if (!no.pricequantity.panels) {

    files <- file.path(path.pricequantity.panel, files.pricequantity.panels)
    list.dfs <- lapply(files, data.table::fread)

  }

  df.weight.final <- data.frame(matrix(nrow = 0, ncol = 1,
                                       dimnames = list(NULL, "date")))

  for (i in 1:length(list.dfs)) {

    ticker <- stringr::str_match(files[i], "pricequantity_panel_(.*?)_from")[, 2]
    df.pricequantity.panel <- list.dfs[[i]]

    df.weight.panel <- df.pricequantity.panel[, c("date", "value")]
    names(df.weight.panel)[2] <- ticker

    df.weight.final <- merge(df.weight.final,
                             df.weight.panel,
                             by = "date",
                             all.x = TRUE, all.y = TRUE)

  }

  df.weight.final[is.na(df.weight.final)] <- 0
  df.weight.final$sum <- rowSums(df.weight.final[-1])
  df.weight.final <- df.weight.final[df.weight.final$sum > 0, ]
  df.weight.final[, 2:(length(df.weight.final) - 1)] <- df.weight.final[, 2:(length(df.weight.final) - 1)] / df.weight.final$sum
  df.weight.final <- df.weight.final[, names(df.weight.final) != "sum"]
  row.names(df.weight.final) <- df.weight.final$date
  # df.weight.final <- df.weight.final[, names(df.weight.final) != "date"]
  xts.weight <- xts::as.xts(df.weight.final)
  xts.weight <- xts.weight[, names(xts.weight) != "date"]
  storage.mode(xts.weight) <- "numeric"

  ## Get daily portfolio returns
  ## Super slow: find other solution for this: Use slim version of this function
  xts.portfolio <- PerformanceAnalytics::Return.portfolio(xts.returns.max, xts.weight)

  df.portfolio.daily <- data.frame(xts.portfolio)
  df.portfolio.daily$date <- row.names(df.portfolio.daily)
  df.portfolio.daily <- df.portfolio.daily[, c("date", "portfolio.returns")]
  names(df.portfolio.daily)[2] <- "portfolio_returns"
  row.names(df.portfolio.daily) <- 1:nrow(df.portfolio.daily)

  data.table::fwrite(df.portfolio.daily, file.path(path.returns, file.return.portfolio.daily))

}

#' Write cumulative daily investment returns for all tickers to a csv file
#'
#' @usage write_roi_by_period_all(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
write_roi_by_period_all <- function(path) {

  get_user_names(path)

  df.transaction.history <- data.table::fread(file.path(path.transactions,
                                                        file.transactions))

  tickers <- get_tickers_from_transactions(df.transaction.history, path)

  ## Write cumulative daily investment return for all tickers
  output <- mapply(write_roi_by_period, tickers, MoreArgs = list(path))

}

#' Write cumulative daily investment returns to a csv file
#'
#' @usage write_roi_by_period(ticker, path)
#' @param ticker A single character string containing the ticker.
#' @param path A single character string. Path where data are stored.
#'
#' @export
write_roi_by_period <- function(ticker, path) {

  get_user_names(path)

  if (length(list.files(path.complete.panel, pattern = ticker)) > 0) {

    df.complete.panel <- data.table::fread(file.path(path.complete.panel,
                                              list.files(path.complete.panel,
                                                         pattern = ticker)))

    df.roi <- get_roi_by_period(df.complete.panel, nb_period = NULL,
                                period_type = "max")

    file.roi.panel <- paste0("return_on_investment_daily_", ticker,
                             "_from_", min(df.roi$date),
                             "_to_", max(df.roi$date),
                             ".csv")

    data.table::fwrite(df.roi, file.path(path.returns.roi, file.roi.panel))

  }

}

#' Get cumulative return on investment for specific period
#'
#' @usage get_roi_by_period(df.complete.panel, nb_period = NULL, period_type = "max")
#' @param df.complete.panel A data.frame containing the complete panel.
#' @param nb_period An integer indicating the number of months. Default is \emph{NULL}.
#' @param period_type A single character string. Default \emph{max}. Possible
#' values \emph{max}, \emph{weeks} and \emph{months}.
#'
#' @return A data frame containing the cumulative return on investment for a given period.
#'
#' @export
get_roi_by_period <- function(df.complete.panel, nb_period = NULL, period_type = "max") {

  df.complete.panel.period <- get_df_with_selected_time_period(df = df.complete.panel,
                                                               nb_period = nb_period,
                                                               period_type = period_type)

  if (nrow(df.complete.panel.period) > 0) {


    if (period_type == "months" || period_type == "weeks" || period_type == "days") {

      index.first.period <- df.complete.panel.period$date == min(df.complete.panel.period$date)

      df.complete.panel.period <- df.complete.panel.period[order(df.complete.panel.period$date), ]

      ## Re-compute purchase cum value and first entry is simply equal to value
      df.complete.panel.period$purchase_value[index.first.period] <- df.complete.panel.period$value[index.first.period]
      df.complete.panel.period$purchase_cum_value <- cumsum(df.complete.panel.period$purchase_value)

      ## Re-compute dividend and sale cum value
      df.complete.panel.period$sale_cum_value <- cumsum(df.complete.panel.period$sale_value)
      df.complete.panel.period$dividend_cum_value <- cumsum(df.complete.panel.period$dividend_value)

    }


    df.complete.panel.period[is.na(df.complete.panel.period)] <- 0
    df.complete.panel.period <- df.complete.panel.period[df.complete.panel.period$cum_quantity != 0
                                                         | df.complete.panel.period$sale_value != 0
                                                         | df.complete.panel.period$dividend_value != 0, ]

    df.complete.panel.period$daily_cum_roi <- (df.complete.panel.period$value +
                                                 df.complete.panel.period$sale_cum_value +
                                                 df.complete.panel.period$dividend_cum_value) / df.complete.panel.period$purchase_cum_value

    df.roi.period <- df.complete.panel.period[, c("date", "daily_cum_roi")]

    return(df.roi.period)

  }

}

#' Write TWR factors on daily basis for portfolio
#'
#' Write the true-weighted return factors on a daily basis for the entire portfolio
#'
#' @usage write_portfolio_twr_factors(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
write_portfolio_twr_factors <- function(path) {

  get_user_names(path)

  df.all <- get_complete_portfolio_panel(path)

  if (!is.null(df.all)) {

    df.all <- df.all[, c("date", "value", "purchase_value", "sale_value",
                         "dividend_value", "currently_invested", "value_available")]

    ## Take sum by group "date" for value, purchase_value, sale_value and dividend_cum_value
    df.all <- data.table::setDT(df.all)[, lapply(.SD, sum, na.rm = TRUE), by = date]

    ## Re-compute cumulative dividend payments
    df.all <- df.all[order(df.all$date), ]
    df.all$dividend_cum_value <- cumsum(df.all$dividend_value)

    ## Remove all dates where currently invested is unequal value available
    ## this means that prices are not available for all current investments at time t
    df.all <- df.all[df.all$currently_invested == df.all$value_available, ]

    df.twr <- df.all

    df.twr[is.na(df.twr)] <- 0

    ## Remove all periods with no portfolio value, no purchase and no sale value at the same time
    days.empty.portfolio <- df.twr$value == 0 & df.twr$purchase_value == 0 & df.twr$sale_value == 0
    df.twr <- df.twr[!days.empty.portfolio, ]


    df.twr$end_value <- df.twr$value + df.twr$dividend_cum_value

    df.twr$initial_value <- data.table::shift(df.twr$value) + data.table::shift(df.twr$dividend_cum_value)

    df.twr$cash_flow <- df.twr$purchase_value - df.twr$sale_value

    ## Compute daily holding period return
    ## Cash flows occur just before the valuation (of end_value)
    df.twr$twr_factor <- (df.twr$end_value - df.twr$cash_flow) / df.twr$initial_value
    # df.twr$twr_factor <- df.twr$end_value / (df.twr$initial_value + df.twr$cash_flow)

    data.table::fwrite(df.twr, file.path(path.returns, file.returns.twr.daily))

  }

}

#' Get true time-weighted rate of return (TTWROR) of total portfolio
#'
#' @usage get_portfolio_ttwror(path, nb_period = NULL, period_type = "max")
#' @param path A single character string. Path where data are stored.
#' @param nb_period An integer indicating the number of months. Default is \emph{NULL}.
#' @param period_type A single character string. Default \emph{max}. Possible values \emph{max}, \emph{weeks} and \emph{months}.
#'
#' @return A numeric for the true time-weighted rate of return (TTWROR) of your portfolio
#'
#' @export
get_portfolio_ttwror <- function(path, nb_period = NULL, period_type = "max") {

  get_user_names(path)

  if (file.exists(file.path(path.returns, file.returns.twr.daily))) {

    df.twr.factors <- data.table::fread(file.path(path.returns, file.returns.twr.daily))

    ## Select time period with TWR factors
    df.selected.period <- get_df_with_selected_time_period(df = df.twr.factors,
                                                           nb_period = nb_period,
                                                           period_type = period_type)

    ## Remove focal periods succeeding periods where value is zero (because for the subsequent focal period
    ## has no reasonable initial value)
    df.selected.period <- df.selected.period[data.table::shift(df.selected.period$value) != 0, ]

    periods <- length(df.selected.period$twr_factor[!is.na(df.selected.period$twr_factor)])

    ## Multiply all TWR factors to get ttwror
    ## Total period
    # ttwror <- prod(df.selected.period$twr_factor, na.rm = TRUE) - 1
    ## Annualized (meaning over a period of 365 trading days)
    annualized.ttwror <- prod(df.selected.period$twr_factor, na.rm = TRUE)^(365.25 / periods) - 1

  } else {

    annualized.ttwror <- NA

  }

  return(annualized.ttwror)

}


#' Get internal rate of return (IRR) of total portfolio
#'
#' @usage get_portfolio_irr(path, nb_period = NULL, period_type = "max")
#' @param path A single character string. Path where data are stored.
#' @param nb_period An integer indicating the number of months. Default is \emph{NULL}.
#' @param period_type A single character string. Default \emph{max}. Possible values \emph{max}, \emph{weeks} and \emph{months}.
#'
#' @return A numeric for the internal rate of return (IRR) of your portfolio
#'
#' @export
#' @import data.table
get_portfolio_irr <- function(path, nb_period = NULL, period_type = "max") {

  cash_flow <- NULL

  get_user_names(path)

  df.complete.portfolio <- get_complete_portfolio_panel(path)


  if (!is.null(df.complete.portfolio)) {

    df.complete.portfolio <- df.complete.portfolio[, c("date", "value", "purchase_value",
                                                       "sale_value", "dividend_value",
                                                       "currently_invested", "value_available")]

    ## Take sum by group "date" for value, purchase_value, sale_value and dividend_value
    ## Group date means: the values for all individual investments are aggregated (summed)
    df.portfolio <- data.table::setDT(df.complete.portfolio)[, lapply(.SD, sum, na.rm = TRUE),
                                                                      by = date]

    ## Select time period for portfolio panel
    df.portfolio <- get_df_with_selected_time_period(df = df.portfolio,
                                                           nb_period = nb_period,
                                                           period_type = period_type)

    ## Remove all dates where currently invested is unequal value available
    ## this means that prices are not available for all current investments at time t
    df.portfolio <- df.portfolio[df.portfolio$currently_invested == df.portfolio$value_available, ]

    df.portfolio[is.na(df.portfolio)] <- 0

    ## Remove all periods with no portfolio value, no purchase and no sale value at the same time
    days.empty.portfolio <- df.portfolio$value == 0 &
      df.portfolio$purchase_value == 0 &
      df.portfolio$sale_value == 0
    df.portfolio <- df.portfolio[!days.empty.portfolio, ]

    ## In the first period, the investment would be hypothetically purchased
    ## If something was indeed purchased, this would appear in the "value" column. So no need to
    ## consider it twice
    is.first.day <- df.portfolio$date == min(df.portfolio$date)
    df.portfolio$purchase_value[is.first.day] <- df.portfolio$value[is.first.day]

    ## In the last period, the investment would be hypothetically sold
    ## If something was indeed sold on the last day, this would NOT appear in the "value" column.
    ## So need to consider this as well.
    is.last.day <- df.portfolio$date == max(df.portfolio$date)
    df.portfolio$sale_value[is.last.day] <- df.portfolio$sale_value[is.last.day] +
      df.portfolio$value[is.last.day]

    ## Compute cash flow
    df.portfolio$cash_flow <- df.portfolio$sale_value +
      df.portfolio$dividend_value -
      df.portfolio$purchase_value

    ## Aggregate cash flow on month level
    ## If it gets too slow, compute IRR on year level
    ## For now month level should be fine
    df.portfolio.by.month <- data.table::setDT(df.portfolio)[, list(cash_flow = sum(cash_flow)),
                              by = list(yr = lubridate::year(date),
                                        mon = months(date))]


    ## Computation of IRR on monthly basis
    irr_final <- jrvFinance::irr(df.portfolio.by.month$cash_flow, cf.freq = 12,
                                 comp.freq = Inf)

  } else {

    irr_final <- NA

  }

  return(irr_final)

}
