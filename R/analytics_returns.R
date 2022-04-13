#' Write IRRs for all tickers to csv files
#'
#' @usage write_investment_irr_all(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
write_investment_irr_all <- function(path) {

  #### TO DO: use apply functions rather than two for loops
  ####        or simply do everything in one data frame and use group by
  ## Group by should be the fastest! <- Do this!

  cash_flow <- NULL

  get_user_names(path)

  files_complete_panels <- list.files(path.complete.panel)

  if (length(files_complete_panels) > 0) {

    files <- file.path(path.complete.panel, files_complete_panels)
    list_dfs <- lapply(files, data.table::fread)

    df_irr <- data.frame(
      matrix(ncol = 3, nrow = 0,
             dimnames = list(NULL,
                             c("ticker", "time_period", "irr"))))

    ## Could also do mapply and do.call (if necessary)
    ## For loop over all investments
    for (i in 1:length(list_dfs)) {

      ticker <- stringr::str_match(
        files[i], "complete_panel_(.*?)_from")[, 2]
      df_panel <- list_dfs[[i]]

      ## Select time period to compute IRR
      nb_periods <- c(1, 3, 6, 12, 36, 60, 120, 1, 1)
      period_types <- c(rep("months", 7), "ytd", "max")

      for (t in 1:length(nb_periods)) {

        df_selected <- get_df_with_selected_time_period(
          df = df_panel, nb_period = nb_periods[t],
          period_type = period_types[t])

        if (nrow(df_selected) > 0) {

          time_period <- paste(nb_periods[t], period_types[t])

          ## Sometimes dates do not exist (e.g., Feb 29, Feb 30,
          ## Feb 31, April 31)
          first_date_target <- Sys.Date() - months(nb_periods[t])
          j <- 1
          while (is.na(first_date_target) && j < 5) {
            first_date_target <- (Sys.Date() - j) - months(nb_periods[i])
            j = j + 1
          }
          first_date_actual <- min(df_selected$date)

          ## If difference is very large, data for this period not available
          if (difftime(first_date_actual, first_date_target) <= 30) {

            ## Remove all periods with no portfolio value,
            ## no purchase and no sale value at the same time
            days_empty_portfolio <- df_selected$value == 0 &
              df_selected$purchase_value == 0 &
              df_selected$sale_value == 0
            df_selected <- df_selected[!days_empty_portfolio, ]

            ## In the first period, the investment would be hypothetically purchased
            ## If something was indeed purchased, this would appear in the
            ## "value" column. So no need to consider it twice
            is_first_day <- df_selected$date == min(df_selected$date)
            df_selected$purchase_value[is_first_day] <- df_selected$value[is_first_day]

            ## In the last period, the investment would be hypothetically sold
            ## If something was indeed sold on the last day, this would NOT
            ## appear in the "value" column. So need to consider this as well.
            is_last_day <- df_selected$date == max(df_selected$date)
            df_selected$sale_value[is_last_day] <- df_selected$sale_value[is_last_day] +
              df_selected$value[is_last_day]

            df_selected$cash_flow <- df_selected$sale_value +
              df_selected$dividend_value -
              df_selected$purchase_value

            ## Aggregate cash flow on month level
            ## If it gets too slow, compute IRR on year level
            ## For now month level should be fine
            df_selected_by_month <- data.table::setDT(
              df_selected)[, list(cash_flow = sum(cash_flow)),
                           by = list(yr = lubridate::year(date),
                                     mon = months(date))]

            ## Computation of IRR on monthly basis
            irr <- jrvFinance::irr(df_selected_by_month$cash_flow,
                                   cf.freq = 12, comp.freq = Inf)

            df_temp <- data.frame(ticker = ticker, time_period, irr)

          } else {

            df_temp <- data.frame(ticker = ticker, time_period, irr = NA)

          }

          df_irr <- rbind(df_irr, df_temp)

        }

      }

    }

    ## Long to wide
    data.table::setDT(df_irr)
    df_irr <- data.table::dcast(df_irr, ticker ~ time_period,
                                value.var = "irr")

    data.table::setDF(df_irr)

    names(df_irr)[names(df_irr) == "1 max"] <- "max"
    names(df_irr)[names(df_irr) == "1 ytd"] <- "ytd"
    names(df_irr)[names(df_irr) == "1 months"] <- "1m"
    names(df_irr)[names(df_irr) == "3 months"] <- "3m"
    names(df_irr)[names(df_irr) == "6 months"] <- "6m"
    names(df_irr)[names(df_irr) == "12 months"] <- "1y"
    names(df_irr)[names(df_irr) == "36 months"] <- "3y"
    names(df_irr)[names(df_irr) == "60 months"] <- "5y"
    names(df_irr)[names(df_irr) == "120 months"] <- "10y"

    data.table::fwrite(df_irr, file.path(path.returns, file.returns.irr))

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

  files_pricequantity_panels <- list.files(path.pricequantity.panel)

  if (length(files_pricequantity_panels) > 0) {

    files <- file.path(path.pricequantity.panel, files_pricequantity_panels)
    list_dfs <- lapply(files, data.table::fread)

    # last_year <- lubridate::year(Sys.Date()) - 1
    # df_annual <- data.frame(year = last.year)
    df_daily <- data.frame(matrix(nrow = 0, ncol = 1,
                                  dimnames = list(NULL, "date")))
    df_monthly <- data.frame(matrix(nrow = 0, ncol = 1,
                                    dimnames = list(NULL, "date")))
    df_annual <- data.frame(matrix(nrow = 0, ncol = 1,
                                   dimnames = list(NULL, "date")))

    ## Could also do mapply and do.call (if necessary)
    for (i in 1:length(list_dfs)) {

      ticker <- stringr::str_match(files[i],
                                   "pricequantity_panel_(.*?)_from")[, 2]
      df_pricequantity_panel <- list_dfs[[i]]

      df_temp <- get_returns_all(df_pricequantity_panel, ticker)

      ticker_daily <- paste0(ticker, ".daily")
      ticker_monthly <- paste0(ticker, ".monthly")
      ticker_yearly <- paste0(ticker, ".yearly")

      df_daily_temp <- df_temp[!is.na(df_temp[, ticker_daily]),
                               c("date", ticker_daily)]
      df_monthly_temp <- df_temp[!is.na(df_temp[, ticker_monthly]),
                                 c("date", ticker_monthly)]
      df_yearly_temp <- df_temp[!is.na(df_temp[, ticker_yearly]),
                                c("date", ticker_yearly)]

      df_yearly_temp$date <- lubridate::floor_date(
        df_yearly_temp$date, unit = "year")

      df_daily <- merge(df_daily, df_daily_temp,
                        by = "date",
                        all.x = TRUE, all.y = TRUE)
      df_monthly <- merge(df_monthly, df_monthly_temp,
                          by = "date",
                          all.x = TRUE, all.y = TRUE)
      df_annual <- merge(df_annual, df_yearly_temp,
                         by = "date",
                         all.x = TRUE, all.y = TRUE)

    }

    data.table::fwrite(df_daily, file.path(path.returns, file.returns.daily))
    data.table::fwrite(df_monthly, file.path(path.returns, file.returns.monthly))
    data.table::fwrite(df_annual, file.path(path.returns, file.returns.annual))

  }

}

#' Get all returns (daily, monthly, annual) for ticker
#'
#' @usage get_returns_all(df, ticker)
#' @param df A data frame containing \emph{date} and prices with name \emph{adjusted}.
#' @param ticker A single character string containing the ticker.
#'
#' @return A data frame containing daily, monthly and annual returns.
#'
#' @export
get_returns_all <- function(df, ticker) {

  df$date <- as.Date(df$date, format = "%Y-%m-%d")

  df <- df[, c("date", "adjusted")]
  xts_prices <- xts::as.xts(df)

  # xts_monthly_returns <- quantmod::monthlyReturn(xts_prices)
  # xts_annual_returns <- quantmod::annualReturn(xts_prices)

  xts_all_returns <- quantmod::allReturns(xts_prices)

  df_all_returns <- as.data.frame(xts_all_returns)

  names(df_all_returns) <- paste0(ticker, ".", names(df_all_returns))

  df_all_returns$date <- rownames(df_all_returns)

  rownames(df_all_returns) <- 1:nrow(df_all_returns)

  df_all_returns$date <- as.Date(df_all_returns$date, format = "%Y-%m-%d")

  return(df_all_returns)

}

#' Write annualized returns to a csv file
#'
#' @usage write_annualized_returns(user_path, db_path)
#' @param user_path A single character string containing the directory of the user.
#' @param db_path A single character string containing the directory of the database.
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
write_annualized_returns <- function(user_path, db_path) {

  get_user_names(user_path)
  get_db_names(db_path)

  file_name <- list.files(path.returns, pattern = "daily_returns")

  df_returns <- data.table::fread(file.path(path.returns, file_name))

  names(df_returns) <- gsub("\\.daily", "", names(df_returns))

  df_returns$date <- as.Date(df_returns$date, format = "%Y-%m-%d")

  xts_returns_max <- xts::as.xts(df_returns)

  df_annualized_years <- data.frame(
    matrix(nrow = length(names(xts_returns_max)), ncol = 0,
           dimnames = list(names(xts_returns_max), NULL)))

  ## Annualized returns for 1, 3, 5 and 10 years
  annualize_return_periods <- c(1, 3, 5, 10)

  for (annualize_return_period in annualize_return_periods) {

    ## Select period
    xts_returns_Xy <- xts_returns_max[paste0(
      Sys.Date() - lubridate::years(annualize_return_period), "/")]

    ## Compute annualized return if prices exist for X years

    df_temp <- get_annualized_returns(xts_returns_Xy)

    names(df_temp) <- paste0(annualize_return_period, "y")

    df_annualized_years <- cbind(df_annualized_years, df_temp)

  }

  df_annualized_max <- get_annualized_returns(xts_returns_max)
  names(df_annualized_max) <- "max"
  df_annualized <- cbind(df_annualized_years, df_annualized_max)

  col_names <- names(df_annualized)
  df_annualized$ticker <- rownames(df_annualized)
  df_annualized <- df_annualized[, c("ticker", col_names)]

  file_path_transactions <- file.path(path.transactions, file.transactions)

  if (file.exists(file_path_transactions)) {

    df_transactions <- data.table::fread(file_path_transactions)

    df_isin_ticker <- data.table::fread(file.path(path.database, file.tickers.db))

    df_transactions <- merge(df_transactions,
                             df_isin_ticker,
                             by = "isin")

    df_ticker_date <- df_transactions[df_transactions$transaction_type == "Purchase",
                                      c("ticker", "transaction_date")]
    names(df_ticker_date)[names(df_ticker_date) == "transaction_date"] <- "date"
    df_ticker_date$date <- as.Date(df_ticker_date$date,
                                   format = "%d-%m-%Y")
    df_ticker_date <- df_ticker_date %>%
      dplyr::group_by(.data$ticker) %>%
      dplyr::filter(date == min(.data$date))
    df_ticker_date <- df_ticker_date %>%
      dplyr::group_by(.data$ticker) %>%
      dplyr::sample_n(size = 1)
    df_ticker_date$age_yrs <- floor(
      lubridate::time_length(difftime(Sys.Date(), df_ticker_date$date), "years"))

    ## Make return NA or "-" if column year greater than age_yrs
    df_annualized <- merge(df_annualized, df_ticker_date, by = "ticker")
    for (i in 1:nrow(df_annualized)) {
      incomplete_positions <- which(df_annualized$age_yrs[i] < annualize_return_periods) + 1
      df_annualized[i, incomplete_positions] <- NA
    }

    df_annualized <- df_annualized[, names(df_annualized) != "age_yrs"
                                   & names(df_annualized) != "date"]

    data.table::fwrite(
      df_annualized, file.path(path.returns, file.returns.annualized))

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

  file_name <- list.files(path.returns, pattern = "daily_returns")

  df_returns <- data.table::fread(file.path(path.returns, file_name))

  names(df_returns) <- gsub("\\.daily", "", names(df_returns))

  df_returns$date <- as.Date(df_returns$date, format = "%Y-%m-%d")

  df_returns[is.na(df_returns)] <- 0

  xts_returns_max <- xts::as.xts(df_returns)


  files_pricequantity_panels <- list.files(path.pricequantity.panel)

  if (length(files_pricequantity_panels) > 0) {

    files <- file.path(path.pricequantity.panel, files_pricequantity_panels)
    list_dfs <- lapply(files, data.table::fread)

  }

  df_weight_final <- data.frame(matrix(nrow = 0, ncol = 1,
                                       dimnames = list(NULL, "date")))

  for (i in 1:length(list_dfs)) {

    ticker <- stringr::str_match(files[i],
                                 "pricequantity_panel_(.*?)_from")[, 2]
    df_pricequantity_panel <- list_dfs[[i]]

    df_weight_panel <- df_pricequantity_panel[, c("date", "value")]
    names(df_weight_panel)[2] <- ticker

    df_weight_final <- merge(df_weight_final,
                             df_weight_panel,
                             by = "date",
                             all.x = TRUE, all.y = TRUE)

  }

  df_weight_final[is.na(df_weight_final)] <- 0
  df_weight_final$sum <- rowSums(df_weight_final[-1])
  df_weight_final <- df_weight_final[df_weight_final$sum > 0, ]
  df_weight_final[, 2:(length(df_weight_final) - 1)] <- df_weight_final[, 2:(length(df_weight_final) - 1)] / df_weight_final$sum
  df_weight_final <- df_weight_final[, names(df_weight_final) != "sum"]
  row.names(df_weight_final) <- df_weight_final$date
  # df_weight_final <- df_weight_final[, names(df_weight_final) != "date"]
  xts_weight <- xts::as.xts(df_weight_final)
  xts_weight <- xts_weight[, names(xts_weight) != "date"]
  storage.mode(xts_weight) <- "numeric"

  ## Get daily portfolio returns
  ## Super slow: find other solution for this: Create faster/slim version of this function
  xts_portfolio <- PerformanceAnalytics::Return.portfolio(xts_returns_max,
                                                          xts_weight)

  df_portfolio_daily <- data.frame(xts_portfolio)
  df_portfolio_daily$date <- row.names(df_portfolio_daily)
  df_portfolio_daily <- df_portfolio_daily[, c("date", "portfolio.returns")]
  names(df_portfolio_daily)[2] <- "portfolio_returns"
  row.names(df_portfolio_daily) <- 1:nrow(df_portfolio_daily)

  data.table::fwrite(df_portfolio_daily,
                     file.path(path.returns, file.return.portfolio.daily))

}

#' Write cumulative daily investment returns for all tickers to a csv file
#'
#' @usage write_roi_by_period_all(user_path, db_path)
#' @param user_path A single character string containing the directory of the user.
#' @param db_path A single character string containing the directory of the database.
#'
#' @export
write_roi_by_period_all <- function(user_path, db_path) {

  get_user_names(user_path)
  get_db_names(db_path)

  df_transactions <- data.table::fread(
    file.path(path.transactions, file.transactions))

  tickers <- get_tickers_from_db(df_transactions, db_path)[[2]]

  output <- mapply(write_roi_by_period, tickers,
                   MoreArgs = list(user_path))

}

#' Write cumulative daily investment returns to a csv file
#'
#' @usage write_roi_by_period(ticker, user_path)
#' @param ticker A single character string containing the ticker.
#' @param user_path A single character string. Path where data are stored.
#'
#' @export
write_roi_by_period <- function(ticker, user_path) {

  get_user_names(user_path)

  file_complete_panel <- list.files(path.complete.panel, pattern = ticker)

  if (length(file_complete_panel) > 0) {

    df_complete_panel <- data.table::fread(
      file.path(path.complete.panel, file_complete_panel))

    df_roi <- get_roi_by_period(df_complete_panel, nb_period = NULL,
                                period_type = "max")

    file_roi_panel <- paste0("return_on_investment_daily_", ticker,
                             "_from_", min(df_roi$date),
                             "_to_", max(df_roi$date),
                             ".csv")

    data.table::fwrite(df_roi, file.path(path.returns.roi, file_roi_panel))

  }

}

#' Get cumulative return on investment for specific period
#'
#' @usage get_roi_by_period(df_complete_panel, nb_period = NULL,
#'                          period_type = "max")
#' @param df_complete_panel A data frame containing the complete panel.
#' @param nb_period An integer indicating the number of months.
#' Default is \emph{NULL}.
#' @param period_type A single character string. Default \emph{max}. Possible
#' values \emph{max}, \emph{weeks} and \emph{months}.
#'
#' @return A data frame containing the cumulative return on investment
#' for a given period.
#'
#' @export
get_roi_by_period <- function(df_complete_panel, nb_period = NULL,
                              period_type = "max") {

  df_complete_panel_period <- get_df_with_selected_time_period(
    df = df_complete_panel, nb_period = nb_period, period_type = period_type)

  if (nrow(df_complete_panel_period) > 0) {


    if (period_type == "months" ||
        period_type == "weeks" ||
        period_type == "days") {

      index_first_period <- df_complete_panel_period$date == min(df_complete_panel_period$date)

      df_complete_panel_period <- df_complete_panel_period[order(df_complete_panel_period$date), ]

      ## Re-compute purchase cum value and first entry is simply equal to value
      df_complete_panel_period$purchase_value[index_first_period] <- df_complete_panel_period$value[index_first_period]
      df_complete_panel_period$purchase_cum_value <- cumsum(df_complete_panel_period$purchase_value)

      ## Re-compute dividend and sale cum value
      df_complete_panel_period$sale_cum_value <- cumsum(df_complete_panel_period$sale_value)
      df_complete_panel_period$dividend_cum_value <- cumsum(df_complete_panel_period$dividend_value)

    }

    df_complete_panel_period[is.na(df_complete_panel_period)] <- 0
    df_complete_panel_period <- df_complete_panel_period[df_complete_panel_period$cum_quantity != 0
                                                         | df_complete_panel_period$sale_value != 0
                                                         | df_complete_panel_period$dividend_value != 0, ]

    df_complete_panel_period$daily_cum_roi <- (df_complete_panel_period$value +
                                                 df_complete_panel_period$sale_cum_value +
                                                 df_complete_panel_period$dividend_cum_value) / df_complete_panel_period$purchase_cum_value

    df_roi_period <- df_complete_panel_period[, c("date", "daily_cum_roi")]

    return(df_roi_period)

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

  df_all <- get_complete_portfolio_panel(path)

  if (!is.null(df_all)) {

    df_all <- df_all[, c("date", "value", "purchase_value",
                         "sale_value", "dividend_value",
                         "currently_invested", "value_available")]

    ## Take sum by group "date" for value, purchase_value, sale_value and dividend_cum_value
    df_all <- data.table::setDT(
      df_all)[, lapply(.SD, sum, na.rm = TRUE), by = date]

    ## Re-compute cumulative dividend payments
    df_all <- df_all[order(df_all$date), ]
    df_all$dividend_cum_value <- cumsum(df_all$dividend_value)

    ## Remove all dates where currently invested is unequal value available
    ## this means that prices are not available for all current investments at time t
    df_all <- df_all[df_all$currently_invested == df_all$value_available, ]

    df_twr <- df_all

    df_twr[is.na(df_twr)] <- 0

    ## Remove all periods with no portfolio value, no purchase and no sale value at the same time
    days_empty_portfolio <- df_twr$value == 0 &
      df_twr$purchase_value == 0 &
      df_twr$sale_value == 0
    df_twr <- df_twr[!days_empty_portfolio, ]


    df_twr$end_value <- df_twr$value + df_twr$dividend_cum_value

    df_twr$initial_value <- data.table::shift(df_twr$value) +
      data.table::shift(df_twr$dividend_cum_value)

    df_twr$cash_flow <- df_twr$purchase_value - df_twr$sale_value

    ## Compute daily holding period return
    ## Cash flows occur just before the valuation (of end_value)
    df_twr$twr_factor <- (df_twr$end_value - df_twr$cash_flow) / df_twr$initial_value
    # df_twr$twr_factor <- df_twr$end_value / (df_twr$initial_value + df_twr$cash_flow)

    data.table::fwrite(df_twr, file.path(path.returns, file.returns.twr.daily))

  }

}

#' Get true time-weighted rate of return (TTWROR) of total portfolio
#'
#' @usage get_portfolio_ttwror(path, nb_period = NULL,
#'                             period_type = "max")
#' @param path A single character string. Path where data are stored.
#' @param nb_period An integer indicating the number of months. Default
#' is \emph{NULL}.
#' @param period_type A single character string. Default \emph{max}.
#' Possible values \emph{max}, \emph{weeks} and \emph{months}.
#'
#' @return A numeric for the true time-weighted rate of return (TTWROR) of
#' your portfolio
#'
#' @export
get_portfolio_ttwror <- function(path, nb_period = NULL,
                                 period_type = "max") {

  get_user_names(path)

  if (file.exists(file.path(path.returns, file.returns.twr.daily))) {

    df_twr_factors <- data.table::fread(file.path(path.returns,
                                                  file.returns.twr.daily))

    ## Select time period with TWR factors
    df_selected_period <- get_df_with_selected_time_period(
      df = df_twr_factors, nb_period = nb_period, period_type = period_type)

    ## Remove focal periods succeeding periods where value is zero (because
    ## for the subsequent focal period has no reasonable initial value)
    df_selected_period <- df_selected_period[data.table::shift(df_selected_period$value) != 0, ]

    periods <- length(df_selected_period$twr_factor[!is.na(df_selected_period$twr_factor)])

    ## Multiply all TWR factors to get ttwror
    ## Total period
    # ttwror <- prod(df_selected_period$twr_factor, na.rm = TRUE) - 1
    ## Annualized (meaning over a period of 365 trading days)
    annualized_ttwror <- prod(df_selected_period$twr_factor,
                              na.rm = TRUE)^(365.25 / periods) - 1

  } else {

    annualized_ttwror <- NA

  }

  return(annualized_ttwror)

}


#' Get internal rate of return (IRR) of total portfolio
#'
#' @usage get_portfolio_irr(path, nb_period = NULL, period_type = "max")
#' @param path A single character string. Path where data are stored.
#' @param nb_period An integer indicating the number of months. Default
#' is \emph{NULL}.
#' @param period_type A single character string. Default \emph{max}.
#' Possible values \emph{max}, \emph{weeks} and \emph{months}.
#'
#' @return A numeric for the internal rate of return (IRR) of your portfolio
#'
#' @export
#' @import data.table
get_portfolio_irr <- function(path, nb_period = NULL, period_type = "max") {

  cash_flow <- NULL

  get_user_names(path)

  df_complete_portfolio <- get_complete_portfolio_panel(path)


  if (!is.null(df_complete_portfolio)) {

    col_names <- c("date", "value", "purchase_value",
                   "sale_value", "dividend_value",
                   "currently_invested", "value_available")

    df_complete_portfolio <- df_complete_portfolio[, col_names]

    ## Take sum by group "date" for value, purchase_value, sale_value and dividend_value
    ## Group date means: the values for all individual investments are aggregated (summed)
    df_portfolio <- data.table::setDT(
      df_complete_portfolio)[, lapply(.SD, sum, na.rm = TRUE), by = date]

    ## Select time period for portfolio panel
    df_portfolio <- get_df_with_selected_time_period(
      df = df_portfolio, nb_period = nb_period, period_type = period_type)

    ## Remove all dates where currently invested is unequal value available
    ## this means that prices are not available for all current investments at time t
    df_portfolio <- df_portfolio[df_portfolio$currently_invested == df_portfolio$value_available, ]

    df_portfolio[is.na(df_portfolio)] <- 0

    ## Remove all periods with no portfolio value, no purchase and no sale
    ## value at the same time
    days_empty_portfolio <- df_portfolio$value == 0 &
      df_portfolio$purchase_value == 0 &
      df_portfolio$sale_value == 0
    df_portfolio <- df_portfolio[!days_empty_portfolio, ]

    ## In the first period, the investment would be hypothetically purchased
    ## If something was indeed purchased, this would appear in the "value"
    ## column. So no need to consider it twice
    is_first_day <- df_portfolio$date == min(df_portfolio$date)
    df_portfolio$purchase_value[is_first_day] <- df_portfolio$value[is_first_day]

    ## In the last period, the investment would be hypothetically sold
    ## If something was indeed sold on the last day, this would NOT appear
    ## in the "value" column. So need to consider this as well.
    is_last_day <- df_portfolio$date == max(df_portfolio$date)
    df_portfolio$sale_value[is_last_day] <- df_portfolio$sale_value[is_last_day] +
      df_portfolio$value[is_last_day]

    ## Compute cash flow
    df_portfolio$cash_flow <- df_portfolio$sale_value +
      df_portfolio$dividend_value -
      df_portfolio$purchase_value

    ## Aggregate cash flow on month level
    ## If it gets too slow, compute IRR on year level
    ## For now month level should be fine
    df_portfolio_by_month <- data.table::setDT(
      df_portfolio)[, list(cash_flow = sum(cash_flow)),
                    by = list(yr = lubridate::year(date),
                              mon = months(date))]


    ## Computation of IRR on monthly basis
    irr_final <- jrvFinance::irr(df_portfolio_by_month$cash_flow,
                                 cf.freq = 12, comp.freq = Inf)

  } else {

    irr_final <- NA

  }

  return(irr_final)

}
