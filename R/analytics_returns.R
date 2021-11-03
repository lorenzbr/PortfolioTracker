#' Write returns for all tickers to csv files
#'
#' @usage write_returns(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
write_returns <- function(path) {

  get_names(path)

  files.price.panels <- list.files(path.price.panel)

  no.price.panels <- rlang::is_empty(files.price.panels)

  if ( !no.price.panels ) {

    files <- paste0(path.price.panel, files.price.panels)
    list.dfs <- lapply(files, data.table::fread)

    # last.year <- lubridate::year(Sys.Date()) - 1
    # df.annual <- data.frame(year = last.year)
    df.daily <- data.frame(matrix(nrow = 0, ncol = 1, dimnames = list(NULL, "date")))
    df.monthly <- data.frame(matrix(nrow = 0, ncol = 1, dimnames = list(NULL, "date")))
    df.annual <- data.frame(matrix(nrow = 0, ncol = 1, dimnames = list(NULL, "date")))

    ## could also do mapply and do.call (if necessary)
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

      df.daily <- merge(df.daily, df.daily.temp, by = "date", all.x = TRUE, all.y = TRUE)
      df.monthly <- merge(df.monthly, df.monthly.temp, by = "date", all.x = TRUE, all.y = TRUE)
      df.annual <- merge(df.annual, df.yearly.temp, by = "date", all.x = TRUE, all.y = TRUE)

    }

    data.table::fwrite(df.daily, paste0(path.returns, file.returns.daily))
    data.table::fwrite(df.monthly, paste0(path.returns, file.returns.monthly))
    data.table::fwrite(df.annual, paste0(path.returns, file.returns.annual))

  } else {

    message("No price panels to calculate annual returns.")

  }

}

#' Get returns
#'
#' @usage get_returns_all(df, ticker)
#' @param df A data.frame containing date and prices.
#' @param ticker A single character string containing the ticker.
#' @return df.allreturns A data.frame containing daily, monthly and annual returns.
#'
#' @export
get_returns_all <- function(df, ticker) {

  df$date <- as.Date(df$date, "%Y-%m-%d")

  df <- df[, c("date", "adjusted")]
  xts.prices <- xts::as.xts(df)

  xts.monthlyreturns <- quantmod::monthlyReturn(xts.prices)
  xts.annualreturns <- quantmod::annualReturn(xts.prices)

  xts.allreturns <- quantmod::allReturns(xts.prices)

  df.allreturns <- as.data.frame(xts.allreturns)

  names(df.allreturns) <- paste0(ticker, ".", names(df.allreturns))

  df.allreturns$date <- rownames(df.allreturns)

  rownames(df.allreturns) <- 1:nrow(df.allreturns)

  df.allreturns$date <- as.Date(df.allreturns$date, "%Y-%m-%d")

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

  get_names(path)

  returns.period <- "daily"
  returns.period.file <- "daily_returns"

  file.name <- list.files(path.returns, pattern = returns.period.file)

  df.returns <- data.table::fread(paste0(path.returns, file.name))

  names(df.returns) <- gsub(paste0("\\.", returns.period), "", names(df.returns))

  df.returns$date <- as.Date(df.returns$date, "%Y-%m-%d")

  xts.returns.max <- xts::as.xts(df.returns)

  df.annualized <- data.frame(matrix(nrow = length(names(xts.returns.max)), ncol = 0,
                                     dimnames = list(names(xts.returns.max), NULL)))

  annualize.return.periods <- c(1, 3, 5, 10)

  for (annualize.return.period in annualize.return.periods) {

    xts.returns.Xy <- xts.returns.max[paste0(Sys.Date() - lubridate::years(annualize.return.period), "/")]

    ## compute annualized return if prices exist for X years

    annualized.returns.Xy <- PerformanceAnalytics::Return.annualized(xts.returns.Xy)

    df.temp <- as.data.frame(t(annualized.returns.Xy))
    names(df.temp) <- paste0(annualize.return.period, "y")

    df.annualized <- cbind(df.annualized, df.temp)

  }

  annualized.returns.max <- PerformanceAnalytics::Return.annualized(xts.returns.max)
  df.temp <- as.data.frame(t(annualized.returns.max))
  names(df.temp) <- "max"
  df.annualized <- cbind(df.annualized, df.temp)

  col.names <- names(df.annualized)
  df.annualized$ticker <- rownames(df.annualized)
  df.annualized <- df.annualized[, c("ticker", col.names)]


  transaction.history.exists <- file.exists(file.path(path.transactions, file.transactions))
  isin.ticker.exists <- file.exists(file.path(path.tickers, file.tickers))

  if (transaction.history.exists && isin.ticker.exists) {

    df.transaction.history <- data.table::fread(file.path(path.transactions, file.transactions))
    df.isin.ticker <- data.table::fread(file.path(path.tickers, file.tickers))

    df.transaction.history <- merge(df.transaction.history, df.isin.ticker, by = "isin")

    df.ticker.date <- df.transaction.history[df.transaction.history$transaction_type == "Purchase",
                                                        c("ticker", "transaction_date")]
    names(df.ticker.date)[names(df.ticker.date) == "transaction_date"] <- "date"
    df.ticker.date$date <- as.Date(df.ticker.date$date, "%d-%m-%Y")
    df.ticker.date <- df.ticker.date %>% dplyr::group_by(.data$ticker) %>% dplyr::filter(date == min(.data$date))
    df.ticker.date <- df.ticker.date %>% dplyr::group_by(.data$ticker) %>% dplyr::sample_n(size = 1)
    df.ticker.date$age_yrs <- floor(lubridate::time_length(difftime(Sys.Date(), df.ticker.date$date), "years"))

    ## make return NA or "-" if column year greater than age_yrs
    df.annualized <- merge(df.annualized, df.ticker.date, by = "ticker")
    for (i in 1:nrow(df.annualized)) {
      incomplete.positions <- which(df.annualized$age_yrs[i] < annualize.return.periods) + 1
      df.annualized[i, incomplete.positions] <- NA
    }

    df.annualized <- df.annualized[, names(df.annualized) != "age_yrs" & names(df.annualized) != "date"]

    data.table::fwrite(df.annualized, paste0(path.returns, file.returns.annualized))

  }

}

#' Write portfolio return to a csv file
#'
#' @usage write_portfolio_return(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
write_portfolio_return <- function(path) {

  get_names(path)

  # df.transaction.history <- data.table::fread(file.path(path.transactions, file.transactions))
  # df.isin.ticker <- data.table::fread(file.path(path.tickers, file.tickers))
  #
  # df.transaction.history <- merge(df.transaction.history, df.isin.ticker, by = "isin")

  returns.period <- "daily"
  returns.period.file <- "daily_returns"

  file.name <- list.files(path.returns, pattern = returns.period.file)

  df.returns <- data.table::fread(paste0(path.returns, file.name))

  names(df.returns) <- gsub(paste0("\\.", returns.period), "", names(df.returns))

  df.returns$date <- as.Date(df.returns$date, "%Y-%m-%d")

  df.returns[is.na(df.returns)] <- 0

  xts.returns.max <- xts::as.xts(df.returns)


  files.pricequantity.panels <- list.files(path.pricequantity.panel)

  no.pricequantity.panels <- rlang::is_empty(files.pricequantity.panels)

  if (!no.pricequantity.panels) {

    files <- paste0(path.pricequantity.panel, files.pricequantity.panels)
    list.dfs <- lapply(files, data.table::fread)

  }

  df.weight.final <- data.frame(matrix(nrow = 0, ncol = 1, dimnames = list(NULL, "date")))

  for (i in 1:length(list.dfs)) {

    ticker <- stringr::str_match(files[i], "pricequantity_panel_(.*?)_from")[, 2]
    df.pricequantity.panel <- list.dfs[[i]]

    df.weight.panel <- df.pricequantity.panel[, c("date", "value")]
    names(df.weight.panel)[2] <- ticker

    df.weight.final <- merge(df.weight.final, df.weight.panel, by = "date", all.x = TRUE, all.y = TRUE)

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

  ## get daily portfolio returns
  xts.portfolio <- PerformanceAnalytics::Return.portfolio(xts.returns.max, xts.weight)

  df.portfolio.daily <- data.frame(xts.portfolio)
  df.portfolio.daily$date <- row.names(df.portfolio.daily)
  df.portfolio.daily <- df.portfolio.daily[, c("date", "portfolio.returns")]
  names(df.portfolio.daily)[2] <- "portfolio_returns"
  row.names(df.portfolio.daily) <- 1:nrow(df.portfolio.daily)

  data.table::fwrite(df.portfolio.daily, paste0(path.returns, file.return.portfolio.daily))

}

#' Write cumulative daily investment returns to a csv file
#'
#' @usage write_roi_by_period_all(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
write_roi_by_period_all <- function(path) {

  get_names(path)

  df.transaction.history <- data.table::fread(paste0(path.transactions, file.transactions))

  ## get tickers from history of transactions
  tickers <- get_tickers_from_transactions(df.transaction.history, path)

  ## write cumulative daily investment return for all tickers
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

  get_names(path)

  if ( !rlang::is_empty(list.files(paste0(path.complete.panel), pattern = ticker)) ) {

    df.complete.panel <- data.table::fread(paste0(path.complete.panel,
                                              list.files(paste0(path.complete.panel), pattern = ticker)))

    df.roi <- get_roi_by_period(df.complete.panel, nb_period = NULL, period = "max")

    ## start and end date
    from <- min(df.roi$date)
    to <- max(df.roi$date)

    ## file name
    file.roi.panel <- paste0("return_on_investment_daily_", ticker, "_from_", from, "_to_", to, ".csv")

    ## Store price quantity panel as csv
    data.table::fwrite(df.roi, paste0(path.returns.roi, file.roi.panel))

    message("Daily investment return for ", ticker, " successfully created!")

  } else { message("No complete panel available for ", ticker, ".") }

}

#' Get cumulative return on investment for specific period
#'
#' @usage get_roi_by_period(df.complete.panel, nb_period = NULL, period_type = "max")
#' @param df.complete.panel A data.frame containing the complete panel.
#' @param nb_period An integer indicating the number of months. Default is \emph{NULL}.
#' @param period_type A single character string. Default \emph{max}. Possible values \emph{max}, \emph{weeks} and \emph{months}.
#'
#' @return df.roi.period A data.frame containing the cumulative return on investment for a given period.
#'
#' @export
get_roi_by_period <- function(df.complete.panel, nb_period = NULL, period_type = "max") {

  df.complete.panel.period <- get_df_with_selected_time_period(df = df.complete.panel,
                                                               nb_period = nb_period,
                                                               period_type = period_type)

  if ( nrow(df.complete.panel.period) > 0) {


    if (period_type == "months" || period_type == "weeks" || period_type == "days") {

      index.first.period <- df.complete.panel.period$date == min(df.complete.panel.period$date)

      df.complete.panel.period <- df.complete.panel.period[order(df.complete.panel.period$date), ]

      ## re-compute purchase cum value and first entry is simply equal to value
      df.complete.panel.period$purchase_value[index.first.period] <- df.complete.panel.period$value[index.first.period]
      df.complete.panel.period$purchase_cum_value <- cumsum(df.complete.panel.period$purchase_value)

      ## re-compute dividend and sale cum value
      df.complete.panel.period$sale_cum_value <- cumsum(df.complete.panel.period$sale_value)
      df.complete.panel.period$dividend_cum_value <- cumsum(df.complete.panel.period$dividend_value)

    }


    df.complete.panel.period[is.na(df.complete.panel.period)] <- 0
    df.complete.panel.period <- df.complete.panel.period[df.complete.panel.period$cum_quantity != 0
                                                         | df.complete.panel.period$sale_value != 0
                                                         | df.complete.panel.period$dividend_value != 0, ]

    df.complete.panel.period$daily_cum_roi <- (df.complete.panel.period$value + df.complete.panel.period$sale_cum_value
                                    + df.complete.panel.period$dividend_cum_value ) / df.complete.panel.period$purchase_cum_value

    df.roi.period <- df.complete.panel.period[, c("date", "daily_cum_roi")]

    return(df.roi.period)

  }

}

#' Get TWR factors on daily basis for portfolio
#'
#' Get the true-weighted return factors on a daily basis for the entire portfolio
#'
#' @usage get_twr_factors(path)
#' @param path A single character string. Path where data are stored.
#'
#' @export
get_twr_factors <- function(path) {

  get_names(path)

  files.complete.panels <- list.files(path.complete.panel)

  no.complete.panels <- rlang::is_empty(files.complete.panels)

  if ( !no.complete.panels ) {

    ## load all complete panels
    files <- paste0(path.complete.panel, files.complete.panels)
    list.dfs <- lapply(files, data.table::fread)

    df.all <- do.call(rbind, list.dfs)

    df.all <- df.all[, c("date", "value", "purchase_value", "sale_value",
                         "dividend_cum_value")]

    ## take sum by group "date" for value, purchase_value, sale_value and dividend_cum_value
    df.all <- data.table::setDT(df.all)[, lapply(.SD, sum, na.rm = TRUE), by = date]

    first.day <- min(df.all$date)
    last.day <- max(df.all$date)

    ## get daily full time period but remove saturday and sunday
    full.time.period <- seq(first.day, last.day, by = "day")
    mysysgetlocale <- Sys.getlocale('LC_TIME')
    Sys.setlocale('LC_TIME', 'ENGLISH')
    ## remove weekends
    full.time.period <- full.time.period[!weekdays(full.time.period) %in% c('Saturday', 'Sunday')]
    Sys.setlocale('LC_TIME', mysysgetlocale)

    df.twr <- data.frame(date = full.time.period)

    df.twr <- merge(df.twr, df.all, by = "date", all.x = TRUE)

    df.twr[is.na(df.twr)] <- 0

    ## remove all periods with no portfolio value, no purchase and no sale value at the same time
    days.empty.portfolio <- df.twr$value == 0 & df.twr$purchase_value == 0 & df.twr$sale_value == 0

    df.twr <- df.twr[ !days.empty.portfolio, ]

    ## potential to do:
      ## 1. for some days no prices (i.e. no value information) is available
      ## if value is zero and pre period purchase is zero, take value from last period
      ## OR 2. remove all periods which have pre-period value and cash flow (purchase and sale value)
      ## equal to zero, because holding period return cannot be computed (divide by zero)

    ## compute end value
    df.twr$end_value <- df.twr$value + df.twr$dividend_cum_value
    ## compute initial value
    df.twr$initial_value <- data.table::shift(df.twr$value) + data.table::shift(df.twr$dividend_cum_value)
    ## compute cash flow
    df.twr$cash_flow <- df.twr$purchase_value - df.twr$sale_value

    ## compute daily holding period return
    df.twr$twr_factor <- df.twr$end_value / (df.twr$initial_value + df.twr$cash_flow)

    data.table::fwrite(df.twr, paste0(path.returns, file.returns.twr.daily))

  } else {

    message("No complete panels to compute TWR factors available.")

  }

}

#' Get true time-weighted rate of return (TTWROR) for portfolio
#'
#' @usage get_ttwror(path, nb_period = NULL, period_type = "max")
#' @param path A single character string. Path where data are stored.
#' @param nb_period An integer indicating the number of months. Default is \emph{NULL}.
#' @param period_type A single character string. Default \emph{max}. Possible values \emph{max}, \emph{weeks} and \emph{months}.
#'
#' @return A numeric for the true time-weighted rate of return (TTWROR) for your portfolio
#'
#' @export
get_ttwror <- function(path, nb_period = NULL, period_type = "max") {

  get_names(path)

  if ( file.exists(file.path(path.returns, file.returns.twr.daily)) ) {

    df.twr.factors <- data.table::fread(file.path(path.returns, file.returns.twr.daily))

    ## Select time period with TWR factors
    df.selected.period <- get_df_with_selected_time_period(df = df.twr.factors,
                                                           nb_period = nb_period,
                                                           period_type = period_type)

    ## Multiply all TWR factors to get ttwror
    periods <- length(df.selected.period$twr_factor[!is.na(df.selected.period$twr_factor)])

    ## Total period
    # ttwror <- prod(df.selected.period$twr_factor, na.rm = TRUE) - 1
    ## Annualized (meaning over a period of 365 trading days)
    annualized.ttwror <- prod(df.selected.period$twr_factor, na.rm = TRUE)^(365/periods) - 1

  } else {

    message("TWR factors for portfolio are not available.")
    annualized.ttwror <- NA

  }

  return(annualized.ttwror)

}
