#' Write returns for all tickers to csv files
#'
#' @usage write_returns(path)
#' @param path A single character string. Directory where all data are stored.
#'
#' @export
write_returns <- function(path) {

  list.names <- get_names(path)
  path.pricepanel <- list.names$path.pricepanel
  path.returns <- list.names$path.returns
  file.returns.daily <- list.names$file.returns.daily
  file.returns.monthly <- list.names$file.returns.monthly
  file.returns.annual <- list.names$file.returns.annual


  files.pricepanels <- list.files(path.pricepanel)

  no.pricepanels <- rlang::is_empty(files.pricepanels)

  if (!no.pricepanels) {

    files <- paste0(path.pricepanel, files.pricepanels)
    list.dfs <- lapply(files, data.table::fread)

    # last.year <- lubridate::year(Sys.Date()) - 1
    # df.annual <- data.frame(year = last.year)
    df.daily <- data.frame(matrix(nrow = 0, ncol = 1, dimnames = list(NULL, "date")))
    df.monthly <- data.frame(matrix(nrow = 0, ncol = 1, dimnames = list(NULL, "date")))
    df.annual <- data.frame(matrix(nrow = 0, ncol = 1, dimnames = list(NULL, "date")))

    ## could also do mapply and do.call (if necessary)
    for (i in 1:length(list.dfs)) {

      ticker <- stringr::str_match(files[i], "price_panel_(.*?)_from")[, 2]
      df.pricepanel <- list.dfs[[i]]

      df.temp <- get_returns_all(df.pricepanel, ticker)

      ticker.daily <- paste0(ticker, ".daily")
      ticker.monthly <- paste0(ticker, ".monthly")
      ticker.yearly <- paste0(ticker, ".yearly")

      df.daily.temp <- df.temp[!is.na(df.temp[, ticker.daily]), c("date", ticker.daily)]
      df.monthly.temp <- df.temp[!is.na(df.temp[, ticker.monthly]), c("date", ticker.monthly)]
      df.yearly.temp <- df.temp[!is.na(df.temp[, ticker.yearly]), c("date", ticker.yearly)]

      df.daily <- merge(df.daily, df.daily.temp, by = "date", all.x = TRUE, all.y = TRUE)
      df.monthly <- merge(df.monthly, df.monthly.temp, by = "date", all.x = TRUE, all.y = TRUE)
      df.annual <- merge(df.annual, df.yearly.temp, by = "date", all.x = TRUE, all.y = TRUE)

    }

    data.table::fwrite(df.daily, paste0(path.returns, file.returns.daily))
    data.table::fwrite(df.monthly, paste0(path.returns, file.returns.monthly))
    data.table::fwrite(df.annual, paste0(path.returns, file.returns.annual))

  } else { message("No price panels to calculate annual returns.") } ## end of if else statement

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

} ## end of function get_returns_all

#' Write annualized returns to a csv file
#'
#' @usage write_annualized_returns(path)
#' @param path A single character string. Directory where all data are stored.
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
write_annualized_returns <- function(path) {

  list.names <- get_names(path)
  path.returns <- list.names$path.returns
  path.tickers <- list.names$path.tickers
  path.transactions <- list.names$path.transactions
  file.transactions <- list.names$file.transactions
  file.tickers <- list.names$file.tickers
  file.name.annualized <- list.names$file.name.annualized

  df.transaction.history <- data.table::fread(paste0(path.transactions, file.transactions))
  df.isin.ticker <- data.table::fread(paste0(path.tickers, file.tickers))

  df.transaction.history <- merge(df.transaction.history, df.isin.ticker, by = "isin")

  returns.period <- "daily"

  file.name <- list.files(path.returns, pattern = returns.period)

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

  data.table::fwrite(df.annualized, paste0(path.returns, file.name.annualized))

}

#' Write portfolio return to a csv file
#'
#' @usage write_portfolio_return(path)
#' @param path A single character string. Directory where all data are stored.
#'
#' @export
write_portfolio_return <- function(path) {

  list.names <- get_names(path)
  path.returns <- list.names$path.returns
  path.tickers <- list.names$path.tickers
  path.pricequantitypanel <- list.names$path.pricequantitypanel
  path.transactions <- list.names$path.transactions
  file.transactions <- list.names$file.transactions
  file.tickers <- list.names$file.tickers
  file.return.portfolio.daily <- list.names$file.return.portfolio.daily

  df.transaction.history <- data.table::fread(paste0(path.transactions, file.transactions))
  df.isin.ticker <- data.table::fread(paste0(path.tickers, file.tickers))

  df.transaction.history <- merge(df.transaction.history, df.isin.ticker, by = "isin")

  returns.period <- "daily"

  file.name <- list.files(path.returns, pattern = returns.period)

  df.returns <- data.table::fread(paste0(path.returns, file.name))

  names(df.returns) <- gsub(paste0("\\.", returns.period), "", names(df.returns))

  df.returns$date <- as.Date(df.returns$date, "%Y-%m-%d")

  df.returns[is.na(df.returns)] <- 0

  xts.returns.max <- xts::as.xts(df.returns)



  files.pricequantitypanels <- list.files(path.pricequantitypanel)

  no.pricequantitypanels <- rlang::is_empty(files.pricequantitypanels)

  if (!no.pricequantitypanels) {

    files <- paste0(path.pricequantitypanel, files.pricequantitypanels)
    list.dfs <- lapply(files, data.table::fread)

  }

  df.weight.final <- data.frame(matrix(nrow = 0, ncol = 1, dimnames = list(NULL, "date")))

  for (i in 1:length(list.dfs)) {

    ticker <- stringr::str_match(files[i], "pricequantity_panel_(.*?)_from")[, 2]
    df.pricequantitypanel <- list.dfs[[i]]

    df.weightpanel <- df.pricequantitypanel[, c("date", "value")]
    names(df.weightpanel)[2] <- ticker

    df.weight.final <- merge(df.weight.final, df.weightpanel, by = "date", all.x = TRUE, all.y = TRUE)

  }

  df.weight.final[is.na(df.weight.final)] <- 0
  df.weight.final$sum <- rowSums(df.weight.final[-1])
  df.weight.final <- df.weight.final[df.weight.final$sum > 0, ]
  df.weight.final[, 2:(length(df.weight.final) - 1)] <- df.weight.final[, 2:(length(df.weight.final) - 1)] / df.weight.final$sum
  df.weight.final <- df.weight.final[, names(df.weight.final) != "sum"]
  row.names(df.weight.final) <- df.weight.final$date
  df.weight.final <- df.weight.final[, names(df.weight.final) != "date"]
  xts.weight <- xts::as.xts(df.weight.final)

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
#' @usage write_cum_investment_returns_daily(path)
#' @param path A single character string. Directory where all data are stored.
#'
#' @export
write_cum_investment_returns_daily <- function(path) {

  list.names <- get_names(path)
  path.value.panel <- list.names$path.value.panel
  path.tickers <- list.names$path.tickers
  path.transactions <- list.names$path.transactions
  file.transactions <- list.names$file.transactions
  file.tickers <- list.names$file.tickers

  df.transaction.history <- data.table::fread(paste0(path.transactions, file.transactions))

  ## get tickers from history of transactions
  tickers <- get_tickers_from_transactions(df.transaction.history, path)

  ## write cumulative daily investment return for all tickers
  output <- mapply(write_cum_investment_return_daily, tickers, MoreArgs = list(path))

}

#' Write cumulative daily investment returns to a csv file
#'
#' @usage write_cum_investment_return_daily(ticker, path)
#' @param ticker A single character string containing the ticker.
#' @param path A single character string. Directory where all data are stored.
#'
#' @export
write_cum_investment_return_daily <- function(ticker, path) {

  list.names <- get_names(path)
  path.value.panel <- list.names$path.value.panel
  path.pricequantitypanel <- list.names$path.pricequantitypanel
  path.returns.roi <- list.names$path.returns.roi
  path.tickers <- list.names$path.tickers
  path.transactions <- list.names$path.transactions
  file.transactions <- list.names$file.transactions
  file.tickers <- list.names$file.tickers

  if (!rlang::is_empty(list.files(paste0(path.pricequantitypanel), pattern = ticker))) {

    df.pricequantitypanel <- data.table::fread(paste0(path.pricequantitypanel,
                                              list.files(paste0(path.pricequantitypanel), pattern = ticker)))

    df.pricequantitypanel <- df.pricequantitypanel[, c("date", "adjusted", "value", "cum_quantity")]

    ## load value panels
    if (!rlang::is_empty(list.files(paste0(path.value.panel), pattern = ticker))) {

      ticker.value.panels <- list.files(paste0(path.value.panel), pattern = ticker)

      purchase.exist <- grepl("^purchase", ticker.value.panels)
      sale.exist <- grepl("^sale", ticker.value.panels)
      dividend.exist <- grepl("^dividend", ticker.value.panels)
      if (any(purchase.exist)) df.purchasevaluepanel <- data.table::fread(paste0(path.value.panel, ticker.value.panels[purchase.exist]))
      if (any(sale.exist)) df.salevaluepanel <- data.table::fread(paste0(path.value.panel, ticker.value.panels[sale.exist]))
      if (any(dividend.exist)) df.dividendvaluepanel <- data.table::fread(paste0(path.value.panel, ticker.value.panels[dividend.exist]))

      if ( exists("df.purchasevaluepanel") ) {

        df.roi <- merge(df.pricequantitypanel, df.purchasevaluepanel, by = "date", all = TRUE)

        if ( exists("df.salevaluepanel") ) {
          df.roi <- merge(df.roi, df.salevaluepanel, by = "date", all = TRUE)
        } else {
          df.roi$sale_cum_value <- 0
          df.roi$sale_value <- 0
          }

        if ( exists("df.dividendvaluepanel") ) {
          df.roi <- merge(df.roi, df.dividendvaluepanel, by = "date", all = TRUE)
        } else {
          df.roi$dividend_cum_value <- 0
          df.roi$dividend_value <- 0
          }

      }

      df.roi[is.na(df.roi)] <- 0
      df.roi <- df.roi[df.roi$cum_quantity != 0 | df.roi$sale_value != 0 | df.roi$dividend_value != 0, ]

      df.roi$daily_cum_roi <- (df.roi$value + df.roi$sale_cum_value + df.roi$dividend_cum_value ) / df.roi$purchase_cum_value

      df.roi <- df.roi[, c("date", "daily_cum_roi")]

      ## start and end date
      from <- min(df.roi$date)
      to <- max(df.roi$date)

      ## file name
      filename.roi.panel <- paste0("return_on_investment_daily_", ticker, "_from_", from, "_to_", to, ".csv")

      ## store price quantity panel as csv
      data.table::fwrite(df.roi, paste0(path.returns.roi, filename.roi.panel))

      message("Daily investment return for ", ticker, " successfully created!")

    } else { message("No transaction value panels available.") }

  } else { message("No price-quantity panel available.") }


}

# PerformanceAnalytics::Return.cumulative
# PerformanceAnalytics::Return.portfolio()

# ## load tables with annual return
# if (!rlang::is_empty(list.files(paste0(path.returns), pattern = "^annual_returns_all_from_"))) {
#
#   ## file name for annual returns
#   filename.annual.returns <- list.files(paste0(path.returns), pattern = "^annual_returns_all_from_")
#
#   ## choose annual returns with most recent year and minimum year
#   filename.annual.returns <- filename.annual.returns[grepl(max(as.numeric(stringr::str_match(filename.annual.returns,
#                                                                                              "to_(.*?).csv")[,2])), filename.annual.returns)]
#   filename.annual.returns <- filename.annual.returns[grepl(min(as.numeric(stringr::str_match(filename.annual.returns,
#                                                                                              "from_(.*?)_to_")[,2])), filename.annual.returns)]
#
#   ## load annual returns
#   df.annual.returns <- data.table::fread(paste0(path.returns, filename.annual.returns))
#
#   ## clean table
#   names(df.annual.returns)[names(df.annual.returns) == "year"] <- "Year"
#   df.annual.returns <- df.annual.returns[rev(order(df.annual.returns$Year)), ]
#
#   ## change column names
#   names(df.annual.returns) <- gsub("\\.yearly\\.returns", "", names(df.annual.returns))
#
#   ## add one missing year to get calculation of mean annual returns right
#   df.annual.returns <- rbind(df.annual.returns,df.annual.returns[nrow(df.annual.returns) + 1, ])
#   df.annual.returns[nrow(df.annual.returns), "Year"] <- min(df.annual.returns$Year, na.rm = TRUE) - 1
#
#   ## compute mean annual returns for 1yr, 3yrs, 5yrs, 10yrs and MAX
#   ## table with rows being investments and columns being 1Y,3Y, 5Y, 10Y, MAX annual returns
#
#   df.annual.returns <- as.data.frame(df.annual.returns)
#
#   ## multiply all columns with 100 to get returns in percent
#   df.annual.returns[,names(df.annual.returns) != "Year"] <- df.annual.returns[,names(df.annual.returns) != "Year"] * 100
#
#
#   df.mean.annual.returns <- data.frame(matrix(nrow = 0, ncol = 6))
#
#   for (i in 2:length(df.annual.returns)) {
#
#     ticker <- names(df.annual.returns)[i]
#     annual.returns.1y <- mean(df.annual.returns[df.annual.returns$Year > lubridate::year(Sys.Date()) - 1, i])
#     annual.returns.3y <- mean(df.annual.returns[df.annual.returns$Year > lubridate::year(Sys.Date()) - 3, i])
#     annual.returns.5y <- mean(df.annual.returns[df.annual.returns$Year > lubridate::year(Sys.Date()) - 5, i])
#     annual.returns.10y <- mean(df.annual.returns[df.annual.returns$Year > lubridate::year(Sys.Date()) - 10, i])
#     annual.returns.max <- mean(df.annual.returns[, i])
#     df.temp <- data.frame(ticker, annual.returns.1y, annual.returns.3y, annual.returns.5y, annual.returns.10y,
#                           annual.returns.max)
#     df.mean.annual.returns <- rbind(df.mean.annual.returns, df.temp)
#
#   } ## end of for loop which creates mean annual returns
#
#   ## add name and ISIN
#   df.mean.annual.returns <- merge(df.mean.annual.returns, df.ticker.investmentnames, by = "ticker")
#   df.mean.annual.returns <- merge(df.mean.annual.returns, df.isin.ticker.converter, by = "ticker")
#
#   names(df.mean.annual.returns) <- c("Ticker", "1Y", "3Y", "5Y", "10Y", "Max", "Name", "ISIN")
#   df.mean.annual.returns <- df.mean.annual.returns[, c("Name", "ISIN", "Ticker", "1Y", "3Y", "5Y", "10Y", "Max")]
#
#   ## formatting numbers
#   df.mean.annual.returns$`1Y` <- as.numeric(formatC(df.mean.annual.returns$`1Y`, digits = 2, format = "f"))
#   df.mean.annual.returns$`3Y` <- as.numeric(formatC(df.mean.annual.returns$`3Y`, digits = 2, format = "f"))
#   df.mean.annual.returns$`5Y` <- as.numeric(formatC(df.mean.annual.returns$`5Y`, digits = 2, format = "f"))
#   df.mean.annual.returns$`10Y` <- as.numeric(formatC(df.mean.annual.returns$`10Y`, digits = 2, format = "f"))
#   df.mean.annual.returns$Max <- as.numeric(formatC(df.mean.annual.returns$Max, digits = 2, format = "f"))
#
#   ## formatting of annual returns
#   if (length(df.annual.returns) > 3) {
#
#     df.annual.returns[, names(df.annual.returns) != "Year"] <- apply(df.annual.returns[, names(df.annual.returns) != "Year"],
#                                                                      2, formatC, digits = 2, format = "f")
#
#     } else if (length(df.annual.returns) == 2) {
#
#     df.annual.returns[, 2] <- formatC(df.annual.returns[, 2], digits = 2, format = "f")
#
#   }
#
# } ## end of if statement is empty



# get.daily.returns <- function(df){
#
#   #### get daily returns
#
#   ticker <- unique(df$ticker)
#
#   df$date <- as.Date(df$date, "%Y-%m-%d")
#
#   rownames(df) <- df$date
#
#   df <- df[, c("date", "adjusted")]
#   df <- xts::as.xts(df)
#   df <- quantmod::dailyReturn(df)
#   df <- as.data.frame(df)
#
#   names(df) <- paste0(ticker, ".", names(df))
#
#   df$date <- rownames(df)
#   rownames(df) <- 1:nrow(df)
#
#   df$date <- as.Date(df$date, "%Y-%m-%d")
#   df$year <- as.numeric(format(df$date, "%Y"))
#
#   df <- df[,names(df) != "date"]
#
#   return(df)
#
#
# } ## end of function get.daily.returns


# mean(get.annual.returns(df2))


# portfolio return needs to be weighted by share of investment value in overall portfolio value
#
# df1.returns <- Return.calculate(df1.adj)
# df1.returns.value <- Return.calculate(df1.value)
#
# # Annualized Performance with Risk Free Rate 4.5%
# performance_table <- as.data.frame(table.AnnualizedReturns(df1.returns, Rf = 0.05/279.8))
# performance_table <- rownames_to_column(performance_table)
# names(performance_table)[1] <- 'Performance'
#
# # Tidying Annualized Performance Dataframe
# performance_df <- performance_table %>% gather(key = 'Code', value = 'Values', -Performance) %>% spread(key = Performance, value = Values) %>%
#   rename('Annualized_Return' = 'Annualized Return', 'Annualized_Sharpe' = 'Annualized Sharpe (Rf=4.5%)','Annualized_StdDev' = 'Annualized Std Dev' ) %>%
#   select(Code,Annualized_Return, Annualized_StdDev, Annualized_Sharpe)
