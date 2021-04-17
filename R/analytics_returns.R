#' Write returns for all tickers to csv files
#'
#' @usage write_returns(path)
#' @param path A single character string. Directory where all data are stored.
#'
#' @export
write_returns <- function(path) {

  #### write returns for all tickers

  ## create folder if not exists and get folder name for price panel
  list.paths <- PortfolioTracker::create_portfoliotracker_dir(path)
  path.pricepanel <- list.paths$path.pricepanel
  path.returns <- list.paths$path.returns
  files.pricepanels <- list.files(path.pricepanel)

  ## load price panel
  no.pricepanels <- !rlang::is_empty(files.pricepanels)

  if (no.pricepanels) {

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

      df.temp <- PortfolioTracker::get_returns_all(df.pricepanel, ticker)

      ticker.daily <- paste0(ticker, ".daily")
      ticker.monthly <- paste0(ticker, ".monthly")
      ticker.yearly <- paste0(ticker, ".yearly")

      df.daily.temp <- df.temp[!is.na(df.temp[, ticker.daily]), c("date", ticker.daily)]
      df.monthly.temp <- df.temp[!is.na(df.temp[, ticker.monthly]), c("date", ticker.monthly)]
      df.yearly.temp <- df.temp[!is.na(df.temp[, ticker.yearly]), c("date", ticker.yearly)]

      df.daily <- merge(df.daily, df.daily.temp, by = "date", all.x = TRUE, all.y = TRUE)
      df.monthly <- merge(df.monthly, df.monthly.temp, by = "date", all.x = TRUE, all.y = TRUE)
      df.annual <- merge(df.annual, df.yearly.temp, by = "date", all.x = TRUE, all.y = TRUE)

    } ## end of for loop

    data.table::fwrite(df.daily, paste0(path.returns, "daily_returns.csv"))
    data.table::fwrite(df.monthly, paste0(path.returns, "monthly_returns.csv"))
    data.table::fwrite(df.annual, paste0(path.returns, "annual_returns.csv"))

  } else { message("No price panels to calculate annual returns.") } ## end of if else statement

} ## end of function write_returns

#' Get annual returns
#'
#' @usage get_returns_all(df, ticker)
#' @param df A data frame containing date and prices.
#' @param ticker A single character string containing the ticker.
#' @return df.allreturns A data frame containing years and annual returns.
#'
#' @export
get_returns_all <- function(df, ticker) {

  #### get all returns

  df$date <- as.Date(df$date, "%Y-%m-%d")

  df <- df[, c("date", "adjusted")]
  xts.prices <- xts::as.xts(df)


  xts.monthlyreturns <- quantmod::monthlyReturn(xts.prices)
  xts.annualreturns <- quantmod::annualReturn(xts.prices)

  xts.allreturns <- quantmod::allReturns(xts.prices)

  # df.annualreturns <- as.data.frame(xts.annualreturns)
  # df.monthlyreturns <- as.data.frame(xts.monthlyreturns)
  df.allreturns <- as.data.frame(xts.allreturns)

  # names(df.annualreturns) <- paste0(ticker, ".", names(df.annualreturns))
  # names(df.monthlyreturns) <- paste0(ticker, ".", names(df.monthlyreturns))
  names(df.allreturns) <- paste0(ticker, ".", names(df.allreturns))

  # df.annualreturns$date <- rownames(df.annualreturns)
  # df.monthlyreturns$date <- rownames(df.monthlyreturns)
  df.allreturns$date <- rownames(df.allreturns)

  # rownames(df.annualreturns) <- 1:nrow(df.annualreturns)
  rownames(df.allreturns) <- 1:nrow(df.allreturns)

  # df.annualreturns$date <- as.Date(df.annualreturns$date, "%Y-%m-%d")
  # df.monthlyreturns$date <- as.Date(df.monthlyreturns$date, "%Y-%m-%d")
  df.allreturns$date <- as.Date(df.allreturns$date, "%Y-%m-%d")

  # df.annualreturns$year <- as.numeric(format(df.annualreturns$date, "%Y"))

  # df.annualreturns <- df.annualreturns[, names(df.annualreturns) != "date"]

  # list.returns <-list(
  #   df.annualreturns = df.annualreturns,
  #   df.monthlyreturns = df.monthlyreturns
  # )

  return(df.allreturns)

} ## end of function get_returns_all


#' Write annualized returns to a csv file
#'
#' @usage write_annualized_returns(path)
#' @param path A single character string. Directory where all data are stored.
#'
#' @export
write_annualized_returns <- function(path) {

  #### write annualized returns to csv

  ## create folder if not exists and get folder name for price panel
  list.paths <- PortfolioTracker::create_portfoliotracker_dir(path)
  path.returns <- list.paths$path.returns

  returns.period <- "daily"

  file.name <- list.files(path.returns, pattern = returns.period)

  df.returns <- data.table::fread(paste0(path.returns, file.name))

  names(df.returns) <- gsub(paste0("\\.", returns.period), "", names(df.returns))

  df.returns$date <- as.Date(df.returns$date, "%Y-%m-%d")

  xts.returns.max <- xts::as.xts(df.returns)

  df.annualized <- data.frame(matrix(nrow = length(names(xts.returns.max)), ncol = 0,
                                     dimnames = list(names(xts.returns.max), NULL)))

  annualize.return.periods <- c(1, 3, 5, 10)

  for(annualize.return.period in annualize.return.periods){

    xts.returns.Xy <- xts.returns.max[paste0(Sys.Date() - lubridate::years(annualize.return.period), "/")]

    annualized.returns.Xy <- PerformanceAnalytics::Return.annualized(xts.returns.Xy)

    df.temp <- as.data.frame(t(annualized.returns.Xy))
    names(df.temp) <- paste0(annualize.return.period, "y")

    df.annualized <- cbind(df.annualized, df.temp)

  } # end of for loop

  annualized.returns.max <- PerformanceAnalytics::Return.annualized(xts.returns.max)
  df.temp <- as.data.frame(t(annualized.returns.max))
  names(df.temp) <- "max"
  df.annualized <- cbind(df.annualized, df.temp)

  col.names <- names(df.annualized)

  df.annualized$ticker <- rownames(df.annualized)

  df.annualized <- df.annualized[, c("ticker", col.names)]

  file.name.annualized <- "annualized_returns.csv"

  data.table::fwrite(df.annualized, paste0(path.returns, file.name.annualized))

} ## end of function write_annualized_returns


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
# #Annualized Performance with Risk Free Rate 4.5%
# performance_table <- as.data.frame(table.AnnualizedReturns(df1.returns, Rf = 0.05/279.8))
# performance_table <- rownames_to_column(performance_table)
# names(performance_table)[1] <- 'Performance'
#
# #Tidying Annualized Performance Dataframe
# performance_df <- performance_table %>% gather(key = 'Code', value = 'Values', -Performance) %>% spread(key = Performance, value = Values) %>%
#   rename('Annualized_Return' = 'Annualized Return', 'Annualized_Sharpe' = 'Annualized Sharpe (Rf=4.5%)','Annualized_StdDev' = 'Annualized Std Dev' ) %>%
#   select(Code,Annualized_Return, Annualized_StdDev, Annualized_Sharpe)