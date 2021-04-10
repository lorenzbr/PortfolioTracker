#' Get annual returns
#'
#' @usage get_annual_returns_all(path)
#' @param path A single character string. Folder where all data are stored.
#' @return get_annual_returns_all returns
#'
#' @export
get_annual_returns_all <- function(path){

  #### get annual returns for all tickers

  ## create folder if not exists and get folder name for quantity panel and tickers
  list.paths <- portfoliotracker::create_portfoliotracker_dir(path)
  path.pricequantitypanel <- list.paths$path.pricequantitypanel

  ## load price-quantity panel

  if (!rlang::is_empty(list.files(paste0(path.pricequantitypanel)))) {

    filenames <- paste0(path.pricequantitypanel, list.files(paste0(path.pricequantitypanel)))
    list.dfs <- lapply(filenames, data.table::fread)

    last.year <- lubridate::year(Sys.Date()) - 1
    df <- data.frame(year = last.year)

    for (i in 1:length(list.dfs)) {

      df.temp <- portfoliotracker::get_annual_returns(list.dfs[[i]])

      df <- merge(df, df.temp, by = "year", all.x = TRUE, all.y = TRUE)

    } ## end of for loop

    min.year <- min(df$year)
    max.year <- max(df$year)

    ## file name for annual returns
    filename.annual.returns <- paste0("annual_returns_all_from_", min.year, "_to_", max.year, ".csv")

    ## store as csv
    data.table::fwrite(df,paste0(path.returns, filename.annual.returns))

  } else {message("No price-quanity panel to calculate annual returns.")} ## end of if else statement

} ## end of function get_annual_returns_all


get_annual_returns <- function(df){

  #### get annual returns

  ticker <- unique(df$ticker)

  df$date <- as.Date(df$date, "%Y-%m-%d")

  if(!("date" %in% names(df))) rownames(df) <- df$date

  df <- df[,c("date", "adjusted")]
  df <- xts::as.xts(df)
  df <- quantmod::annualReturn(df)
  df <- as.data.frame(df)

  names(df) <- paste0(ticker, ".", names(df))

  df$date <- rownames(df)
  rownames(df) <- 1:nrow(df)

  df$date <- as.Date(df$date, "%Y-%m-%d")
  df$year <- as.numeric(format(df$date, "%Y"))

  df <- df[, names(df) != "date"]

  return(df)

} ## end of function get_annual_returns





## load tables with annual return
if (!rlang::is_empty(list.files(paste0(path.returns),
                               pattern = "^annual_returns_all_from_"))) {

  ## file name for annual returns
  filename.annual.returns <- list.files(paste0(path.returns),
                                        pattern = "^annual_returns_all_from_")

  ## choose annual returns with most recent year and minimum year
  filename.annual.returns <- filename.annual.returns[grepl(max(as.numeric(stringr::str_match(filename.annual.returns,
                                                                                             "to_(.*?).csv")[,2])), filename.annual.returns)]
  filename.annual.returns <- filename.annual.returns[grepl(min(as.numeric(stringr::str_match(filename.annual.returns,
                                                                                             "from_(.*?)_to_")[,2])), filename.annual.returns)]

  ## load annual returns
  df.annual.returns <- data.table::fread(paste0(path.returns, filename.annual.returns))

  ## clean table
  names(df.annual.returns)[names(df.annual.returns) == "year"] <- "Year"
  df.annual.returns <- df.annual.returns[rev(order(df.annual.returns$Year)), ]

  ## change column names
  names(df.annual.returns) <- gsub("\\.yearly\\.returns", "", names(df.annual.returns))

  ## add one missing year to get calculation of mean annual returns right
  df.annual.returns <- rbind(df.annual.returns,df.annual.returns[nrow(df.annual.returns) + 1, ])
  df.annual.returns[nrow(df.annual.returns), "Year"] <- min(df.annual.returns$Year, na.rm = TRUE) - 1

  ## compute mean annual returns for 1yr, 3yrs, 5yrs, 10yrs and MAX
  ## table with rows being investments and columns being 1Y,3Y, 5Y, 10Y, MAX annual returns

  df.annual.returns <- as.data.frame(df.annual.returns)

  ## multiply all columns with 100 to get returns in percent
  df.annual.returns[,names(df.annual.returns) != "Year"] <- df.annual.returns[,names(df.annual.returns) != "Year"] * 100


  df.mean.annual.returns <- data.frame(matrix(nrow = 0, ncol = 6))

  for (i in 2:length(df.annual.returns)) {

    ticker <- names(df.annual.returns)[i]
    annual.returns.1y <- mean(df.annual.returns[df.annual.returns$Year > lubridate::year(Sys.Date()) - 1, i])
    annual.returns.3y <- mean(df.annual.returns[df.annual.returns$Year > lubridate::year(Sys.Date()) - 3, i])
    annual.returns.5y <- mean(df.annual.returns[df.annual.returns$Year > lubridate::year(Sys.Date()) - 5, i])
    annual.returns.10y <- mean(df.annual.returns[df.annual.returns$Year > lubridate::year(Sys.Date()) - 10, i])
    annual.returns.max <- mean(df.annual.returns[, i])
    df.temp <- data.frame(ticker, annual.returns.1y, annual.returns.3y, annual.returns.5y, annual.returns.10y,
                          annual.returns.max)
    df.mean.annual.returns <- rbind(df.mean.annual.returns, df.temp)

  } ## end of for loop which creates mean annual returns

  ## add name and ISIN
  df.mean.annual.returns <- merge(df.mean.annual.returns, df.ticker.investmentnames, by = "ticker")
  df.mean.annual.returns <- merge(df.mean.annual.returns, df.isin.ticker.converter, by = "ticker")

  names(df.mean.annual.returns) <- c("Ticker", "1Y", "3Y", "5Y", "10Y", "Max", "Name", "ISIN")
  df.mean.annual.returns <- df.mean.annual.returns[, c("Name", "ISIN", "Ticker", "1Y", "3Y", "5Y", "10Y", "Max")]

  ## formatting numbers
  df.mean.annual.returns$`1Y` <- as.numeric(formatC(df.mean.annual.returns$`1Y`, digits = 2, format = "f"))
  df.mean.annual.returns$`3Y` <- as.numeric(formatC(df.mean.annual.returns$`3Y`, digits = 2, format = "f"))
  df.mean.annual.returns$`5Y` <- as.numeric(formatC(df.mean.annual.returns$`5Y`, digits = 2, format = "f"))
  df.mean.annual.returns$`10Y` <- as.numeric(formatC(df.mean.annual.returns$`10Y`, digits = 2, format = "f"))
  df.mean.annual.returns$Max <- as.numeric(formatC(df.mean.annual.returns$Max, digits = 2, format = "f"))

  ## formatting of annual returns
  if (length(df.annual.returns) > 3) {

    df.annual.returns[, names(df.annual.returns) != "Year"] <- apply(df.annual.returns[, names(df.annual.returns) != "Year"],
                                                                     2, formatC, digits = 2, format = "f")

    } else if (length(df.annual.returns) == 2) {

    df.annual.returns[, 2] <- formatC(df.annual.returns[, 2], digits = 2, format = "f")

  }

} ## end of if statement is empty



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


# df1.returns <- Return.calculate(df1.adj)
# df1.returns.value <- Return.calculate(df1.value)

# #Annualized Performance with Risk Free Rate 4.5%
# performance_table <- as.data.frame(table.AnnualizedReturns(df1.returns, Rf = 0.05/279.8))
# performance_table <- rownames_to_column(performance_table)
# names(performance_table)[1] <- 'Performance'

# #Tidying Annualized Performance Dataframe
# performance_df <- performance_table %>% gather(key = 'Code', value = 'Values', -Performance) %>% spread(key = Performance, value = Values) %>%
#   rename('Annualized_Return' = 'Annualized Return', 'Annualized_Sharpe' = 'Annualized Sharpe (Rf=4.5%)','Annualized_StdDev' = 'Annualized Std Dev' ) %>%
#   select(Code,Annualized_Return, Annualized_StdDev, Annualized_Sharpe)
