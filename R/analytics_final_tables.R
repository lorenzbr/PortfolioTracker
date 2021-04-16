# #### FUNCTIONS TO CREATE FINAL TABLES WITH PERFORMANCE AND SO ON
# ####
#
#
# ## functions to get final tables for performance, value over time, annual returns for each ticker, and all together
# ##
# ##
# ##
#
#
#
# ## load functions to get financials (to get function get.ticker.from.isin)
# source(paste0(path.src.data.financials,filename.src.get.data.financials))
#
#
#
#
# ## run function to get transaction history
# df.transaction.history <- get.transaction.history.table()
#
#
# ## get tickers from ISIN
# df.isin.ticker.converter <- get.ticker.from.isin()
#
# ## keep ISIN and name
# df.ticker.investmentnames <- unique(df.transaction.history[, c("isin", "name")])
#
# ## add tickers
# df.ticker.investmentnames <- merge(df.ticker.investmentnames,df.isin.ticker.converter, by = "isin")
# df.ticker.investmentnames <- unique(df.ticker.investmentnames[, c("ticker", "name")])
#
#
#
#
#
# ## load price quantity panels
# if (!rlang::is_empty(list.files(paste0(path.data.processed.analytics.pricequantitypanel)))) {
#
#   filenames <- paste0(path.data.processed.analytics.pricequantitypanel,
#                       list.files(paste0(path.data.processed.analytics.pricequantitypanel)))
#   list.dfs <- lapply(filenames, data.table::fread)
#
#   ## for loop over all price-quantity panel dfs in list. store them in separate dfs
#   for (i in 1:length(list.dfs)) {
#
#     assign(paste0("df.pricequantity.panel", i), list.dfs[[i]])
#     df.temp <- get(paste0("df.pricequantity.panel", i))
#     df.temp$date <- as.Date(df.temp$date, "%Y-%m-%d")
#     assign(paste0("df.pricequantity.panel", i), df.temp)
#
#   } ## end of for loop
#   rm(df.temp)
#
# } ## end of if statement price-quantity panel is empty


## compute performance in %
# df.pricequantity.panel.temp <- df.pricequantity.panel
# df.pricequantity.panel.temp <- df.pricequantity.panel.temp[order(df.pricequantity.panel.temp$date),]

#' Write current portfolio investments to a csv file
#'
#' @usage write_current_portfolio(path, file.name = "current_portfolio.csv", file.tickers = "isin_ticker.csv",
#'                       file.transactions = "transaction_fullhistory.csv")
#' @param path A single character string. Directory of your data.
#' @param file.name A single character string. Name of created csv file containing current portfolio.
#' @param file.tickers A single character string. Name of csv containing ISIN-ticker pairs.
#' @param file.transactions A single character string. Name of csv file containing transactions.
#'
#' @export
write_current_portfolio <- function(path, file.name = "current_portfolio.csv", file.tickers = "isin_ticker.csv",
                                  file.transactions = "transaction_fullhistory.csv"){

  ## create folder if not exists and get folder name for quantity panel and tickers
  list.paths <- PortfolioTracker::create_portfoliotracker_dir(path)
  path.pricequantitypanel <- list.paths$path.pricequantitypanel
  path.tickers <- list.paths$path.tickers
  path.transactions <- list.paths$path.transactions
  path.data <- list.paths$path.data

  ## load price quantity panels if exists
  if (!rlang::is_empty(list.files(path.pricequantitypanel))) {

    filenames <- paste0(path.pricequantitypanel, list.files(path.pricequantitypanel))
    list.dfs <- lapply(filenames, data.table::fread)

    ## get table that converts ISIN to ticker (which is needed by Yahoo Finance)
    df.isin.ticker <- data.table::fread(paste0(path.tickers, file.tickers))

    ## keep ISIN and name
    df.transaction.history <- data.table::fread(paste0(path.transactions, file.transactions))
    df.ticker.investmentnames <- unique(df.transaction.history[, c("isin", "name")])

    ## add tickers
    df.ticker.investmentnames <- merge(df.ticker.investmentnames, df.isin.ticker, by = "isin")
    df.ticker.investmentnames <- unique(df.ticker.investmentnames[, c("ticker", "name")])

    ## get most recent entry in each price-quantity panel
    df.all <- do.call(rbind, list.dfs)

    ## keep latest date for each ticker
    df.current <- stats::aggregate(date ~ ticker, data = df.all, max)

    ## add details
    df.current <- merge(df.all, df.current, by = c("ticker", "date"))

    ## keep investments with quantity greater zero
    df.current <- df.current[df.current$cum_quantity > 0, ]

    df.current <- unique(df.current)

    ## add name and ISIN
    df.current <- merge(df.current, df.ticker.investmentnames, by = "ticker")
    df.current <- merge(df.current, df.isin.ticker, by = "ticker")

    df.current <- df.current[, c("name", "isin", "ticker", "adjusted", "cum_quantity", "value")]

    ## current total portfolio value
    total.portfolio.value <- sum(df.current$value)

    ## compute weight of each investment in total portfolio value
    df.current$weight <- df.current$value / total.portfolio.value

    data.table::fwrite(df.current, paste0(path.data, file.name))

  } else { message("No price quantity panels available.") } ## end of if statement

} # end of function write_current_portfolio



# ##
# if(nrow(df.all) > 0){
#
#   df.investments <- df.all
#   df.investments$weight <- df.investments$weight * 100
#   df.investments$weight <- as.numeric(formatC(df.investments$weight, digits = 2, format = "f"))
#   df.investments$adjusted <- as.numeric(formatC(df.investments$adjusted, digits = 2, format = "f"))
#
#   # tickers <- unique(df.all$ticker)
#   # df.all[df.all$ticker == tickers[1]]
#
#   # } ## end of function get.current.portfolio.list
#
#   ## total price gains and until time t
#   price.gains.max <- total.portfolio.value - amount.invested.max
#   # ...
#
# } else {
#
#   total.portfolio.value <- 0
#   price.gains.max <- 0
#   names.investments <- names(df.all)
#   df.investments <- data.frame(matrix(nrow = 0, ncol = length(names.investments), dimnames = list(NULL, names.investments)))
#
# } ## end of if else statement df exists
#
#
# ## changes names of df.investments
# names(df.investments) <- c("Name", "ISIN", "Ticker", "Price [EUR]", "Quantity", "Value [EUR]", "Weight [%]")

